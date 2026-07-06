;;; taut-cache.el --- SQLite Persistent Cache for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us
;; Keywords: comm, slack

;;; Commentary:
;; This file implements a persistent SQLite cache for Taut. It stores
;; users, channels, messages, and watched thread states to make startup
;; instantaneous and reduce Slack Web API load.
;; It degrades gracefully to in-memory mode if SQLite is not available.

;;; Code:

(require 'sqlite)
(require 'json)
(require 'taut-model)

(defcustom taut-cache-enabled t
  "When non-nil, enable SQLite persistent caching if available."
  :type 'boolean
  :group 'taut)

(defcustom taut-cache-db-path (expand-file-name "taut-cache.db" user-emacs-directory)
  "The path to the SQLite database file used for Taut caching."
  :type 'file
  :group 'taut)

(defcustom taut-cache-keep-days 180
  "Number of days of message history to retain in the persistent cache."
  :type 'integer
  :group 'taut)

(defvar taut-cache--db nil
  "The active SQLite database connection.")

(defun taut-cache--available-p ()
  "Return non-nil if SQLite persistent cache is supported and enabled."
  (and taut-cache-enabled
       (fboundp 'sqlite-available-p)
       (sqlite-available-p)))

(defun taut-cache--get-db ()
  "Retrieve or initialize the SQLite database connection.
Creates the necessary tables if they do not exist."
  (when (taut-cache--available-p)
    (unless (and taut-cache--db (sqlitep taut-cache--db))
      (setq taut-cache--db (sqlite-open taut-cache-db-path))
      ;; Initialize database tables
      (sqlite-execute taut-cache--db
                      "CREATE TABLE IF NOT EXISTS users (
                         id TEXT PRIMARY KEY,
                         username TEXT,
                         real_name TEXT,
                         presence TEXT,
                         is_me INTEGER,
                         custom_status TEXT
                       )")
      (sqlite-execute taut-cache--db
                      "CREATE TABLE IF NOT EXISTS channels (
                         id TEXT PRIMARY KEY,
                         name TEXT,
                         type TEXT,
                         unread_count INTEGER,
                         mention_count INTEGER,
                         is_starred INTEGER,
                         topic TEXT,
                         purpose TEXT
                       )")
      (ignore-errors
        (sqlite-execute taut-cache--db "ALTER TABLE channels ADD COLUMN is_hidden INTEGER DEFAULT 0"))
      (sqlite-execute taut-cache--db
                      "CREATE TABLE IF NOT EXISTS messages (
                         id TEXT PRIMARY KEY,
                         channel_id TEXT,
                         user_id TEXT,
                         text TEXT,
                         ts TEXT,
                         thread_ts TEXT,
                         reply_count INTEGER,
                         reactions_json TEXT,
                         is_unread INTEGER,
                         is_mention INTEGER,
                         is_starred INTEGER
                       )")
      (sqlite-execute taut-cache--db
                      "CREATE TABLE IF NOT EXISTS watched_threads (
                         thread_ts TEXT PRIMARY KEY
                       )")
      ;; Create indexes to optimize queries
      (sqlite-execute taut-cache--db
                      "CREATE INDEX IF NOT EXISTS idx_messages_chan ON messages (channel_id)")
      (sqlite-execute taut-cache--db
                      "CREATE INDEX IF NOT EXISTS idx_messages_thread ON messages (thread_ts)"))
    taut-cache--db))

(defun taut-cache-save-user (user)
  "Save USER (a `taut-user' struct) to the SQLite database."
  (let ((db (taut-cache--get-db)))
    (when db
      (sqlite-execute db
                      "INSERT OR REPLACE INTO users (id, username, real_name, presence, is_me, custom_status)
                       VALUES (?, ?, ?, ?, ?, ?)"
                      (list (taut-user-id user)
                            (taut-user-username user)
                            (taut-user-real-name user)
                            (symbol-name (or (taut-user-presence user) 'offline))
                            (if (taut-user-is-me user) 1 0)
                            (taut-user-custom-status user))))))

(defun taut-cache-save-channel (chan)
  "Save CHAN (a `taut-channel' struct) to the SQLite database."
  (let ((db (taut-cache--get-db)))
    (when db
      (sqlite-execute db
                      "INSERT OR REPLACE INTO channels (id, name, type, unread_count, mention_count, is_starred, is_hidden, topic, purpose)
                       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
                      (list (taut-channel-id chan)
                            (taut-channel-name chan)
                            (symbol-name (or (taut-channel-type chan) 'public))
                            (or (taut-channel-unread-count chan) 0)
                            (or (taut-channel-mention-count chan) 0)
                            (if (taut-channel-is-starred chan) 1 0)
                            (if (taut-channel-is-hidden chan) 1 0)
                            (taut-channel-topic chan)
                            (taut-channel-purpose chan))))))

(defun taut-cache-save-message (msg)
  "Save MSG (a `taut-message' struct) to the SQLite database."
  (let ((db (taut-cache--get-db)))
    (when db
      (let* ((reactions (taut-message-reactions msg))
             (reactions-json (if reactions (json-encode reactions) "")))
        (sqlite-execute db
                        "INSERT OR REPLACE INTO messages (id, channel_id, user_id, text, ts, thread_ts, reply_count, reactions_json, is_unread, is_mention, is_starred)
                         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                        (list (taut-message-id msg)
                              (taut-message-channel-id msg)
                              (taut-message-user-id msg)
                              (taut-message-text msg)
                              (taut-message-ts msg)
                              (taut-message-thread-ts msg)
                              (or (taut-message-reply-count msg) 0)
                              reactions-json
                              (if (taut-message-is-unread msg) 1 0)
                              (if (taut-message-is-mention msg) 1 0)
                              (if (taut-message-is-starred msg) 1 0)))))))

(defun taut-cache-delete-message (ts)
  "Delete message identified by TS from the SQLite database."
  (let ((db (taut-cache--get-db)))
    (when db
      (sqlite-execute db "DELETE FROM messages WHERE ts = ?" (list ts)))))

(defun taut-cache-save-watched-thread (thread-ts)
  "Save THREAD-TS to the SQLite watched threads database."
  (let ((db (taut-cache--get-db)))
    (when db
      (sqlite-execute db "INSERT OR REPLACE INTO watched_threads (thread_ts) VALUES (?)" (list thread-ts)))))

(defun taut-cache-load-all ()
  "Load all cached data from SQLite into the in-memory `taut-model' tables."
  (interactive)
  (let ((db (taut-cache--get-db)))
    (when db
      (message "Taut: Loading cached workspace state from SQLite...")
      (taut-model-clear-all)
      
      ;; 1. Load users
      (let ((users (sqlite-select db "SELECT id, username, real_name, presence, is_me, custom_status FROM users")))
        (dolist (row users)
          (let* ((id (nth 0 row))
                 (username (nth 1 row))
                 (real-name (nth 2 row))
                 (presence-str (nth 3 row))
                 (is-me (nth 4 row))
                 (custom-status (nth 5 row))
                 (user (make-taut-user
                        :id id
                        :username username
                        :real-name real-name
                        :presence (taut-model-normalize-presence presence-str)
                        :is-me (= is-me 1)
                        :custom-status custom-status)))
            (setf (gethash id taut-users) user)
            (when (= is-me 1)
              (setq taut-current-user-id id)))))

      ;; 2. Load channels
      (let ((chans (sqlite-select db "SELECT id, name, type, unread_count, mention_count, is_starred, topic, purpose, is_hidden FROM channels")))
        (dolist (row chans)
          (let* ((id (nth 0 row))
                 (name (nth 1 row))
                 (type-str (nth 2 row))
                 (unread-count (nth 3 row))
                 (mention-count (nth 4 row))
                 (is-starred (nth 5 row))
                 (topic (nth 6 row))
                 (purpose (nth 7 row))
                 (is-hidden (or (nth 8 row) 0))
                 (chan (make-taut-channel
                        :id id
                        :name name
                        :type (intern type-str)
                        :unread-count unread-count
                        :mention-count mention-count
                        :is-starred (= is-starred 1)
                        :is-hidden (= is-hidden 1)
                        :topic topic
                        :purpose purpose)))
            (setf (gethash id taut-channels) chan))))

      ;; 3. Load watched threads
      (let ((threads (sqlite-select db "SELECT thread_ts FROM watched_threads")))
        (dolist (row threads)
          (push (car row) taut-watched-threads)))

      ;; 4. Load messages
      (let ((msgs (sqlite-select db "SELECT id, channel_id, user_id, text, ts, thread_ts, reply_count, reactions_json, is_unread, is_mention, is_starred FROM messages")))
        (dolist (row msgs)
          (let* ((id (nth 0 row))
                 (channel-id (nth 1 row))
                 (user-id (nth 2 row))
                 (text (nth 3 row))
                 (ts (nth 4 row))
                 (thread-ts (nth 5 row))
                 (reply-count (nth 6 row))
                 (reactions-json (nth 7 row))
                 (is-unread (nth 8 row))
                 (is-mention (nth 9 row))
                 (is-starred (nth 10 row))
                 (reactions (unless (string= reactions-json "")
                              (ignore-errors
                                (let ((json-array-type 'list)
                                      (json-object-type 'alist)
                                      (json-key-type 'symbol))
                                  (json-read-from-string reactions-json)))))
                 (msg (make-taut-message
                       :id id
                       :channel-id channel-id
                       :user-id user-id
                       :text text
                       :ts ts
                       :thread-ts thread-ts
                       :reply-count reply-count
                       :reactions reactions
                       :is-unread (= is-unread 1)
                       :is-mention (= is-mention 1)
                       :is-starred (= is-starred 1))))
            ;; Insert message directly into memory maps based on whether it is a thread reply
            (if (and thread-ts (not (equal thread-ts ts)))
                (let ((replies (gethash thread-ts taut-threads)))
                  (setf (gethash thread-ts taut-threads) (append replies (list msg))))
              (let ((chan-msgs (gethash channel-id taut-messages)))
                (setf (gethash channel-id taut-messages) (append chan-msgs (list msg))))))))

      (taut-model-trigger-update)
      (message "Taut: Loaded state from SQLite cache successfully!")
      (ignore-errors (taut-cache-prune)))))

(defun taut-cache-prune (&optional days)
  "Prune messages older than DAYS (default `taut-cache-keep-days') from cache."
  (interactive)
  (let ((db (taut-cache--get-db))
        (days (or days taut-cache-keep-days)))
    (when db
      (let* ((seconds-limit (* days 24 60 60))
             (cutoff-time (- (time-convert nil 'integer) seconds-limit))
             (cutoff-ts (format "%d.000000" cutoff-time)))
        (sqlite-execute db
                        "DELETE FROM messages 
                         WHERE ts < ? 
                           AND is_starred = 0 
                           AND is_mention = 0"
                        (list cutoff-ts))
        (message "Taut: Pruned cached messages older than %d days." days)))))

(defun taut-cache-clear ()
  "Clear all persistent cache tables."
  (interactive)
  (let ((db (taut-cache--get-db)))
    (when db
      (sqlite-execute db "DELETE FROM users")
      (sqlite-execute db "DELETE FROM channels")
      (sqlite-execute db "DELETE FROM messages")
      (sqlite-execute db "DELETE FROM watched_threads")
      (message "Taut: Cache cleared successfully."))))

(defun taut-cache-search-messages (query &optional channel-id user-id)
  "Search local SQLite messages matching QUERY.
QUERY is a search term which will be matched using SQL LIKE syntax (e.g. %term%).
If CHANNEL-ID is specified, limit results to that channel.
If USER-ID is specified, limit results to messages from that user."
  (let ((db (taut-cache--get-db)))
    (when db
      (let* ((like-query (concat "%" query "%"))
             (sql "SELECT id, channel_id, user_id, text, ts, thread_ts, reply_count, reactions_json, is_unread, is_mention, is_starred
                   FROM messages
                   WHERE text LIKE ?")
             (params (list like-query)))
        (when channel-id
          (setq sql (concat sql " AND channel_id = ?"))
          (push channel-id params))
        (when user-id
          (setq sql (concat sql " AND user_id = ?"))
          (push user-id params))
        (setq sql (concat sql " ORDER BY ts DESC"))
        (let ((rows (sqlite-select db sql (nreverse params)))
              results)
          (dolist (row rows)
            (let* ((id (nth 0 row))
                   (channel-id (nth 1 row))
                   (user-id (nth 2 row))
                   (text (nth 3 row))
                   (ts (nth 4 row))
                   (thread-ts (nth 5 row))
                   (reply-count (nth 6 row))
                   (reactions-json (nth 7 row))
                   (is-unread (nth 8 row))
                   (is-mention (nth 9 row))
                   (is-starred (nth 10 row))
                   (reactions (unless (or (null reactions-json) (string= reactions-json ""))
                                (ignore-errors
                                  (let ((json-array-type 'list)
                                        (json-object-type 'alist)
                                        (json-key-type 'symbol))
                                    (json-read-from-string reactions-json)))))
                   (msg (make-taut-message
                         :id id
                         :channel-id channel-id
                         :user-id user-id
                         :text text
                         :ts ts
                         :thread-ts thread-ts
                         :reply-count reply-count
                         :reactions reactions
                         :is-unread (= is-unread 1)
                         :is-mention (= is-mention 1)
                         :is-starred (= is-starred 1))))
              (push msg results)))
          (nreverse results))))))

(provide 'taut-cache)
;;; taut-cache.el ends here

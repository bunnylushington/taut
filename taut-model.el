;;; taut-model.el --- State and Data Models for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Google DeepMind

;; Author: Antigravity
;; Keywords: comm, slack

;;; Commentary:
;; This file defines the core data structures and in-memory state storage
;; for the Taut Slack client. It manages channels, users, messages, and
;; thread states, providing high-level queries and mutators that trigger
;; UI updates.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Customization and Variables

(defgroup taut nil
  "An elegant Slack client for Emacs."
  :group 'applications)

(defgroup taut-faces nil
  "Faces used by the Taut Slack client."
  :group 'taut)

;;;; Data Structures

(cl-defstruct taut-user
  "Represents a Slack user."
  id
  username
  real-name
  presence     ; 'online, 'away, 'offline
  is-me        ; t or nil
  custom-status)

(cl-defstruct taut-channel
  "Represents a Slack channel, group, or DM."
  id
  name         ; "general", "alice" (for DM)
  type         ; 'public, 'private, 'dm, 'group
  (unread-count 0)
  (mention-count 0)
  is-starred
  topic
  purpose)

(cl-defstruct taut-message
  "Represents a single Slack message."
  id
  channel-id
  user-id
  text
  ts            ; Slack timestamp string, e.g., "1688474251.0001"
  thread-ts     ; Set if this is a thread reply or root of a thread
  (reply-count 0) ; Number of replies if this is a thread root
  reactions     ; Alist of (emoji-name . list-of-user-ids)
  is-unread     ; t or nil
  is-mention    ; t or nil if current user was mentioned
  is-starred)   ; t or nil if starred/bookmarked

(cl-defstruct taut-inbox-item
  "Represents an item in the Gnus-style unified Inbox feed."
  id            ; Unique item ID (often same as message ts or thread ts)
  type          ; 'dm, 'mention, 'thread-update
  channel-id
  message-id
  thread-ts     ; If thread-update, the thread root timestamp
  user-id
  title         ; e.g., "#development", "DM: @alice", "Thread: #ideas"
  snippet       ; Message excerpt
  ts            ; Timestamp of the update
  is-read)      ; t or nil

;;;; In-Memory Databases

(defvar taut-current-user-id "U_ME"
  "The Slack user ID of the current logged-in user.")

(defvar taut-users (make-hash-table :test 'equal)
  "Hash table mapping user-id (string) to `taut-user` struct.")

(defvar taut-channels (make-hash-table :test 'equal)
  "Hash table mapping channel-id (string) to `taut-channel` struct.")

(defvar taut-messages (make-hash-table :test 'equal)
  "Hash table mapping channel-id to lists of `taut-message` structs.")

(defvar taut-threads (make-hash-table :test 'equal)
  "Hash table mapping thread-ts to lists of reply `taut-message` structs.")

(defvar taut-watched-threads nil
  "List of thread-ts strings that the user is watching/participated in.")

(defvar taut-typing-users (make-hash-table :test 'equal)
  "Hash table mapping target key (channel-id or thread-ts) to user-ids.")

(defvar taut-typing-timers (make-hash-table :test 'equal)
  "Hash table mapping (target-key . user-id) to expiration timers.")

;;;; Hook Definitions

(defvar taut-model-updated-hook nil
  "Hook run after any significant state change in the data model.
Functions on this hook can redraw buffers like the sidebar or inbox.")

;;;; Query Helper Functions

(defun taut-model-get-user (user-id)
  "Retrieve the `taut-user` for USER-ID. Returns a fallback if not found."
  (or (gethash user-id taut-users)
      (make-taut-user :id user-id :username (format "user-%s" user-id) :real-name "Unknown User" :presence 'offline)))

(defun taut-model-get-user-by-username (username)
  "Find a `taut-user` by USERNAME. Returns nil if not found."
  (let (found)
    (maphash (lambda (_id user)
               (when (equal (taut-user-username user) username)
                 (setq found user)))
             taut-users)
    found))

(defun taut-model-get-channel (channel-id)
  "Retrieve the `taut-channel` for CHANNEL-ID."
  (gethash channel-id taut-channels))

(defun taut-model-get-messages (channel-id)
  "Get all messages for CHANNEL-ID, sorted by timestamp ascending."
  (gethash channel-id taut-messages))

(defun taut-model-get-thread-replies (thread-ts)
  "Get all replies for the thread identified by THREAD-TS."
  (gethash thread-ts taut-threads))

(defun taut-model-get-message-by-ts (ts)
  "Find a `taut-message' struct with timestamp TS by searching local storage."
  (let (found)
    ;; Search channel messages
    (maphash (lambda (_chan-id msgs)
               (unless found
                 (setq found (cl-find ts msgs :key #'taut-message-ts :test #'equal))))
             taut-messages)
    ;; If not found, search thread replies
    (unless found
      (maphash (lambda (_thread-ts replies)
                 (unless found
                   (setq found (cl-find ts replies :key #'taut-message-ts :test #'equal))))
               taut-threads))
    found))

(defun taut-model-get-channels-list ()
  "Return a list of all channels, ordered by type and name."
  (let (channels)
    (maphash (lambda (_k v) (push v channels)) taut-channels)
    (sort channels
          (lambda (a b)
            (let ((star-a (taut-channel-is-starred a))
                  (star-b (taut-channel-is-starred b)))
              (if (not (eq star-a star-b))
                  star-a
                (string< (or (taut-channel-name a) "") (or (taut-channel-name b) ""))))))))

;;;; Derive Gnus-Style Inbox Items

(defun taut-model-get-inbox-items ()
  "Query and construct a list of active `taut-inbox-item` objects.
Unified Inbox contains:
1. Unread direct messages (DMs).
2. Unread channel mentions.
3. Thread updates for threads the user is watching, where new replies exist
   after the user's last-read timestamp of that thread."
  (let (items)
    ;; 1 & 2: DMs and Channel Mentions
    (maphash
     (lambda (chan-id chan)
       (let ((msgs (gethash chan-id taut-messages)))
         (dolist (msg msgs)
           (when (taut-message-is-unread msg)
             ;; If it's a DM, any unread message goes to inbox (unless sent by me)
             (cond
              ((and (eq (taut-channel-type chan) 'dm)
                    (not (equal (taut-message-user-id msg) taut-current-user-id)))
               (push (make-taut-inbox-item
                      :id (taut-message-ts msg)
                      :type 'dm
                      :channel-id chan-id
                      :message-id (taut-message-id msg)
                      :user-id (taut-message-user-id msg)
                      :title (format "DM: @%s" (or (taut-channel-name chan) "unknown"))
                      :snippet (taut-message-text msg)
                      :ts (taut-message-ts msg)
                      :is-read nil)
                     items))
              ;; If it's a channel mention (and not sent by me)
              ((and (taut-message-is-mention msg)
                    (not (equal (taut-message-user-id msg) taut-current-user-id)))
               (push (make-taut-inbox-item
                      :id (taut-message-ts msg)
                      :type 'mention
                      :channel-id chan-id
                      :message-id (taut-message-id msg)
                      :user-id (taut-message-user-id msg)
                      :title (format "#%s" (or (taut-channel-name chan) "unknown"))
                      :snippet (taut-message-text msg)
                      :ts (taut-message-ts msg)
                      :is-read nil)
                     items)))))))
     taut-channels)

    ;; 3: Thread updates
    (dolist (th-ts taut-watched-threads)
      (let* ((replies (gethash th-ts taut-threads))
             (last-reply (car (last replies))))
        (when (and last-reply
                   (not (equal (taut-message-user-id last-reply) taut-current-user-id))
                   (taut-message-is-unread last-reply))
          ;; Find root message to get channel info and snippet
          (let* ((chan-id (taut-message-channel-id last-reply))
                 (chan (taut-model-get-channel chan-id))
                 (chan-name (if chan (or (taut-channel-name chan) "unknown") "unknown"))
                 (is-dm (and chan (eq (taut-channel-type chan) 'dm))))
            (push (make-taut-inbox-item
                   :id th-ts
                   :type 'thread-update
                   :channel-id chan-id
                   :message-id (taut-message-id last-reply)
                   :thread-ts th-ts
                   :user-id (taut-message-user-id last-reply)
                   :title (if is-dm (format "Thread in DM: @%s" chan-name) (format "Thread: #%s" chan-name))
                   :snippet (format "Reply: %s" (taut-message-text last-reply))
                   :ts (taut-message-ts last-reply)
                   :is-read nil)
                  items)))))

    ;; Sort items descending by timestamp so most recent is on top
    (sort items (lambda (a b) (string> (or (taut-inbox-item-ts a) "") (or (taut-inbox-item-ts b) ""))))))

;;;; Mutation & Operations Layer

(defvar taut-model--update-timer nil
  "Timer used to debounce model update hook execution.")

(defun taut-model-trigger-update ()
  "Schedule `taut-model-updated-hook` to run safely in the main event loop.
Debounces multiple rapid model changes."
  (unless taut-model--update-timer
    (setq taut-model--update-timer
          (run-at-time 0.01 nil
                       (lambda ()
                         (setq taut-model--update-timer nil)
                         (run-hooks 'taut-model-updated-hook))))))

(defun taut-model-set-user-typing (channel-id user-id &optional thread-ts)
  "Mark USER-ID as typing in CHANNEL-ID or THREAD-TS.
Starts an expiration timer to automatically clear the indicator."
  (let* ((target-key (or thread-ts channel-id))
         (timer-key (cons target-key user-id)))
    ;; Cancel existing timer if any
    (let ((old-timer (gethash timer-key taut-typing-timers)))
      (when old-timer
        (cancel-timer old-timer)))
    
    ;; Add to typing users
    (let ((users (gethash target-key taut-typing-users)))
      (unless (member user-id users)
        (puthash target-key (cons user-id users) taut-typing-users)))
    
    ;; Schedule clearing timer
    (let ((timer (run-with-timer 6.0 nil
                                 (lambda ()
                                   (taut-model-clear-user-typing channel-id user-id thread-ts)))))
      (puthash timer-key timer taut-typing-timers))
    (taut-model-trigger-update)))

(defun taut-model-clear-user-typing (channel-id user-id &optional thread-ts)
  "Clear USER-ID from typing list in CHANNEL-ID or THREAD-TS."
  (let* ((target-key (or thread-ts channel-id))
         (timer-key (cons target-key user-id)))
    (let ((timer (gethash timer-key taut-typing-timers)))
      (when timer
        (cancel-timer timer)
        (remhash timer-key taut-typing-timers)))
    (let ((users (gethash target-key taut-typing-users)))
      (when (member user-id users)
        (let ((new-users (delete user-id users)))
          (if new-users
              (puthash target-key new-users taut-typing-users)
            (remhash target-key taut-typing-users)))
        (taut-model-trigger-update)))))

(defun taut-model-add-user (user)
  "Register USER in the global database."
  (setf (gethash (taut-user-id user) taut-users) user)
  (taut-model-trigger-update))

(defun taut-model-add-channel (chan)
  "Register channel CHAN in the global database."
  (setf (gethash (taut-channel-id chan) taut-channels) chan)
  (taut-model-trigger-update))

(defun taut-model-delete-message (ts)
  "Remove message with timestamp TS from storage (both channels and threads)."
  (let ((found nil))
    (maphash (lambda (chan-id msgs)
               (let ((new-msgs (cl-remove-if (lambda (m) (equal (taut-message-ts m) ts)) msgs)))
                 (unless (= (length msgs) (length new-msgs))
                   (setq found t)
                   (puthash chan-id new-msgs taut-messages))))
             taut-messages)
    (maphash (lambda (thread-ts replies)
               (let ((new-replies (cl-remove-if (lambda (m) (equal (taut-message-ts m) ts)) replies)))
                 (unless (= (length replies) (length new-replies))
                   (setq found t)
                   (puthash thread-ts new-replies taut-threads))))
             taut-threads)
    (when found
      (taut-model-trigger-update))
    found))

(defun taut-model-add-message (msg &optional no-inc-reply-p)
  "Insert message MSG into storage, managing unreads and notifications.
Avoid inserting duplicate messages based on timestamp TS.
If NO-INC-REPLY-P is non-nil, do not increment the root message's reply count."
  (let* ((chan-id (taut-message-channel-id msg))
         (chan (taut-model-get-channel chan-id))
         (thread-ts (taut-message-thread-ts msg))
         (msg-ts (taut-message-ts msg))
         (is-duplicate nil))

    ;; Check if it's a thread reply or main channel message
    (if (and thread-ts (not (equal thread-ts msg-ts)))
        ;; Thread reply
        (let ((replies (gethash thread-ts taut-threads)))
          (if (cl-some (lambda (m) (equal (taut-message-ts m) msg-ts)) replies)
              (setq is-duplicate t)
            (setf (gethash thread-ts taut-threads) (append replies (list msg)))
            ;; Increment root reply-count if root exists
            (unless no-inc-reply-p
              (let* ((root-chan-msgs (gethash chan-id taut-messages))
                     (root-msg (cl-find thread-ts root-chan-msgs :key #'taut-message-ts :test #'equal)))
                (when root-msg
                  (unless (taut-message-reply-count root-msg)
                    (setf (taut-message-reply-count root-msg) 0))
                  (cl-incf (taut-message-reply-count root-msg)))))
            ;; If I sent a message in this thread, or if it's my thread, watch it
            (let ((is-my-msg (equal (taut-message-user-id msg) taut-current-user-id)))
              (when (and is-my-msg (not (member thread-ts taut-watched-threads)))
                (push thread-ts taut-watched-threads)))))
      
      ;; Main channel message
      (let ((msgs (gethash chan-id taut-messages)))
        (if (cl-some (lambda (m) (equal (taut-message-ts m) msg-ts)) msgs)
            (setq is-duplicate t)
          (setf (gethash chan-id taut-messages) (append msgs (list msg))))))

    ;; Update channel unread/mention statistics (only if not a duplicate)
    (unless is-duplicate
      (when (and chan (not (equal (taut-message-user-id msg) taut-current-user-id)))
        (when (taut-message-is-unread msg)
          (unless (taut-channel-unread-count chan)
            (setf (taut-channel-unread-count chan) 0))
          (cl-incf (taut-channel-unread-count chan))
          (when (taut-message-is-mention msg)
            (unless (taut-channel-mention-count chan)
              (setf (taut-channel-mention-count chan) 0))
            (cl-incf (taut-channel-mention-count chan)))))

      (taut-model-trigger-update))))

(defun taut-model-mark-channel-read (channel-id)
  "Mark all messages in channel CHANNEL-ID as read."
  (let ((chan (taut-model-get-channel channel-id))
        (msgs (gethash channel-id taut-messages)))
    (when chan
      (setf (taut-channel-unread-count chan) 0)
      (setf (taut-channel-mention-count chan) 0))
    (dolist (msg msgs)
      (setf (taut-message-is-unread msg) nil))
    (taut-model-trigger-update)))

(defun taut-model-mark-thread-read (thread-ts)
  "Mark all replies in thread THREAD-TS as read."
  (let ((replies (gethash thread-ts taut-threads)))
    (dolist (msg replies)
      (setf (taut-message-is-unread msg) nil))
    (taut-model-trigger-update)))

(defun taut-model-get-starred-messages ()
  "Get a list of all locally cached `taut-message' structs that are starred."
  (let (starred)
    ;; Search channel messages
    (maphash (lambda (_chan-id msgs)
               (dolist (msg msgs)
                 (when (taut-message-is-starred msg)
                   (push msg starred))))
             taut-messages)
    ;; Search thread replies
    (maphash (lambda (_thread-ts replies)
               (dolist (msg replies)
                 (when (taut-message-is-starred msg)
                   (push msg starred))))
             taut-threads)
    ;; Sort by timestamp descending
    (sort starred (lambda (a b) (string> (or (taut-message-ts a) "") (or (taut-message-ts b) ""))))))

(defun taut-model-clear-all ()
  "Reset all local databases (primarily for tests/re-initialization)."
  (clrhash taut-users)
  (clrhash taut-channels)
  (clrhash taut-messages)
  (clrhash taut-threads)
  (setq taut-watched-threads nil)
  (taut-model-trigger-update))

(provide 'taut-model)
;;; taut-model.el ends here

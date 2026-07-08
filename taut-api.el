;;; taut-api.el --- Slack Web API Client for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us
;; Keywords: comm, slack

;;; Commentary:
;; This file implements the Slack REST Web API client for Taut. It handles
;; authorization, channel fetching, user translation, posting messages,
;; and adding reactions using Emacs's built-in url libraries and json parser.

;;; Code:

(require 'json)
(require 'url)
(require 'taut-model)
(require 'auth-source)

;;;; User Tokens and Configuration

(defcustom taut-bot-token nil
  "The Slack OAuth Token starting with `xoxp-' (User) or `xoxb-' (Bot).
To operate as yourself (showing your personal stars, channels, and DMs),
configure this with a User Token starting with `xoxp-'."
  :type 'string
  :group 'taut)

(defcustom taut-app-token nil
  "The Slack App-Level Token starting with `xapp-' for Socket Mode."
  :type 'string
  :group 'taut)

(defcustom taut-only-show-subscribed-channels t
  "When non-nil, only fetch and display channels you have joined/subscribed to."
  :type 'boolean
  :group 'taut)

(defun taut-api-load-tokens-from-authinfo ()
  "Load Slack tokens from auth-source (e.g., ~/.authinfo)."
  (interactive)
  (unless taut-bot-token
    (let ((match (car (auth-source-search :host "api.slack.com" :user "bot" :require '(:secret)))))
      (when match
        (let ((secret (plist-get match :secret)))
          (setq taut-bot-token (if (functionp secret) (funcall secret) secret))
          (message "Taut: Loaded taut-bot-token from auth-source.")))))
  (unless taut-app-token
    (let ((match (car (auth-source-search :host "api.slack.com" :user "app" :require '(:secret)))))
      (when match
        (let ((secret (plist-get match :secret)))
          (setq taut-app-token (if (functionp secret) (funcall secret) secret))
          (message "Taut: Loaded taut-app-token from auth-source."))))))

(defvar taut-api-base-url "https://slack.com/api/"
  "The base URL for Slack's Web API.")

;;;;;;; Utility HTTP Request Handlers

(defun taut-api--url-encode-params (params)
  "Convert PARAMS (alist of (key . val)) to a URL-encoded query string."
  (mapconcat (lambda (pair)
               (concat (url-hexify-string (symbol-name (car pair)))
                       "="
                       (url-hexify-string (format "%s" (cdr pair)))))
             params
             "&"))

(defun taut-api--request (endpoint &optional params method AppToken)
  "Make a request to Slack Web API ENDPOINT.
PARAMS is an alist of payload parameters.
METHOD can be GET or POST (defaults to POST if params present).
If APPTOKEN is non-nil, use the App Token starting with xapp-."
  (let* ((method (or method (if params "POST" "GET")))
         (url (concat taut-api-base-url endpoint))
         (token (if AppToken taut-app-token taut-bot-token)))
    
    (unless token
      (error "Taut: Token not configured. Please set `taut-bot-token' / `taut-app-token'"))

    (when (and params (string= method "GET"))
      (setq url (concat url "?" (taut-api--url-encode-params params))))

    (let ((curl-bin (executable-find "curl")))
      (if (null curl-bin)
          (error "Taut: `curl' executable not found on system. Please install curl.")
        (with-temp-buffer
          (let* ((args (list "-s" "-X" method
                             "-H" (concat "Authorization: Bearer " token)
                             "-H" "Content-Type: application/json; charset=utf-8")))
            (when (and params (not (string= method "GET")))
              (let ((json-encoding-pretty-print nil))
                (setq args (append args (list "-d" (json-encode params))))))
            (setq args (append args (list url)))
            ;; Execute curl synchronously to fetch the JSON response
            (apply #'call-process curl-bin nil t nil args)
            (goto-char (point-min))
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (json-false nil)
                   (json-null nil)
                   (res (condition-case nil
                             (json-read)
                           (error nil))))
              (if (null res)
                  (error "Taut: Failed to parse Slack JSON response")
                (if (cdr (assoc 'ok res))
                    res
                  (error "Slack API Error (%s): %s" endpoint (or (cdr (assoc 'error res)) "unknown error")))))))))))

;;;; High-level API Integration Wrappers

(defun taut-api-test-auth ()
  "Test bot credentials. Returns active user-id."
  (let* ((res (taut-api--request "auth.test" nil "POST"))
         (bot-id (cdr (assoc 'user_id res)))
         (team-id (cdr (assoc 'team_id res))))
    (setq taut-current-user-id bot-id)
    (when team-id
      (setq taut-team-id team-id))
    bot-id))

(defun taut-api-fetch-users ()
  "Fetch all users in workspace and populate `taut-users'."
  (interactive)
  (message "Taut: Syncing users list...")
  (let* ((res (taut-api--request "users.list" nil "GET"))
         (members (cdr (assoc 'members res))))
    (dolist (m members)
      (let* ((id (cdr (assoc 'id m)))
             (profile (cdr (assoc 'profile m)))
             (deleted (taut-api--bool (cdr (assoc 'deleted m)))))
        (unless deleted
          (taut-model-add-user
           (make-taut-user
            :id id
            :username (cdr (assoc 'name m))
            :real-name (or (cdr (assoc 'real_name profile)) (cdr (assoc 'real_name m)) (cdr (assoc 'name m)))
            :presence (if (equal (cdr (assoc 'presence m)) "away") 'away 'offline)
            :is-me (equal id taut-current-user-id))))))
  (message "Taut: Synced %d workspace users." (hash-table-count taut-users))))

(defun taut-api-fetch-active-presences ()
  "Fetch presence status for all users involved in active DM conversations."
  (interactive)
  (let (user-ids)
    (maphash (lambda (_id chan)
               (when (and (eq (taut-channel-type chan) 'dm)
                          (or (> (or (taut-channel-unread-count chan) 0) 0)
                              (> (or (taut-channel-mention-count chan) 0) 0)
                              (and (fboundp 'taut-model-channel-active-last-30-days-p)
                                   (taut-model-channel-active-last-30-days-p (taut-channel-id chan)))))
                 (let ((user (taut-model-get-user-by-username (taut-channel-name chan))))
                   (when user
                     (push (taut-user-id user) user-ids)))))
             taut-channels)
    (when user-ids
      (message "Taut: Syncing presence for %d active direct messages..." (length user-ids))
      (dolist (uid user-ids)
        (ignore-errors
          (let* ((res (taut-api--request "users.getPresence" `((user . ,uid)) "GET"))
                 (presence-str (cdr (assoc 'presence res))))
            (when presence-str
              (let ((user (taut-model-get-user uid)))
                (when user
                  (setf (taut-user-presence user) (taut-model-normalize-presence presence-str))
                  (when (fboundp 'taut-cache-save-user)
                    (taut-cache-save-user user))))))))
      (message "Taut: Finished syncing active presences."))))

(defun taut-api--bool (val)
  "Convert a JSON-parsed boolean VAL to a standard Lisp boolean.
Parsed JSON booleans are t or :json-false."
  (and val (not (eq val :json-false))))

(defun taut-api--extract-attachments-text (msg-data)
  "Extract readable text from attachments in MSG-DATA."
  (let* ((attachments (cdr (assoc 'attachments msg-data)))
         (texts nil))
    (dolist (att attachments)
      (let ((title (cdr (assoc 'title att)))
            (text (cdr (assoc 'text att)))
            (fallback (cdr (assoc 'fallback att)))
            (pretext (cdr (assoc 'pretext att))))
        (when pretext (push pretext texts))
        (when title (push title texts))
        (when text (push text texts))
        ;; Only use fallback if we didn't get any other fields
        (when (and fallback (not (or text title pretext)))
          (push fallback texts))))
    (if texts
        (mapconcat #'identity (nreverse texts) "\n")
      "")))

(defun taut-api--get-message-text (msg-data raw-text)
  "Get combined message text from RAW-TEXT and attachments."
  (let* ((attachment-text (taut-api--extract-attachments-text msg-data))
         (clean-raw (or raw-text ""))
         (subtype (cdr (assoc 'subtype msg-data)))
         (room (cdr (assoc 'room msg-data)))
         (call (cdr (assoc 'call msg-data)))
         (huddle (cdr (assoc 'huddle msg-data)))
         (text-to-use
          (if (string-blank-p clean-raw)
              (cond
               ((or (equal subtype "huddle_thread") room huddle)
                (let* ((room-name (and room (cdr (assoc 'name room))))
                       (has-ended (and room (not (eq (cdr (assoc 'has_ended room)) :json-false)))))
                  (format "📞 Slack Huddle%s%s"
                          (if room-name (format ": %s" room-name) "")
                          (if has-ended " (Ended)" " in progress"))))
               ((or (equal subtype "call_id") call)
                "📞 Slack Call")
               (t attachment-text))
            (if (string-blank-p attachment-text)
                clean-raw
              (concat clean-raw "\n" attachment-text)))))
    text-to-use))

(defun taut-api--clean-mpim-name (raw-name)
  "Clean up MPIM raw name (mpdm-a--b-1 -> a, b)."
  (if (and raw-name (string-prefix-p "mpdm-" raw-name))
      (let* ((name (substring raw-name 5)) ; strip mpdm-
             (name (replace-regexp-in-string "-[0-9]+$" "" name)) ; strip trailing -1
             (name (replace-regexp-in-string "--" ", " name))) ; replace -- with ,
        name)
    raw-name))

(defun taut-api-fetch-channels ()
  "Fetch channels (public, private, DM) and populate `taut-channels'."
  (interactive)
  (message "Taut: Syncing conversations list...")
  (let* ((types "public_channel,private_channel,im,mpim")
         (params `((types . ,types)
                   (limit . 1000)))
         (res (condition-case err
                  (taut-api--request "users.conversations" params "GET")
                (error
                 (let ((err-msg (error-message-string err)))
                   (cond
                    ;; If missing_scope, retry without private_channel
                    ((string-match-p "missing_scope" err-msg)
                     (message "Taut: private_channel scope missing; retrying...")
                     (taut-api--request "users.conversations"
                                        '((types . "public_channel,im,mpim")
                                          (limit . 1000))
                                        "GET"))
                    ;; Fallback to conversations.list if users.conversations fails
                    (t
                     (message "Taut: users.conversations failed (%s); trying conversations.list..." err-msg)
                     (condition-case err2
                         (taut-api--request "conversations.list" params "GET")
                       (error
                        (if (string-match-p "missing_scope" (error-message-string err2))
                            (progn
                              (message "Taut: private_channel scope missing; retrying...")
                              (taut-api--request "conversations.list"
                                                 '((types . "public_channel,im,mpim")
                                                   (limit . 1000))
                                                 "GET"))
                          (signal (car err2) (cdr err2)))))))))))
         (channels (cdr (assoc 'channels res))))
    (dolist (c channels)
      (let* ((id (cdr (assoc 'id c)))
             (is-im (taut-api--bool (cdr (assoc 'is_im c))))
             (is-mpim (taut-api--bool (cdr (assoc 'is_mpim c))))
             (is-private (taut-api--bool (cdr (assoc 'is_private c))))
             (unread-count (or (cdr (assoc 'unread_count c)) 0))
             ;; Map name nicely
             (name (or (cond
                        (is-im
                         (let* ((uid (cdr (assoc 'user c)))
                                (user (taut-model-get-user uid)))
                           (or (and user (taut-user-username user)) (concat "user-" (or uid "unknown")))))
                        (is-mpim
                         (taut-api--clean-mpim-name (cdr (assoc 'name c))))
                        (t (cdr (assoc 'name c))))
                       id
                       "unknown"))
             (type (cond
                    ((or is-im is-mpim) 'dm)
                    (is-private 'private)
                    (t 'public)))
             (is-member (let ((val (assoc 'is_member c)))
                           (if val (taut-api--bool (cdr val)) t))))
        (when (or (not taut-only-show-subscribed-channels)
                  is-im
                  is-mpim
                  is-member)
          (taut-model-add-channel
           (make-taut-channel
            :id id
            :name name
            :type type
            :unread-count unread-count
            :mention-count (or (cdr (assoc 'mention_count c)) 0)
            :is-starred (taut-api--bool (cdr (assoc 'is_starred c)))
            :topic (cdr (assoc 'value (cdr (assoc 'topic c))))
            :purpose (cdr (assoc 'value (cdr (assoc 'purpose c))))))))))
  (message "Taut: Synced %d active conversations." (hash-table-count taut-channels)))

(defun taut-api-get-or-fetch-channel (channel-id)
  "Retrieve `taut-channel' for CHANNEL-ID from cache, or fetch it on-demand from Slack."
  (let ((chan (taut-model-get-channel channel-id)))
    (if (or chan (not (and (boundp 'taut-bot-token) taut-bot-token)))
        chan
      (condition-case nil
          (let* ((res (taut-api--request "conversations.info" `((channel . ,channel-id)) "GET"))
                 (c (cdr (assoc 'channel res))))
            (when c
              (let* ((id (cdr (assoc 'id c)))
                     (is-im (taut-api--bool (cdr (assoc 'is_im c))))
                     (is-mpim (taut-api--bool (cdr (assoc 'is_mpim c))))
                     (is-private (taut-api--bool (cdr (assoc 'is_private c))))
                     (unread-count (or (cdr (assoc 'unread_count c)) 0))
                     (name (or (cond
                                (is-im
                                 (let* ((uid (cdr (assoc 'user c)))
                                        (user (taut-model-get-user uid)))
                                   (or (and user (taut-user-username user)) (concat "user-" (or uid "unknown")))))
                                (is-mpim
                                 (taut-api--clean-mpim-name (cdr (assoc 'name c))))
                                (t (cdr (assoc 'name c))))
                               id
                               "unknown"))
                     (type (cond
                            ((or is-im is-mpim) 'dm)
                            (is-private 'private)
                            (t 'public)))
                     (new-chan (make-taut-channel
                                :id id
                                :name name
                                :type type
                                :unread-count unread-count
                                :mention-count (or (cdr (assoc 'unread_count_display_messages c)) 0)
                                :is-starred (taut-api--bool (cdr (assoc 'is_starred c)))
                                :topic (cdr (assoc 'value (cdr (assoc 'topic c))))
                                :purpose (cdr (assoc 'value (cdr (assoc 'purpose c)))))))
                (taut-model-add-channel new-chan)
                new-chan)))
        (error nil)))))

(defun taut-api-fetch-starred ()
  "Fetch starred items from Slack and mark channels and messages as starred."
  (interactive)
  (message "Taut: Syncing starred items...")
  (condition-case err
      (let* ((res (taut-api--request "stars.list" nil "GET"))
             (items (cdr (assoc 'items res)))
             (starred-chan-count 0)
             (starred-msg-count 0))
        (dolist (item items)
          (let* ((type (cdr (assoc 'type item))))
            (cond
             ;; Handle starred channels / groups / IMs
             ((member type '("channel" "group" "im"))
              (let* ((chan-id (cond
                               ((string= type "channel") (cdr (assoc 'channel item)))
                               ((string= type "group") (cdr (assoc 'group item)))
                               ((string= type "im") (cdr (assoc 'im item)))
                               (t nil))))
                (when chan-id
                  (let ((chan (taut-model-get-channel chan-id)))
                    (when chan
                      (setf (taut-channel-is-starred chan) t)
                      (cl-incf starred-chan-count))))))
             ;; Handle starred messages (bookmarks)
             ((string= type "message")
              (let* ((chan-id (cdr (assoc 'channel item)))
                     (msg-data (cdr (assoc 'message item)))
                     (ts (cdr (assoc 'ts msg-data)))
                     (user-id (or (cdr (assoc 'user msg-data)) (cdr (assoc 'bot_id msg-data)) "unknown"))
                     (raw-text (or (cdr (assoc 'text msg-data)) ""))
                     (text (taut-api-unescape-html raw-text))
                     (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) text))
                     (thread-ts (cdr (assoc 'thread_ts msg-data))))
                (when (and chan-id ts)
                  (let ((msg (make-taut-message
                              :id (concat "msg_" ts)
                              :channel-id chan-id
                              :user-id user-id
                              :text text
                              :ts ts
                              :thread-ts thread-ts
                              :is-unread nil
                              :is-mention is-mention
                              :is-starred t)))
                    (taut-model-add-message msg)
                    (cl-incf starred-msg-count))))))))
        (message "Taut: Synced %d starred conversations and %d bookmarked messages."
                 starred-chan-count starred-msg-count))
    (error
     (message "Taut: Starred sync failed: %s" (error-message-string err)))))

(defun taut-api-fetch-inbox-history ()
  "Pre-fetch message history for channels and DMs relevant to the Inbox.
This includes any channel with unread messages or mentions, and recent DMs.
If few or no conversations are active, fall back to fetching the history of
the first few public/private channels to populate the activity feed."
  (interactive)
  (message "Taut: Pre-fetching inbox activity history...")
  (let ((channels (taut-model-get-channels-list))
        (fetched-count 0))
    (dolist (chan channels)
      (let* ((id (taut-channel-id chan))
             (name (taut-channel-name chan))
             (type (taut-channel-type chan))
             (unreads (or (taut-channel-unread-count chan) 0))
             (mentions (or (taut-channel-mention-count chan) 0))
             (starred (taut-channel-is-starred chan)))
        ;; Debug print each channel state to help locate population bugs
        (message "Taut Debug: Channel check: ID=%s, Name=%s, Type=%s, Unreads=%d, Mentions=%d, Starred=%s"
                 id name type unreads mentions (if starred "yes" "no"))
        (let ((has-cached-messages (gethash id taut-messages)))
          (when (or (> unreads 0)
                    (> mentions 0)
                    (and (or (eq type 'dm) starred) (not has-cached-messages)))
            (ignore-errors
              (taut-api-fetch-history id 20)
              (cl-incf fetched-count))))))
    
    ;; Always seed history for up to 10 public/private channels without cached
    ;; messages to discover any unread channel activity.
    (let ((seeded-count 0))
      (dolist (chan channels)
        (let* ((id (taut-channel-id chan))
               (type (taut-channel-type chan))
               (has-cached-messages (gethash id taut-messages)))
          (when (and (< seeded-count 10)
                     (member type '(public private))
                     (not has-cached-messages))
            (ignore-errors
              (taut-api-fetch-history id 15)
              (cl-incf fetched-count)
              (cl-incf seeded-count))))))
    (message "Taut: Pre-fetched history for %d active conversations." fetched-count)))

(defun taut-api-unescape-html (text)
  "Replace common HTML entities in TEXT with their literal characters."
  (if text
      (let ((s text))
        (setq s (replace-regexp-in-string "&lt;" "<" s t t))
        (setq s (replace-regexp-in-string "&gt;" ">" s t t))
        (setq s (replace-regexp-in-string "&amp;" "&" s t t))
        s)
    ""))

(defun taut-api--format-file-shares (event text)
  "If EVENT contains shared files, format them and append to TEXT."
  (let ((files (cdr (assoc 'files event)))
        (file-links nil))
    (dolist (f files)
      (let* ((name (or (cdr (assoc 'title f)) (cdr (assoc 'name f)) "file"))
             (download-url (or (cdr (assoc 'url_private_download f))
                               (cdr (assoc 'url_private f))
                               (cdr (assoc 'permalink f))))
             (browser-url (or (cdr (assoc 'permalink f))
                              (cdr (assoc 'url_private f)))))
        (if download-url
            (let* ((name-hex (url-hexify-string name))
                   (browser-hex (url-hexify-string (or browser-url "")))
                   (has-query (string-match-p "\\?" download-url))
                   (taut-url (concat (replace-regexp-in-string "^https://" "taut-file://" download-url)
                                     (if has-query "&" "?")
                                     "taut_name=" name-hex
                                     "&browser_url=" browser-hex)))
              (push (format "📎 *[Shared File]*: _%s_ (<%s|Download File>)" name taut-url) file-links))
          (push (format "📎 *[Shared File]*: _%s_" name) file-links))))
    (if file-links
        (let ((files-str (mapconcat #'identity (nreverse file-links) "\n")))
          (if (and text (not (string= text "")))
              (concat text "\n" files-str)
            files-str))
      text)))

(defun taut-api-fetch-history (channel-id &optional limit latest)
  "Fetch history for CHANNEL-ID, translating and loading into state.
If LATEST is specified, fetch messages older than LATEST (for pagination)."
  (let* ((chan (taut-model-get-channel channel-id))
         (unread-left (if chan (or (taut-channel-unread-count chan) 0) 0))
         (params (append `((channel . ,channel-id)
                           (limit . ,(or limit 40)))
                         (when latest
                           `((latest . ,latest)))))
         (res (condition-case err
                  (taut-api--request "conversations.history" params "GET")
                (error
                 (if (string-match-p "not_in_channel" (error-message-string err))
                     (progn
                       (message "Taut: Joining channel %s..." channel-id)
                       (taut-api--request "conversations.join" `((channel . ,channel-id)) "POST")
                       (taut-api--request "conversations.history" params "GET"))
                   (signal (car err) (cdr err))))))
         ;; Fetch last-read from conversations.info for public/private channels
         ;; after successfully retrieving history to guarantee we joined first.
         (last-read
          (and chan
               (member (taut-channel-type chan) '(public private))
               (let* ((info-res (ignore-errors
                                  (taut-api--request "conversations.info"
                                                     `((channel . ,channel-id))
                                                     "GET")))
                      (chan-info (cdr (assoc 'channel info-res))))
                 (cdr (assoc 'last_read chan-info)))))
         (messages (cdr (assoc 'messages res))))
    (unless latest
      ;; Keep starred/bookmarked messages so we don't lose them on refresh
      (let ((starred-msgs (cl-remove-if-not #'taut-message-is-starred (gethash channel-id taut-messages))))
        (setf (gethash channel-id taut-messages) starred-msgs)))
    (setq messages (nreverse messages))
    ;; Count total eligible messages to decide unread threshold
    (let ((eligible-count 0)
          (current-eligible-idx 0))
      (dolist (m messages)
        (let* ((ts (cdr (assoc 'ts m)))
               (subtype (cdr (assoc 'subtype m)))
               (user-id (or (cdr (assoc 'user m)) (cdr (assoc 'bot_id m)) "unknown"))
               (is-me (equal user-id taut-current-user-id))
               (is-skipped (member subtype '("channel_join" "channel_leave" "channel_topic" "channel_purpose" "channel_name"))))
          (when (and ts (not is-skipped) (not is-me))
            (cl-incf eligible-count))))
      (dolist (m messages)
        (let* ((ts (cdr (assoc 'ts m)))
               (subtype (cdr (assoc 'subtype m)))
               (user-id (or (cdr (assoc 'user m)) (cdr (assoc 'bot_id m)) "unknown"))
               (is-me (equal user-id taut-current-user-id))
               (is-skipped (member subtype '("channel_join" "channel_leave" "channel_topic" "channel_purpose" "channel_name"))))
          ;; Skip system join/leave messages and verify TS is present
          (when ts
            (unless is-skipped
              (let* ((raw-text (or (cdr (assoc 'text m)) ""))
                     (full-text (taut-api--get-message-text m raw-text))
                     (text (taut-api-unescape-html (taut-api--format-file-shares m full-text)))
                     (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) text))
                     (thread-ts (cdr (assoc 'thread_ts m)))
                     (reply-count (cdr (assoc 'reply_count m)))
                     (reactions (cdr (assoc 'reactions m)))
                     (is-starred (taut-api--bool (cdr (assoc 'is_starred m))))
                     ;; Mark as unread if it is not sent by current user.
                     ;; Use last-read if available; otherwise use index-based
                     ;; unread-left counting.
                     (is-unread (and (or (not is-me)
                                         (and (boundp 'taut-inbox-include-self-dm)
                                              taut-inbox-include-self-dm
                                              (let ((chan (taut-model-get-channel channel-id)))
                                                (and chan (taut-channel-is-self-dm-p chan)))))
                                     (if last-read
                                         (string< last-read ts)
                                       (>= current-eligible-idx
                                           (- eligible-count unread-left)))))
                     ;; Convert reactions list to taut representation
                     (model-reactions nil))
                (unless (and is-me
                             (not (and (boundp 'taut-inbox-include-self-dm)
                                       taut-inbox-include-self-dm
                                       (let ((chan (taut-model-get-channel channel-id)))
                                         (and chan (taut-channel-is-self-dm-p chan))))))
                  (cl-incf current-eligible-idx))
                (dolist (r reactions)
                  (let ((r-name (cdr (assoc 'name r))))
                    (when r-name
                      (push (cons (concat ":" r-name ":")
                                  (cdr (assoc 'users r)))
                            model-reactions))))
                
                (taut-model-add-message
                 (make-taut-message
                  :id (concat "msg_" ts)
                  :channel-id channel-id
                  :user-id user-id
                  :text text
                  :ts ts
                  :thread-ts thread-ts
                  :reply-count (or reply-count 0)
                  :reactions model-reactions
                  :is-unread is-unread
                  :is-mention is-mention
                  :is-starred is-starred)
                 nil
                 t)))))))
    ;; If we fetched with last-read, we can accurately update the channel's
    ;; unread and mention counts based on the identified unread messages.
    (when (and chan last-read (not latest))
      (let ((total-unreads 0)
            (total-mentions 0))
        (dolist (m (gethash channel-id taut-messages))
          (when (taut-message-is-unread m)
            (cl-incf total-unreads)
            (when (taut-message-is-mention m)
              (cl-incf total-mentions))))
        (setf (taut-channel-unread-count chan) total-unreads)
        (setf (taut-channel-mention-count chan) total-mentions)
        (when (fboundp 'taut-cache-save-channel)
          (taut-cache-save-channel chan))))
    (taut-model-trigger-update)
    messages))

(defun taut-api-fetch-replies (channel-id thread-ts)
  "Fetch all thread replies for THREAD-TS in CHANNEL-ID."
  (let* ((params `((channel . ,channel-id)
                   (ts . ,thread-ts)))
         (res (taut-api--request "conversations.replies" params "GET"))
         (messages (cdr (assoc 'messages res))))
    ;; Update root message's reply count from server's root message metadata
    (let* ((root-msg-data (car messages))
           (reply-count (cdr (assoc 'reply_count root-msg-data)))
           (root-is-starred (taut-api--bool (cdr (assoc 'is_starred root-msg-data))))
           (root-chan-msgs (gethash channel-id taut-messages))
           (root-msg (cl-find thread-ts root-chan-msgs :key #'taut-message-ts :test #'equal)))
      (if root-msg
          (progn
            (when reply-count
              (setf (taut-message-reply-count root-msg) reply-count))
            (setf (taut-message-is-starred root-msg) root-is-starred))
        ;; If the root message is not in our channel history cache, construct and add it
        (when root-msg-data
          (let* ((ts (cdr (assoc 'ts root-msg-data)))
                 (user-id (or (cdr (assoc 'user root-msg-data)) (cdr (assoc 'bot_id root-msg-data)) "unknown"))
                 (raw-text (or (cdr (assoc 'text root-msg-data)) ""))
                 (full-text (taut-api--get-message-text root-msg-data raw-text))
                 (text (taut-api-unescape-html full-text))
                 (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) text)))
            (when ts
              (taut-model-add-message
               (make-taut-message
                :id (concat "msg_" ts)
                :channel-id channel-id
                :user-id user-id
                :text text
                :ts ts
                :thread-ts nil
                :reply-count (or reply-count 0)
                :is-unread nil
                :is-mention is-mention
                :is-starred root-is-starred)
               t))))))
    ;; Reset old thread cache, preserving any bookmarked replies
    (let ((starred-replies (cl-remove-if-not #'taut-message-is-starred (gethash thread-ts taut-threads))))
      (setf (gethash thread-ts taut-threads) starred-replies))
    ;; Skip the first message as it is the root message (already in channels history)
    (dolist (m (cdr messages))
      (let* ((ts (cdr (assoc 'ts m)))
             (user-id (or (cdr (assoc 'user m)) (cdr (assoc 'bot_id m)) "unknown"))
             (raw-text (or (cdr (assoc 'text m)) ""))
             (full-text (taut-api--get-message-text m raw-text))
             (text (taut-api-unescape-html full-text))
             (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) text))
             (is-starred (taut-api--bool (cdr (assoc 'is_starred m)))))
        (when ts
          (taut-model-add-message
           (make-taut-message
            :id (concat "msg_" ts)
            :channel-id channel-id
            :user-id user-id
            :text text
            :ts ts
            :thread-ts thread-ts
            :reply-count 0
            :is-unread nil
            :is-mention is-mention
            :is-starred is-starred)
           t))))
    (taut-model-trigger-update)))

(defun taut-api-post-message (channel-id text &optional thread-ts)
  "Send a message TEXT to CHANNEL-ID. Option THREAD-TS to post as thread reply."
  (let* ((params `((channel . ,channel-id)
                   (text . ,text)))
         (params (if thread-ts (append params `((thread_ts . ,thread-ts))) params))
         (res (taut-api--request "chat.postMessage" params "POST"))
         (m (cdr (assoc 'message res)))
         (ts (or (cdr (assoc 'ts m)) (cdr (assoc 'ts res)) (format "%d.0000" (time-convert nil 'integer)))))
    ;; Inject our sent message directly into state for instant responsiveness
    (taut-model-add-message
     (make-taut-message
      :id (concat "msg_" ts)
      :channel-id channel-id
      :user-id taut-current-user-id
      :text text
      :ts ts
      :thread-ts thread-ts
      :reply-count 0
      :is-unread nil
      :is-mention nil))))

(defun taut-api-update-message (channel-id ts text)
  "Update an existing message identified by TS on CHANNEL-ID with new TEXT."
  (let* ((params `((channel . ,channel-id)
                   (ts . ,ts)
                   (text . ,text)))
         (res (taut-api--request "chat.update" params "POST")))
    ;; Update local model state
    (let ((m (taut-model-get-message-by-ts ts)))
      (when m
        (setf (taut-message-text m) text)
        (taut-model-trigger-update)))
    res))

(defun taut-api-delete-message (channel-id ts)
  "Delete an existing message identified by TS on CHANNEL-ID."
  (let* ((params `((channel . ,channel-id)
                   (ts . ,ts)))
         (res (taut-api--request "chat.delete" params "POST")))
    ;; Update local model state (mark deleted)
    (taut-model-delete-message ts)
    res))

(defun taut-api-add-reaction (channel-id timestamp emoji)
  "Add EMOJI reaction to message at TIMESTAMP in CHANNEL-ID."
  ;; Remove bounding colons if present (e.g. ":thumbsup:" -> "thumbsup")
  (let* ((emoji (or emoji ""))
         (emoji-clean (if (string-match "^:\\(.*\\):$" emoji)
                          (match-string 1 emoji)
                        emoji))
         (params `((channel . ,channel-id)
                   (timestamp . ,timestamp)
                   (name . ,emoji-clean))))
    (taut-api--request "reactions.add" params "POST")))
(defun taut-api-star-add (channel-id timestamp)
  "Star/bookmark a message at TIMESTAMP in CHANNEL-ID."
  (let ((params `((channel . ,channel-id)
                  (timestamp . ,timestamp))))
    (taut-api--request "stars.add" params "POST")))

(defun taut-api-star-remove (channel-id timestamp)
  "Unstar/unbookmark a message at TIMESTAMP in CHANNEL-ID."
  (let ((params `((channel . ,channel-id)
                  (timestamp . ,timestamp))))
    (taut-api--request "stars.remove" params "POST")))

(defun taut-api-mark-channel-read (channel-id &optional ts)
  "Mark CHANNEL-ID as read up to TS on Slack.
If TS is nil, use the timestamp of the latest message in memory."
  (interactive "sChannel ID: ")
  (let* ((resolved-ts (or ts
                          (let ((msgs (gethash channel-id taut-messages)))
                            (and msgs (taut-message-ts (car (last msgs))))))))
    (when resolved-ts
      (ignore-errors
        (taut-api--request "conversations.mark"
                           `((channel . ,channel-id)
                             (ts . ,resolved-ts))
                            "POST")))))

(defun taut-api-create-channel (name &optional is-private)
  "Create a new channel with NAME.
If IS-PRIVATE is non-nil, create a private channel."
  (let* ((params `((name . ,name)
                   (is_private . ,(if is-private t nil))))
         (res (taut-api--request "conversations.create" params "POST"))
         (chan-id (cdr (assoc 'id (cdr (assoc 'channel res))))))
    (when chan-id
      (taut-api-get-or-fetch-channel chan-id)
      (taut-model-trigger-update))
    res))

(defun taut-api-invite-to-channel (channel-id user-ids)
  "Invite USER-IDS (list of string user IDs) to CHANNEL-ID."
  (let* ((users-str (if (listp user-ids) (mapconcat #'identity user-ids ",") user-ids))
         (params `((channel . ,channel-id)
                   (users . ,users-str)))
         (res (taut-api--request "conversations.invite" params "POST")))
    res))

(defun taut-api-kick-from-channel (channel-id user-id)
  "Remove USER-ID from CHANNEL-ID."
  (let* ((params `((channel . ,channel-id)
                   (user . ,user-id)))
         (res (taut-api--request "conversations.kick" params "POST")))
    res))

(defun taut-api-set-channel-topic (channel-id topic)
  "Set the topic of CHANNEL-ID to TOPIC."
  (let* ((params `((channel . ,channel-id)
                   (topic . ,topic)))
         (res (taut-api--request "conversations.setTopic" params "POST"))
         (chan (taut-model-get-channel channel-id)))
    (when chan
      (setf (taut-channel-topic chan) topic)
      (taut-model-trigger-update))
    res))

(defun taut-api-archive-channel (channel-id)
  "Archive CHANNEL-ID."
  (let* ((params `((channel . ,channel-id)))
         (res (taut-api--request "conversations.archive" params "POST")))
    (taut-model-delete-channel channel-id)
    res))

(defun taut-api-get-channel-members (channel-id)
  "Retrieve list of member user IDs for CHANNEL-ID."
  (let* ((params `((channel . ,channel-id)))
         (res (taut-api--request "conversations.members" params "GET"))
         (members (cdr (assoc 'members res))))
    members))

(defun taut-api-open-dm (user-id)
  "Open or create a direct message channel with USER-ID."
  (let* ((user-id (or user-id "unknown"))
         (params `((users . ,user-id)))
         (res (taut-api--request "conversations.open" params "POST"))
         (channel (cdr (assoc 'channel res))))
    (if (and (cdr (assoc 'ok res)) channel)
        (let* ((id (cdr (assoc 'id channel)))
               (user (taut-model-get-user user-id))
               (username (or (and user (taut-user-username user)) (concat "user-" user-id)))
               (id (or id (concat "DM_FALLBACK_" user-id)))
               (existing (taut-model-get-channel id)))
          (unless existing
            (taut-model-add-channel
             (make-taut-channel
              :id id
              :name username
              :type 'dm
              :unread-count 0
              :mention-count 0)))
          id)
      (error "Taut API Error: Failed to open DM with %s: %s"
             user-id (or (cdr (assoc 'error res)) "unknown error")))))

(defun taut-api-upload-file (channel-id file-path &optional thread-ts)
  "Upload a file at FILE-PATH to CHANNEL-ID (and optionally THREAD-TS).
Implements Slack's modern 3-step files upload flow:
1. files.getUploadURLExternal to get a temporary upload URL and file ID.
2. POST the raw file bytes directly to the upload URL using curl.
3. files.completeUploadExternal to commit the upload and share it."
  (unless (file-exists-p file-path)
    (error "File does not exist: %s" file-path))
  (let* ((filename (file-name-nondirectory file-path))
         (size (file-attribute-size (file-attributes file-path)))
         (curl-bin (executable-find "curl")))
    (unless curl-bin
      (error "Taut: `curl' executable not found on system. Please install curl."))
    
    (message "Taut: Initiating upload for %s (%d bytes)..." filename size)
    ;; Step 1: getUploadURLExternal (uses GET to ensure robust query-param parsing)
    (let* ((get-res (taut-api--request "files.getUploadURLExternal"
                                       `((filename . ,filename)
                                         (length . ,size))
                                       "GET"))
           (upload-url (cdr (assoc 'upload_url get-res)))
           (file-id (cdr (assoc 'file_id get-res))))
      (unless (and upload-url file-id)
        (error "Taut: Failed to retrieve upload URL or file ID from Slack"))
      
      (message "Taut: Sending raw bytes to Slack Storage...")
      ;; Step 2: Upload file bytes using curl POST/PUT
      (with-temp-buffer
        (let ((args (list "-s" "-F" (format "file=@%s" (expand-file-name file-path)) upload-url)))
          (apply #'call-process curl-bin nil t nil args)))
      
      (message "Taut: Completing upload and sharing...")
      ;; Step 3: completeUploadExternal
      (let* ((files-param (list (list (cons 'id file-id) (cons 'title filename))))
             (params `((files . ,files-param)
                       (channel_id . ,channel-id))))
        (when thread-ts
          (setq params (append params `((thread_ts . ,thread-ts)))))
        (taut-api--request "files.completeUploadExternal" params "POST"))
      (message "Taut: Successfully uploaded %s!" filename))))

(defun taut-api-download-file (url local-path)
  "Download file from Slack private URL to LOCAL-PATH using curl.
Uses the active bearer token for authorization."
  (let ((token taut-bot-token)
        (curl-bin (executable-find "curl")))
    (unless token
      (error "Taut: Token not configured"))
    (unless curl-bin
      (error "Taut: `curl' executable not found"))
    (message "Taut: Downloading file to %s..." local-path)
    (with-temp-buffer
      (let ((args (list "-s" "-L"
                        "-H" (concat "Authorization: Bearer " token)
                        "-o" (expand-file-name local-path)
                        url)))
        (apply #'call-process curl-bin nil t nil args)))
    (message "Taut: Successfully downloaded file to %s" local-path)))

(defun taut-api-search-messages (query &optional sort sort-dir page count)
  "Search Slack workspace messages matching QUERY.
Optional arguments:
- SORT: 'timestamp' or 'score'
- SORT-DIR: 'asc' or 'desc'
- PAGE: page number to retrieve
- COUNT: number of items per page"
  (let ((params `((query . ,query))))
    (when sort (push `(sort . ,sort) params))
    (when sort-dir (push `(sort_dir . ,sort-dir) params))
    (when page (push `(page . ,page) params))
    (when count (push `(count . ,count) params))
    (taut-api--request "search.messages" params "GET")))

(defun taut-api-fetch-custom-emojis ()
  "Fetch custom emojis in the workspace and populate `taut-custom-emojis'."
  (interactive)
  (message "Taut: Syncing custom emojis...")
  (let* ((res (taut-api--request "emoji.list" nil "GET"))
         (emoji-map (cdr (assoc 'emoji res))))
    (clrhash taut-custom-emojis)
    (dolist (pair emoji-map)
      (let ((name (symbol-name (car pair)))
            (url (cdr pair)))
        (puthash name url taut-custom-emojis)))
    (message "Taut: Synced %d workspace custom emojis." (hash-table-count taut-custom-emojis))))

(provide 'taut-api)
;;; taut-api.el ends here

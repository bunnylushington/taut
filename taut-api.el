;;; taut-api.el --- Slack Web API Client for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Google DeepMind

;; Author: Antigravity
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
         (bot-id (cdr (assoc 'user_id res))))
    (setq taut-current-user-id bot-id)
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
             (is-bot (cdr (assoc 'is_bot m)))
             (deleted (cdr (assoc 'deleted m))))
        (unless (or is-bot deleted)
          (taut-model-add-user
           (make-taut-user
            :id id
            :username (cdr (assoc 'name m))
            :real-name (or (cdr (assoc 'real_name profile)) (cdr (assoc 'real_name m)) (cdr (assoc 'name m)))
            :presence (if (equal (cdr (assoc 'presence m)) "away") 'away 'online)
            :is-me (equal id taut-current-user-id))))))
  (message "Taut: Synced %d workspace users." (hash-table-count taut-users))))

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
  (let* ((params '((types . "public_channel,private_channel,im,mpim")
                   (limit . 100)))
         (res (taut-api--request "conversations.list" params "POST"))
         (channels (cdr (assoc 'channels res))))
    (dolist (c channels)
      (let* ((id (cdr (assoc 'id c)))
             (is-im (cdr (assoc 'is_im c)))
             (is-mpim (cdr (assoc 'is_mpim c)))
             (is-private (cdr (assoc 'is_private c)))
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
             (is-member (cdr (assoc 'is_member c))))
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
            :is-starred (cdr (assoc 'is_starred c))
            :topic (cdr (assoc 'value (cdr (assoc 'topic c))))
            :purpose (cdr (assoc 'value (cdr (assoc 'purpose c))))))))))
  (message "Taut: Synced %d active conversations." (hash-table-count taut-channels)))

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
        (when (or (> unreads 0)
                  (> mentions 0)
                  (eq type 'dm)
                  starred)
          (ignore-errors
            (taut-api-fetch-history id 20)
            (cl-incf fetched-count)))))
    
    ;; Fallback: if we fetched history for fewer than 3 conversations, fetch
    ;; history for the first 5 public/private channels to seed the activity feed
    (when (< fetched-count 3)
      (let ((fallback-count 0))
        (dolist (chan channels)
          (let ((id (taut-channel-id chan))
                (type (taut-channel-type chan)))
            (when (and (< fallback-count 5)
                       (member type '(public private)))
              (ignore-errors
                (taut-api-fetch-history id 15)
                (cl-incf fetched-count)
                (cl-incf fallback-count)))))))
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
  (let* ((params (append `((channel . ,channel-id)
                           (limit . ,(or limit 40)))
                         (when latest
                           `((latest . ,latest)))))
         (res (condition-case err
                  (taut-api--request "conversations.history" params "POST")
                (error
                 (if (string-match-p "not_in_channel" (error-message-string err))
                     (progn
                       (message "Taut: Joining channel %s..." channel-id)
                       (taut-api--request "conversations.join" `((channel . ,channel-id)) "POST")
                       (taut-api--request "conversations.history" params "POST"))
                   (signal (car err) (cdr err))))))
         (messages (cdr (assoc 'messages res))))
    (unless latest
      ;; Keep starred/bookmarked messages so we don't lose them when history is refreshed
      (let ((starred-msgs (cl-remove-if-not #'taut-message-is-starred (gethash channel-id taut-messages))))
        (setf (gethash channel-id taut-messages) starred-msgs)))
    (setq messages (nreverse messages))
    (dolist (m messages)
      (let* ((ts (cdr (assoc 'ts m)))
             (subtype (cdr (assoc 'subtype m)))
             (user-id (or (cdr (assoc 'user m)) (cdr (assoc 'bot_id m)) "unknown")))
        ;; Skip system join/leave messages for visual cleanliness and verify TS is present
        (when ts
          (unless (member subtype '("channel_join" "channel_leave" "channel_topic" "channel_purpose" "channel_name"))
            (let* ((raw-text (or (cdr (assoc 'text m)) ""))
                   (text (taut-api-unescape-html (taut-api--format-file-shares m raw-text)))
                   (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) text))
                   (thread-ts (cdr (assoc 'thread_ts m)))
                   (reply-count (cdr (assoc 'reply_count m)))
                   (reactions (cdr (assoc 'reactions m)))
                   (starred-val (cdr (assoc 'is_starred m)))
                   (is-starred (and starred-val (not (eq starred-val :json-false))))
                   ;; Convert reactions list to taut model representation
                   (model-reactions nil))
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
                :is-unread nil
                :is-mention is-mention
                :is-starred is-starred)))))))
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
           (root-starred-val (cdr (assoc 'is_starred root-msg-data)))
           (root-is-starred (and root-starred-val (not (eq root-starred-val :json-false))))
           (root-chan-msgs (gethash channel-id taut-messages))
           (root-msg (cl-find thread-ts root-chan-msgs :key #'taut-message-ts :test #'equal)))
      (when (and root-msg reply-count)
        (setf (taut-message-reply-count root-msg) reply-count))
      (when (and root-msg root-starred-val)
        (setf (taut-message-is-starred root-msg) root-is-starred)))
    ;; Reset old thread cache, preserving any bookmarked replies
    (let ((starred-replies (cl-remove-if-not #'taut-message-is-starred (gethash thread-ts taut-threads))))
      (setf (gethash thread-ts taut-threads) starred-replies))
    ;; Skip the first message as it is the root message (already in channels history)
    (dolist (m (cdr messages))
      (let* ((ts (cdr (assoc 'ts m)))
             (user-id (or (cdr (assoc 'user m)) (cdr (assoc 'bot_id m)) "unknown"))
             (text (taut-api-unescape-html (or (cdr (assoc 'text m)) "")))
             (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) text))
             (starred-val (cdr (assoc 'is_starred m)))
             (is-starred (and starred-val (not (eq starred-val :json-false)))))
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

(defun taut-api-delete-message (channel-id ts)
  "Delete a message with timestamp TS in CHANNEL-ID."
  (taut-api--request "chat.delete"
                     `((channel . ,channel-id)
                       (ts . ,ts))
                     "POST"))

(provide 'taut-api)
;;; taut-api.el ends here

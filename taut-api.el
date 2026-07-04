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
             (is-private (cdr (assoc 'is_private c)))
             (unread-count (or (cdr (assoc 'unread_count c)) 0))
             ;; Map name nicely
             (name (or (cond
                        (is-im
                         (let* ((uid (cdr (assoc 'user c)))
                                (user (taut-model-get-user uid)))
                           (or (and user (taut-user-username user)) (concat "user-" (or uid "unknown")))))
                        (t (cdr (assoc 'name c))))
                       id
                       "unknown"))
             (type (cond
                    (is-im 'dm)
                    (is-private 'private)
                    (t 'public)))
             (is-member (cdr (assoc 'is_member c))))
        (when (or (not taut-only-show-subscribed-channels)
                  is-im
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
  "Fetch starred items from Slack and mark channels as starred."
  (interactive)
  (message "Taut: Syncing starred conversations...")
  (condition-case err
      (let* ((res (taut-api--request "stars.list" nil "GET"))
             (items (cdr (assoc 'items res)))
             (starred-count 0))
        (dolist (item items)
          (let* ((type (cdr (assoc 'type item)))
                 (chan-id (cond
                           ((string= type "channel") (cdr (assoc 'channel item)))
                           ((string= type "group") (cdr (assoc 'group item)))
                           ((string= type "im") (cdr (assoc 'im item)))
                           (t nil))))
            (when chan-id
              (let ((chan (taut-model-get-channel chan-id)))
                (when chan
                  (setf (taut-channel-is-starred chan) t)
                  (cl-incf starred-count))))))
        (message "Taut: Synced %d starred conversations." starred-count))
    (error
     (message "Taut: Starred sync failed: %s" (error-message-string err)))))

(defun taut-api-unescape-html (text)
  "Replace common HTML entities in TEXT with their literal characters."
  (if text
      (let ((s text))
        (setq s (replace-regexp-in-string "&lt;" "<" s t t))
        (setq s (replace-regexp-in-string "&gt;" ">" s t t))
        (setq s (replace-regexp-in-string "&amp;" "&" s t t))
        s)
    ""))

(defun taut-api-fetch-history (channel-id &optional limit)
  "Fetch recent history for CHANNEL-ID, translating and loading into state."
  (let* ((params `((channel . ,channel-id)
                   (limit . ,(or limit 40))))
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
    (setf (gethash channel-id taut-messages) nil)
    (dolist (m (nreverse messages))
      (let* ((ts (cdr (assoc 'ts m)))
             (subtype (cdr (assoc 'subtype m)))
             (user-id (or (cdr (assoc 'user m)) (cdr (assoc 'bot_id m)) "unknown")))
        ;; Skip system join/leave messages for visual cleanliness and verify TS is present
        (when ts
          (unless (member subtype '("channel_join" "channel_leave" "channel_topic" "channel_purpose" "channel_name"))
            (let* ((raw-text (or (cdr (assoc 'text m)) ""))
                   (text (taut-api-unescape-html raw-text))
                   (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) text))
                   (thread-ts (cdr (assoc 'thread_ts m)))
                   (reply-count (cdr (assoc 'reply_count m)))
                   (reactions (cdr (assoc 'reactions m)))
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
                :is-mention is-mention)))))))
    (taut-model-trigger-update)))

(defun taut-api-fetch-replies (channel-id thread-ts)
  "Fetch all thread replies for THREAD-TS in CHANNEL-ID."
  (let* ((params `((channel . ,channel-id)
                   (ts . ,thread-ts)))
         (res (taut-api--request "conversations.replies" params "GET"))
         (messages (cdr (assoc 'messages res))))
    ;; Update root message's reply count from server's root message metadata
    (let* ((root-msg-data (car messages))
           (reply-count (cdr (assoc 'reply_count root-msg-data)))
           (root-chan-msgs (gethash channel-id taut-messages))
           (root-msg (cl-find thread-ts root-chan-msgs :key #'taut-message-ts :test #'equal)))
      (when (and root-msg reply-count)
        (setf (taut-message-reply-count root-msg) reply-count)))
    ;; Reset old thread cache
    (setf (gethash thread-ts taut-threads) nil)
    ;; Skip the first message as it is the root message (already in channels history)
    (dolist (m (cdr messages))
      (let* ((ts (cdr (assoc 'ts m)))
             (user-id (or (cdr (assoc 'user m)) (cdr (assoc 'bot_id m)) "unknown"))
             (text (taut-api-unescape-html (or (cdr (assoc 'text m)) "")))
             (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) text)))
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
            :is-mention is-mention)
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

(provide 'taut-api)
;;; taut-api.el ends here

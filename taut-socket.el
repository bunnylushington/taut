;;; taut-socket.el --- Slack Socket Mode Real-Time Client for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Google DeepMind

;; Author: Antigravity
;; Keywords: comm, slack

;;; Commentary:
;; This file implements Slack's Socket Mode using the `websocket` library.
;; It connects to Slack's real-time streaming servers over a secure WebSocket,
;; receives events (messages, mentions, reactions), acknowledges them immediately,
;; and updates the local `taut-model` to provide live notifications.

;;; Code:

(require 'websocket)
(require 'json)
(require 'cl-lib)
(require 'taut-model)
(require 'taut-api)

(defvar taut-socket-ws nil
  "The active WebSocket connection object.")

(defvar taut-socket-retry-timer nil
  "Timer used to schedule connection retries.")

;; Diagnostic Metric Variables
(defvar taut-socket-events-count 0
  "Total number of WebSocket payloads received.")

(defvar taut-socket-events-by-type nil
  "Alist mapping event types (symbols) to integers.")

(defvar taut-socket-last-event-ts nil
  "Timestamp string of the last received event.")

(defvar taut-socket-last-event-type nil
  "Type string of the last received event.")

;;;###autoload
(defun taut-socket-connect ()
  "Connect to Slack Socket Mode to receive real-time updates."
  (interactive)
  (unless taut-app-token
    (error "Taut: `taut-app-token' (starting with xapp-) must be configured for Socket Mode"))
  
  (setq taut-socket-events-count 0
        taut-socket-events-by-type nil
        taut-socket-last-event-ts nil
        taut-socket-last-event-type nil)

  (message "Taut Socket: Fetching WebSocket URL from Slack...")
  (condition-case err
      (let* ((res (taut-api--request "apps.connections.open" nil "POST" t)) ; t specifies AppToken
             (ws-url (cdr (assoc 'url res))))
        (if (null ws-url)
            (error "Failed to retrieve WebSocket URL from response")
          (taut-socket--open-websocket ws-url)))
    (error
     (message "Taut Socket Connection Failed: %s. Retrying in 10s..." (error-message-string err))
     (taut-socket-schedule-retry))))

(defun taut-socket-disconnect ()
  "Disconnect the active Socket Mode WebSocket connection."
  (interactive)
  (when taut-socket-ws
    (websocket-close taut-socket-ws)
    (setq taut-socket-ws nil))
  (when taut-socket-retry-timer
    (cancel-timer taut-socket-retry-timer)
    (setq taut-socket-retry-timer nil))
  (message "Taut Socket: Disconnected."))

(defun taut-socket-schedule-retry ()
  "Schedule a connection retry."
  (when taut-socket-retry-timer
    (cancel-timer taut-socket-retry-timer))
  (setq taut-socket-retry-timer
        (run-with-timer 10 nil #'taut-socket-connect)))

(defun taut-socket--open-websocket (url)
  "Open a WebSocket connection to URL and assign event callbacks."
  (when taut-socket-ws
    (websocket-close taut-socket-ws))
  
  (setq taut-socket-ws
        (websocket-open
         url
         :on-open (lambda (_ws)
                    (message "Taut Socket: Connected to Slack Live Stream!")
                    (when taut-socket-retry-timer
                      (cancel-timer taut-socket-retry-timer)
                      (setq taut-socket-retry-timer nil)))
         
         :on-message (lambda (ws frame)
                       (condition-case err
                           (let* ((payload-str (websocket-frame-text frame))
                                  ;; Standardize JSON parsing mapping false to nil
                                  (json-object-type 'alist)
                                  (json-array-type 'list)
                                  (json-key-type 'symbol)
                                  (json-false nil)
                                  (json-null nil)
                                  (data (json-read-from-string payload-str)))
                             (taut-socket--handle-payload ws data))
                         (error
                          (message "Taut Socket: Error parsing/handling frame: %s (Frame: %s)"
                                   (error-message-string err)
                                   (websocket-frame-text frame)))))
         
         :on-close (lambda (_ws)
                     (message "Taut Socket: Connection closed by server. Reconnecting...")
                     (setq taut-socket-ws nil)
                     (taut-socket-schedule-retry))
         
         :on-error (lambda (_ws type err)
                     (message "Taut Socket Error (%s): %s" type err)))))

(defun taut-socket--handle-payload (ws data)
  "Acknowledge payload and dispatch event data."
  (let ((envelope-id (cdr (assoc 'envelope_id data)))
        (type (cdr (assoc 'type data)))
        (payload (cdr (assoc 'payload data))))
    
    ;; Update metrics
    (cl-incf taut-socket-events-count)
    (setq taut-socket-last-event-ts (format-time-string "%Y-%m-%d %H:%M:%S")
          taut-socket-last-event-type type)
    (let* ((sym-type (intern (or type "unknown")))
           (existing (assoc sym-type taut-socket-events-by-type)))
      (if existing
          (setcdr existing (1+ (cdr existing)))
        (push (cons sym-type 1) taut-socket-events-by-type)))

    ;; Log every payload received
    (message "Taut Socket: Received payload [type=%s] [envelope_id=%s]" type envelope-id)
    
    ;; 1. Send immediate acknowledgment back to Slack to avoid retry loops
    (when envelope-id
      (websocket-send-text ws (json-encode `((envelope_id . ,envelope-id)))))
    
    ;; 2. Dispatch events
    (cond
     ((string= type "hello")
      (message "Taut Socket: Handshake completed successfully. Active stream connected!"))
     
     ((string= type "events_api")
      (let* ((event (cdr (assoc 'event payload)))
             (event-type (cdr (assoc 'type event))))
        (message "Taut Socket: Dispatching Events API [event_type=%s]" event-type)
        (cond
          ;; Handle incoming message
          ((string= event-type "message")
           (let* ((chan-id (cdr (assoc 'channel event)))
                  (subtype (cdr (assoc 'subtype event))))
             (cond
              ;; 1. Handle edited messages
              ((string= subtype "message_changed")
               (let* ((sub-msg (cdr (assoc 'message event)))
                      (ts (cdr (assoc 'ts sub-msg)))
                      (text (taut-api-unescape-html (or (cdr (assoc 'text sub-msg)) ""))))
                 (message "Taut Socket: Message edited on channel %s, ts: %s" chan-id ts)
                 (when ts
                   (let ((updated nil))
                     ;; Scan main channel messages
                     (let ((msgs (gethash chan-id taut-messages)))
                       (when msgs
                         (let ((m (cl-find ts msgs :key #'taut-message-ts :test #'equal)))
                           (when m
                             (setf (taut-message-text m) text)
                             (setq updated t)))))
                     ;; Scan threads
                     (unless updated
                       (maphash (lambda (_th-ts replies)
                                  (unless updated
                                    (let ((m (cl-find ts replies :key #'taut-message-ts :test #'equal)))
                                      (when m
                                        (setf (taut-message-text m) text)
                                        (setq updated t)))))
                                taut-threads))
                     (when updated
                       (taut-model-trigger-update))))))
              
              ;; 2. Skip boring system messages
              ((member subtype '("channel_join" "channel_leave" "channel_topic" "channel_purpose" "channel_name"))
               (message "Taut Socket: Skipped boring system message subtype: %s" subtype))
              
              ;; 3. Normal incoming message or other allowed subtypes (like file sharing, bot posts)
              (t
               (let* ((user-id (or (cdr (assoc 'user event)) (cdr (assoc 'bot_id event)) "unknown"))
                      (text (taut-api-unescape-html (taut-api--format-file-shares event (or (cdr (assoc 'text event)) ""))))
                      (ts (cdr (assoc 'ts event)))
                      (thread-ts (cdr (assoc 'thread_ts event))))
                 (message "Taut Socket: Incoming message on channel %s from user %s: %s"
                          chan-id user-id (substring text 0 (min (length text) 40)))
                 (when ts
                   (let ((is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) text)))
                     (taut-model-add-message
                      (make-taut-message
                       :id (concat "msg_" ts)
                       :channel-id chan-id
                       :user-id user-id
                       :text text
                       :ts ts
                       :thread-ts thread-ts
                       :reply-count 0
                       :is-unread t
                       :is-mention is-mention)))))))))
         
         ;; Handle added reaction
         ((string= event-type "reaction_added")
          (let* ((item (cdr (assoc 'item event)))
                 (item-type (cdr (assoc 'type item)))
                 (chan-id (cdr (assoc 'channel item)))
                 (ts (cdr (assoc 'ts item)))
                 (emoji (concat ":" (cdr (assoc 'reaction event)) ":"))
                 (user-id (cdr (assoc 'user event))))
            (message "Taut Socket: Reaction added %s by %s on message %s" emoji user-id ts)
            (when (string= item-type "message")
              (let* ((msgs (taut-model-get-messages chan-id))
                     (msg (cl-find ts msgs :key #'taut-message-ts :test #'equal)))
                (when msg
                  (let* ((reactions (taut-message-reactions msg))
                         (existing (assoc emoji reactions)))
                    (if existing
                        (unless (member user-id (cdr existing))
                          (setcdr existing (append (cdr existing) (list user-id))))
                      (setf (taut-message-reactions msg)
                            (append reactions (list (cons emoji (list user-id))))))
                    (taut-model-trigger-update)))))))
         
         ;; Handle removed reaction
         ((string= event-type "reaction_removed")
          (let* ((item (cdr (assoc 'item event)))
                 (item-type (cdr (assoc 'type item)))
                 (chan-id (cdr (assoc 'channel item)))
                 (ts (cdr (assoc 'ts item)))
                 (emoji (concat ":" (cdr (assoc 'reaction event)) ":"))
                 (user-id (cdr (assoc 'user event))))
            (message "Taut Socket: Reaction removed %s by %s on message %s" emoji user-id ts)
            (when (string= item-type "message")
              (let* ((msgs (taut-model-get-messages chan-id))
                     (msg (cl-find ts msgs :key #'taut-message-ts :test #'equal)))
                (when msg
                  (let* ((reactions (taut-message-reactions msg))
                         (existing (assoc emoji reactions)))
                    (when existing
                      (setcdr existing (delete user-id (cdr existing)))
                      (unless (cdr existing) ; if no users left for this reaction, prune it
                        (setf (taut-message-reactions msg) (assoc-delete-all emoji reactions)))
                      (taut-model-trigger-update))))))))))))))

;;;; Diagnostic Status Mode & Dashboard

(defvar taut-socket-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" #'taut-socket-connect-and-status)
    (define-key map "g" #'taut-socket-status)
    map)
  "Keymap for `taut-socket-status-mode'.")

(define-derived-mode taut-socket-status-mode special-mode "Taut Socket Status"
  "Major mode for displaying Taut Socket status and diagnostics."
  :group 'taut)

(defun taut-socket-connect-and-status ()
  "Reconnect the Taut WebSocket and refresh the status buffer."
  (interactive)
  (taut-socket-disconnect)
  (taut-socket-connect)
  ;; Wait briefly for socket connect to initiate, then refresh status
  (run-at-time 1 nil #'taut-socket-status))

;;;###autoload
(defun taut-socket-status ()
  "Display a detailed status and diagnostic report for the Taut Socket Client."
  (interactive)
  (let ((buf (get-buffer-create "*Taut Socket Status*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "============================================================\n")
        (insert "                  TAUT SOCKET MODE DIAGNOSTICS              \n")
        (insert "============================================================\n\n")
        
        ;; 1. Connection State
        (insert "1. Connection State:\n")
        (if (and taut-socket-ws (websocket-openp taut-socket-ws))
            (progn
              (insert "   - Status:      " (propertize "CONNECTED / OPEN" 'face 'font-lock-keyword-face) "\n")
              (insert "   - Remote URL:  " (websocket-url taut-socket-ws) "\n")
              (insert "   - Process:     " (format "%s" (websocket-conn taut-socket-ws)) "\n")
              (insert "   - Status line: " (format "%s" (process-status (websocket-conn taut-socket-ws))) "\n"))
          (insert "   - Status:      " (propertize "DISCONNECTED" 'face 'font-lock-warning-face) "\n"))
        (insert "\n")
        
        ;; 2. Auth Token Info
        (insert "2. Token Information:\n")
        (insert "   - Bot Token:   " (if (and (boundp 'taut-bot-token) taut-bot-token)
                                        (format "%s... (%s token)" 
                                                (substring taut-bot-token 0 (min 10 (length taut-bot-token)))
                                                (if (string-prefix-p "xoxp" taut-bot-token) "User" "Bot"))
                                      "NOT CONFIGURED") "\n")
        (insert "   - App Token:   " (if (and (boundp 'taut-app-token) taut-app-token)
                                        (format "%s... (App-level token)" 
                                                (substring taut-app-token 0 (min 10 (length taut-app-token))))
                                      "NOT CONFIGURED") "\n")
        (insert "\n")
        
        ;; 3. Traffic Statistics
        (insert "3. WebSocket Event Statistics:\n")
        (insert (format "   - Total Payloads Received: %d\n" taut-socket-events-count))
        (insert (format "   - Last Received Event:     %s (%s)\n" 
                        (or taut-socket-last-event-type "None")
                        (or taut-socket-last-event-ts "Never")))
        (insert "   - Received Events breakdown:\n")
        (if taut-socket-events-by-type
            (dolist (item taut-socket-events-by-type)
              (insert (format "     * %-15s: %d\n" (car item) (cdr item))))
          (insert "     * (No events received yet)\n"))
        (insert "\n")
        
        ;; 4. Diagnosis and Custom Recommendations
        (insert "4. Configuration Diagnosis:\n")
        (cond
         ((not (and taut-socket-ws (websocket-openp taut-socket-ws)))
          (insert "   - Warning: WebSocket is disconnected. Ensure your App Token starts with 'xapp-' and is correct.\n"))
         ((and (= taut-socket-events-count 1) (equal taut-socket-last-event-type "hello"))
          (insert "   - Diagnosis: The WebSocket successfully handshaked (received 'hello'), but Slack is not routing any events!\n")
          (insert "     This almost always means there is a subscription mismatch on the Slack Developer Console.\n\n")
          (if (and (boundp 'taut-bot-token) taut-bot-token (string-prefix-p "xoxp" taut-bot-token))
              (progn
                (insert "     [CRITICAL ADVICE FOR USER TOKENS ('xoxp-')]\n")
                (insert "     Because you are logged in using a User Token (your personal credentials):\n")
                (insert "     1. Go to your Slack App Settings: https://api.slack.com/apps\n")
                (insert "     2. Under 'Features' -> 'Event Subscriptions', scroll down to:\n")
                (insert "        'Subscribe to events on behalf of users' (NOT 'Subscribe to bot events')!\n")
                (insert "     3. Click 'Add Workspace User Event' and add the following:\n")
                (insert "        - message.channels\n")
                (insert "        - message.groups\n")
                (insert "        - message.im\n")
                (insert "        - message.mpim\n")
                (insert "        - reaction_added\n")
                (insert "        - reaction_removed\n")
                (insert "     4. Click 'Save Changes'.\n")
                (insert "     5. Reinstall the app to your workspace (using the alert banner at the top of the settings page).\n")
                (insert "     6. Restart Taut using M-x taut-quit and then M-x taut.\n"))
            (progn
              (insert "     [CRITICAL ADVICE FOR BOT TOKENS ('xoxb-')]\n")
              (insert "     Because you are logged in using a Bot Token:\n")
              (insert "     1. Under 'Features' -> 'Event Subscriptions', ensure the following events are added under 'Subscribe to bot events':\n")
              (insert "        - message.channels\n")
              (insert "        - message.groups\n")
              (insert "        - message.im\n")
              (insert "        - message.mpim\n")
              (insert "        - reaction_added\n")
              (insert "        - reaction_removed\n")
              (insert "     2. IMPORTANT: Bots can ONLY receive events for channels/DMs they are members of!\n")
              (insert "        Make sure you have invited your Bot to the channel/DM using '/invite @your_bot_name' in Slack.\n")
              (insert "     3. Restart Taut.\n"))))
         (t
          (insert "   - Status: Active stream receiving events correctly! Your live real-time sync is healthy.\n")))
        
        (insert "\n============================================================\n")
        (insert "Press 'q' to close this buffer. Press 'r' to reconnect WebSocket. Press 'g' to refresh.\n"))
      (set-buffer-modified-p nil)
      (taut-socket-status-mode))
    (pop-to-buffer buf)))

(provide 'taut-socket)
;;; taut-socket.el ends here

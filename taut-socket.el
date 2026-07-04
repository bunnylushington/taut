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
(require 'taut-model)
(require 'taut-api)

(defvar taut-socket-ws nil
  "The active WebSocket connection object.")

(defvar taut-socket-retry-timer nil
  "Timer used to schedule connection retries.")

;;;###autoload
(defun taut-socket-connect ()
  "Connect to Slack Socket Mode to receive real-time updates."
  (interactive)
  (unless taut-app-token
    (error "Taut: `taut-app-token' (starting with xapp-) must be configured for Socket Mode"))
  
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
                       (let* ((payload-str (websocket-frame-text frame))
                              ;; Standardize JSON parsing mapping false to nil
                              (json-object-type 'alist)
                              (json-array-type 'list)
                              (json-key-type 'symbol)
                              (json-false nil)
                              (json-null nil)
                              (data (json-read-from-string payload-str)))
                         (taut-socket--handle-payload ws data)))
         
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
    
    ;; 1. Send immediate acknowledgment back to Slack to avoid retry loops
    (when envelope-id
      (websocket-send-text ws (json-encode `((envelope_id . ,envelope-id)))))
    
    ;; 2. Dispatch events
    (cond
     ((string= type "hello")
      (message "Taut Socket: Handshake completed successfully."))
     
     ((string= type "events_api")
      (let* ((event (cdr (assoc 'event payload)))
             (event-type (cdr (assoc 'type event))))
        (cond
         ;; Handle incoming message
         ((string= event-type "message")
          (let* ((chan-id (cdr (assoc 'channel event)))
                 (user-id (cdr (assoc 'user event)))
                 (text (cdr (assoc 'text event)))
                 (ts (cdr (assoc 'ts event)))
                 (thread-ts (cdr (assoc 'thread_ts event)))
                 (subtype (cdr (assoc 'subtype event))))
            
            ;; Skip join/leave subtype messages for visual hygiene
            (unless (or subtype (null user-id))
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
                  :is-mention is-mention))))))
         
         ;; Handle added reaction
         ((string= event-type "reaction_added")
          (let* ((item (cdr (assoc 'item event)))
                 (item-type (cdr (assoc 'type item)))
                 (chan-id (cdr (assoc 'channel item)))
                 (ts (cdr (assoc 'ts item)))
                 (emoji (concat ":" (cdr (assoc 'reaction event)) ":"))
                 (user-id (cdr (assoc 'user event))))
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
                    (run-hooks 'taut-model-updated-hook)))))))
         
         ;; Handle removed reaction
         ((string= event-type "reaction_removed")
          (let* ((item (cdr (assoc 'item event)))
                 (item-type (cdr (assoc 'type item)))
                 (chan-id (cdr (assoc 'channel item)))
                 (ts (cdr (assoc 'ts item)))
                 (emoji (concat ":" (cdr (assoc 'reaction event)) ":"))
                 (user-id (cdr (assoc 'user event))))
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
                      (run-hooks 'taut-model-updated-hook))))))))))))))

(provide 'taut-socket)
;;; taut-socket.el ends here

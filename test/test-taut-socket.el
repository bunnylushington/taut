;;; test-taut-socket.el --- Unit tests for taut-socket.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; Unit tests for Taut Socket Mode Real-Time Client (taut-socket.el).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'taut-model)
(require 'taut-api)
(require 'taut-socket)

(ert-deftest taut-socket-retry-interval-test ()
  "Test calculating connection retry interval with backoff."
  (let ((taut-socket-retry-count 0)
        (taut-socket-min-retry-interval 2)
        (taut-socket-max-retry-interval 60))
    ;; For retry-count = 0
    (let ((interval (taut-socket-calculate-retry-interval)))
      (should (>= interval 2))
      (should (<= interval 4))) ; with jitter
    
    ;; For high retry-count, it should hit max-retry-interval
    (let* ((taut-socket-retry-count 10)
           (interval (taut-socket-calculate-retry-interval)))
      (should (>= interval 2))
      (should (<= interval 75)))))

(ert-deftest taut-socket-connect-test ()
  "Test connection initialization and Apps Connections Open request."
  (let ((taut-app-token "xapp-mock-token")
        (taut-socket-ws nil)
        (opened-url nil))
    (cl-letf (((symbol-function 'taut-api--request)
               (lambda (endpoint params method apptoken)
                 (should (equal endpoint "apps.connections.open"))
                 (should-not params)
                 (should (equal method "POST"))
                 (should apptoken)
                 '((ok . t)
                   (url . "wss://mock.slack.com/link"))))
              ((symbol-function 'websocket-open)
               (lambda (url &rest _args)
                 (setq opened-url url)
                 "mock-websocket-object")))
      
      (taut-socket-connect)
      (should (equal opened-url "wss://mock.slack.com/link"))
      (should (equal taut-socket-ws "mock-websocket-object")))))

(ert-deftest taut-socket-handle-handshake-and-ack-test ()
  "Test handling handshake hello and immediate envelope acknowledgments."
  (let ((taut-socket-retry-count 5)
        (sent-acks nil)
        (mock-ws "dummy-websocket"))
    (cl-letf (((symbol-function 'websocket-send-text)
               (lambda (ws text)
                 (should (eq ws mock-ws))
                 (push text sent-acks))))
      
      ;; 1. Handle Hello Handshake (No envelope ID, resets retry count)
      (taut-socket--handle-payload
       mock-ws
       '((type . "hello")))
      (should (= taut-socket-retry-count 0))
      (should-not sent-acks)
      
      ;; 2. Handle typical payload with an envelope_id (should acknowledge instantly)
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-12345")
         (type . "events_api")
         (payload . nil)))
      (should (equal (car sent-acks) "{\"envelope_id\":\"env-12345\"}")))))

(ert-deftest taut-socket-handle-message-events-test ()
  "Test dispatching normal, edited, and deleted messages."
  (taut-model-clear-all)
  (let ((mock-ws "dummy-websocket")
        (taut-current-user-id "U_ME"))
    (cl-letf (((symbol-function 'websocket-send-text)
               (lambda (_ws _text) nil)))
      
      ;; Set up active channel
      (taut-model-add-channel (make-taut-channel :id "C_SOCKET_DEV" :name "socket-dev" :type 'public))
      
      ;; 1. Incoming message event
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-1")
         (type . "events_api")
         (payload . ((event . ((type . "message")
                               (channel . "C_SOCKET_DEV")
                               (user . "U_ALICE")
                               (text . "Hello team! <@U_ME> look here.")
                               (ts . "1688500000.0001")))))))
      
      (let ((msg (taut-model-get-message-by-ts "1688500000.0001")))
        (should msg)
        (should (equal (taut-message-text msg) "Hello team! <@U_ME> look here."))
        (should (taut-message-is-unread msg))
        (should (taut-message-is-mention msg)))
      
      ;; 2. Edited message event (message_changed subtype)
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-2")
         (type . "events_api")
         (payload . ((event . ((type . "message")
                               (subtype . "message_changed")
                               (channel . "C_SOCKET_DEV")
                               (message . ((text . "Hello team! (Revised text)")
                                           (ts . "1688500000.0001")))))))))
      
      (let ((msg (taut-model-get-message-by-ts "1688500000.0001")))
        (should msg)
        (should (equal (taut-message-text msg) "Hello team! (Revised text)")))
      
      ;; 3. Deleted message event (message_deleted subtype)
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-3")
         (type . "events_api")
         (payload . ((event . ((type . "message")
                               (subtype . "message_deleted")
                               (channel . "C_SOCKET_DEV")
                               (deleted_ts . "1688500000.0001")))))))
      
      (let ((msg (taut-model-get-message-by-ts "1688500000.0001")))
        (should msg)
        (should (equal (taut-message-text msg) "_This message was deleted._")))
      
      ;; 4. Incoming message event with files
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-files-1")
         (type . "events_api")
         (payload . ((event . ((type . "message")
                               (channel . "C_SOCKET_DEV")
                               (user . "U_ALICE")
                               (text . "Here is a file.")
                               (ts . "1688500000.0002")
                               (files . (((name . "photo.png")
                                          (mimetype . "image/png")
                                          (url_private_download . "https://example.com/download/photo.png"))))))))))
      
      (let ((msg (taut-model-get-message-by-ts "1688500000.0002")))
        (should msg)
        (should (string-prefix-p "Here is a file." (taut-message-text msg)))
        (should (taut-message-files msg))
        (let ((f (car (taut-message-files msg))))
          (should (equal (cdr (assoc 'name f)) "photo.png"))
          (should (equal (cdr (assoc 'mimetype f)) "image/png"))
          (should (equal (cdr (assoc 'url_private_download f)) "https://example.com/download/photo.png")))))))

(ert-deftest taut-socket-handle-reaction-events-test ()
  "Test adding and removing reaction event dispatching."
  (taut-model-clear-all)
  (let ((mock-ws "dummy-websocket"))
    (cl-letf (((symbol-function 'websocket-send-text)
               (lambda (_ws _text) nil)))
      (taut-model-add-channel (make-taut-channel :id "C_SOCKET_DEV" :name "socket-dev" :type 'public))
      
      ;; Add original message
      (taut-model-add-message
       (make-taut-message
        :id "msg_1688510000.0001"
        :channel-id "C_SOCKET_DEV"
        :user-id "U_BOB"
        :text "Let's react to this."
        :ts "1688510000.0001"
        :reactions nil))
      
      ;; 1. Add reaction :rocket: from Alice
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-react-1")
         (type . "events_api")
         (payload . ((event . ((type . "reaction_added")
                               (reaction . "rocket")
                               (user . "U_ALICE")
                               (item . ((type . "message")
                                        (channel . "C_SOCKET_DEV")
                                        (ts . "1688510000.0001")))))))))
      
      (let* ((msg (taut-model-get-message-by-ts "1688510000.0001"))
             (reactions (taut-message-reactions msg)))
        (should reactions)
        (should (equal (assoc ":rocket:" reactions) '(":rocket:" "U_ALICE"))))
      
      ;; 2. Add same reaction :rocket: from Bob
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-react-2")
         (type . "events_api")
         (payload . ((event . ((type . "reaction_added")
                               (reaction . "rocket")
                               (user . "U_BOB")
                               (item . ((type . "message")
                                        (channel . "C_SOCKET_DEV")
                                        (ts . "1688510000.0001")))))))))
      
      (let* ((msg (taut-model-get-message-by-ts "1688510000.0001"))
             (reactions (taut-message-reactions msg)))
        (should reactions)
        (should (equal (assoc ":rocket:" reactions) '(":rocket:" "U_ALICE" "U_BOB"))))
      
      ;; 3. Remove reaction :rocket: from Alice
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-react-3")
         (type . "events_api")
         (payload . ((event . ((type . "reaction_removed")
                               (reaction . "rocket")
                               (user . "U_ALICE")
                               (item . ((type . "message")
                                        (channel . "C_SOCKET_DEV")
                                        (ts . "1688510000.0001")))))))))
      
      (let* ((msg (taut-model-get-message-by-ts "1688510000.0001"))
             (reactions (taut-message-reactions msg)))
        (should reactions)
        (should (equal (assoc ":rocket:" reactions) '(":rocket:" "U_BOB"))))
      
      ;; 4. Remove reaction :rocket: from Bob (should prune reaction entirely)
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-react-4")
         (type . "events_api")
         (payload . ((event . ((type . "reaction_removed")
                               (reaction . "rocket")
                               (user . "U_BOB")
                               (item . ((type . "message")
                                        (channel . "C_SOCKET_DEV")
                                        (ts . "1688510000.0001")))))))))
      
      (let* ((msg (taut-model-get-message-by-ts "1688510000.0001"))
             (reactions (taut-message-reactions msg)))
        (should-not reactions)))))

(ert-deftest taut-socket-handle-user-huddle-changed-test ()
  "Test handling 'user_huddle_changed' event toggling taut-user-is-huddling."
  (taut-model-clear-all)
  (let ((mock-ws "dummy-websocket")
        (user (make-taut-user :id "U_HUDDLER" :username "huddler")))
    (taut-model-add-user user)
    (should-not (taut-user-is-huddling user))
    
    (cl-letf (((symbol-function 'websocket-send-text)
               (lambda (_ws _text) nil)))
      ;; 1. Fire user_huddle_changed with is_in_huddle = t
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-huddle-1")
         (type . "events_api")
         (payload . ((event . ((type . "user_huddle_changed")
                               (user . ((id . "U_HUDDLER")
                                        (huddle_state . ((is_in_huddle . t)))))))))))
      
      (should (taut-user-is-huddling user))
      
      ;; 2. Fire user_huddle_changed with is_in_huddle = :json-false
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-huddle-2")
         (type . "events_api")
         (payload . ((event . ((type . "user_huddle_changed")
                               (user . ((id . "U_HUDDLER")
                                        (huddle_state . ((is_in_huddle . :json-false)))))))))))
      
      (should-not (taut-user-is-huddling user)))))

(ert-deftest taut-socket-handle-presence-change-test ()
  "Test handling 'presence_change' events for single users and multiple users."
  (taut-model-clear-all)
  (let ((mock-ws "dummy-websocket")
        (user1 (make-taut-user :id "U_ALICE" :username "alice" :presence 'offline))
        (user2 (make-taut-user :id "U_BOB" :username "bob" :presence 'offline)))
    (taut-model-add-user user1)
    (taut-model-add-user user2)
    (should (eq (taut-user-presence user1) 'offline))
    (should (eq (taut-user-presence user2) 'offline))

    (cl-letf (((symbol-function 'websocket-send-text)
               (lambda (_ws _text) nil))
              ((symbol-function 'taut-cache-save-user)
               (lambda (_user) nil)))
      
      ;; 1. Single user "U_ALICE" goes active (online)
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-presence-1")
         (type . "events_api")
         (payload . ((event . ((type . "presence_change")
                               (user . "U_ALICE")
                               (presence . "active")))))))
      (should (eq (taut-user-presence user1) 'online))
      (should (eq (taut-user-presence user2) 'offline))

      ;; 2. Single user "U_ALICE" goes away
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-presence-2")
         (type . "events_api")
         (payload . ((event . ((type . "presence_change")
                               (user . "U_ALICE")
                               (presence . "away")))))))
      (should (eq (taut-user-presence user1) 'away))

      ;; 3. Multiple users ["U_ALICE", "U_BOB"] both go active
      (taut-socket--handle-payload
       mock-ws
       '((envelope_id . "env-presence-3")
         (type . "events_api")
         (payload . ((event . ((type . "presence_change")
                               (users . ("U_ALICE" "U_BOB"))
                               (presence . "active")))))))
      (should (eq (taut-user-presence user1) 'online))
      (should (eq (taut-user-presence user2) 'online)))))

(ert-deftest taut-socket-notify-message-test ()
  "Test formatting and routing of incoming messages (echo area and optional OS notifications)."
  (taut-model-clear-all)
  (let ((alice (make-taut-user :id "U_ALICE" :username "alice"))
        (bob (make-taut-user :id "U_BOB" :username "bob"))
        (chan-pub (make-taut-channel :id "C_TEST" :name "test-channel" :type 'public))
        (chan-dm (make-taut-channel :id "D_BOB" :name "bob" :type 'dm))
        logged-messages
        alert-called)
    (taut-model-add-user alice)
    (taut-model-add-user bob)
    (taut-model-add-channel chan-pub)
    (taut-model-add-channel chan-dm)
    
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) logged-messages)))
              ((symbol-function 'require)
               (lambda (feature &rest args)
                 (if (eq feature 'alert)
                     t
                   (apply 'require feature args))))
              ((symbol-function 'alert)
               (lambda (text &rest args)
                 (setq alert-called (cons text args))
                 nil)))
      
      ;; 1. Standard channel message (no OS alert)
      (let ((taut-enable-os-notifications nil))
        (setq logged-messages nil
              alert-called nil)
        (taut-socket--notify-message "C_TEST" "U_ALICE" "Hello public channel!")
        (should (equal logged-messages '("Message on #test-channel from @alice: Hello public channel!")))
        (should-not alert-called))
      
      ;; 2. DM message (no OS alert)
      (let ((taut-enable-os-notifications nil))
        (setq logged-messages nil
              alert-called nil)
        (taut-socket--notify-message "D_BOB" "U_BOB" "Hi there in DM!")
        (should (equal logged-messages '("Message from @bob: Hi there in DM!")))
        (should-not alert-called))
      
      ;; 3. DM message with OS alert enabled
      (let ((taut-enable-os-notifications t))
        (setq logged-messages nil
              alert-called nil)
        (taut-socket--notify-message "D_BOB" "U_BOB" "This should trigger alert!")
        (should (equal logged-messages '("Message from @bob: This should trigger alert!")))
        (should (equal alert-called '("This should trigger alert!" :title "Message from @bob"))))
      
      ;; 4. Truncation test (> 40 chars)
      (let ((taut-enable-os-notifications t)
            (long-text "This is an extremely long message that should definitely be truncated because it exceeds the forty character limit"))
        (setq logged-messages nil
              alert-called nil)
        (taut-socket--notify-message "C_TEST" "U_ALICE" long-text)
        (should (equal logged-messages '("Message on #test-channel from @alice: This is an extremely long message that s [...]")))
        ;; Truncated text should be passed to alert as well
        (should (equal alert-called '("This is an extremely long message that s [...]" :title "Message on #test-channel from @alice")))))))

(provide 'test-taut-socket)
;;; test-taut-socket.el ends here

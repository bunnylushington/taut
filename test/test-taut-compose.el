;;; test-taut-compose.el --- Unit tests for taut-compose.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; Unit tests for Taut Message Composer logic (taut-compose.el).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'taut-compose)
(require 'taut-model)
(require 'taut-test-fixtures)

(ert-deftest taut-compose-typing-translation-test ()
  "Test on-the-fly emoticon to emoji translation in the compose buffer."
  (with-temp-buffer
    (taut-compose-mode)
    (insert "hello :")
    (insert ")")
    (taut-compose--post-self-insert)
    (should (equal (buffer-string) "hello 🙂")))
  
  ;; Boundary condition check
  (with-temp-buffer
    (taut-compose-mode)
    (insert "hello:")
    (insert ")")
    (taut-compose--post-self-insert)
    (should (equal (buffer-string) "hello:)"))))

(ert-deftest taut-compose-open-test ()
  "Test opening the compose buffer and setting correct properties."
  (taut-initialize-mock-data)
  (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &optional _action) buf)))
    ;; Test opening a new message
    (taut-compose-open "C_GENERAL")
    (with-current-buffer "*Taut Compose*"
      (should (equal taut-compose-channel-id "C_GENERAL"))
      (should-not taut-compose-thread-ts)
      (should-not taut-compose-edit-ts)
      (should (string-blank-p (buffer-string))))
    
    ;; Test opening a reply quote message
    (let ((msg (make-taut-message :id "msg-1" :channel-id "C_GENERAL" :user-id "U_ALICE" :text "Hello world" :ts "1688460000.0001")))
      (taut-compose-open "C_GENERAL" "1688460000.0001" msg)
      (with-current-buffer "*Taut Compose*"
        (should (equal taut-compose-channel-id "C_GENERAL"))
        (should (equal taut-compose-thread-ts "1688460000.0001"))
        (should-not taut-compose-edit-ts)
        (should (string-match-p "> \\*@alice wrote:\\*" (buffer-string)))
        (should (string-match-p "> Hello world" (buffer-string)))))

    ;; Test editing an existing message
    (taut-compose-open "C_GENERAL" nil nil "1688460000.0001" "My edit content")
    (with-current-buffer "*Taut Compose*"
      (should (equal taut-compose-channel-id "C_GENERAL"))
      (should-not taut-compose-thread-ts)
      (should (equal taut-compose-edit-ts "1688460000.0001"))
      (should (equal (buffer-string) "My edit content")))))

(ert-deftest taut-compose-send-test ()
  "Test sending composed message with mock/offline fallback."
  (let ((taut-bot-token nil) ; force mock offline fallback
        (taut-current-user-id "U_ME")
        (taut-message-refresh-called nil)
        (taut-thread-refresh-called nil))
    (cl-letf (((symbol-function 'taut-message-refresh) (lambda () (setq taut-message-refresh-called t)))
              ((symbol-function 'taut-thread-refresh) (lambda () (setq taut-thread-refresh-called t)))
              ((symbol-function 'pop-to-buffer) (lambda (buf &optional _action) buf)))
      
      ;; 1. Post new message
      (taut-initialize-mock-data)
      (taut-compose-open "C_GENERAL")
      (with-current-buffer "*Taut Compose*"
        (insert "This is a new message with smiley :)")
        (taut-compose-send))
      ;; Check message was added to the model
      (let* ((msgs (taut-model-get-messages "C_GENERAL"))
             (last-msg (car (last msgs))))
        (should (equal (taut-message-text last-msg) "This is a new message with smiley 🙂"))
        (should (equal (taut-message-user-id last-msg) "U_ME"))
        (should-not (taut-message-thread-ts last-msg))
        (should-not (taut-message-is-mention last-msg)))

      ;; 2. Edit existing message
      (taut-initialize-mock-data)
      (let ((edit-ts "1688450000.0001")) ; Bob's welcome message
        (taut-compose-open "C_GENERAL" nil nil edit-ts "Updated welcome text!")
        (with-current-buffer "*Taut Compose*"
          (taut-compose-send))
        (let ((edited-msg (taut-model-get-message-by-ts edit-ts)))
          (should (equal (taut-message-text edited-msg) "Updated welcome text!"))))

      ;; 3. Mention check
      (taut-initialize-mock-data)
      (taut-compose-open "C_GENERAL")
      (with-current-buffer "*Taut Compose*"
        (insert "Calling <@U_ME> look here!")
        (taut-compose-send))
      (let* ((msgs (taut-model-get-messages "C_GENERAL"))
             (last-msg (car (last msgs))))
        (should (equal (taut-message-text last-msg) "Calling <@U_ME> look here!"))
        (should (taut-message-is-mention last-msg))))))

(ert-deftest taut-compose-insert-helpers-test ()
  "Test composer formatting helpers."
  (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &optional _action) buf)))
    (taut-compose-open "C_GENERAL")
    (with-current-buffer "*Taut Compose*"
      ;; Insert code block
      (erase-buffer)
      (taut-compose-insert-code-block "python")
      (should (equal (buffer-string) "```python\n\n```"))

      ;; Insert link
      (erase-buffer)
      (taut-compose-insert-link "https://google.com" "Google")
      (should (equal (buffer-string) "<https://google.com|Google>")))))

(provide 'test-taut-compose)
;;; test-taut-compose.el ends here

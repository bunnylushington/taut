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
      (should (equal (buffer-string) "<https://google.com|Google>"))

      ;; Insert shell steps skeleton
      (erase-buffer)
      (taut-compose-insert-shell-steps-skeleton)
      (should (equal (buffer-string) "```bash\n# @taut-runnable\n\n```\n"))
      (should (= (point) (progn
                           (goto-char (point-min))
                           (search-forward "# @taut-runnable\n")
                           (point)))))))

(ert-deftest taut-compose-insert-reference-test ()
  "Test inserting a reference from the ring into the compose buffer."
  (let ((taut-message-reference-ring nil))
    ;; 1. If the ring is empty, check it displays warning and does not insert anything
    (with-temp-buffer
      (taut-compose-mode)
      (let ((msg-displayed nil))
        (cl-letf (((symbol-function 'message) (lambda (format-str &rest _args)
                                                (when (and (stringp format-str)
                                                           (string-prefix-p "Taut: Reference ring is empty" format-str))
                                                  (setq msg-displayed t)))))
          (taut-compose-insert-reference)
          (should msg-displayed)
          (should (string-blank-p (buffer-string)))))))

  ;; 2. If the ring has candidates, verify completing-read is called and selected URL is inserted
  (let ((taut-message-reference-ring
         '((:channel-id "C_DEV"
            :channel-name "development"
            :ts "1688460000.0001"
            :author "alice"
            :snippet "Hey team!"
            :url "https://T_MY_TEAM.slack.com/archives/C_DEV/p16884600000001"))))
    (with-temp-buffer
      (taut-compose-mode)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt candidates &optional _predicate _require-match _initial-input _hist _def _inherit-input-method)
                   ;; Return the display string of the single candidate
                   (car (car candidates)))))
        (taut-compose-insert-reference)
        (should (equal (buffer-string) "https://T_MY_TEAM.slack.com/archives/C_DEV/p16884600000001"))))))

(ert-deftest taut-compose-test-bounds-user ()
  "Test parsing of user mentions bounds."
  (with-temp-buffer
    (taut-compose-mode)
    (insert "hello @al")
    (let ((bounds (taut-compose--capf-bounds)))
      (should bounds)
      (should (eq (plist-get bounds :type) 'user))
      (should (= (plist-get bounds :start) 7))
      (should (= (plist-get bounds :end) 10)))))

(ert-deftest taut-compose-test-bounds-channel ()
  "Test parsing of channel bounds."
  (with-temp-buffer
    (taut-compose-mode)
    (insert "check #gen")
    (let ((bounds (taut-compose--capf-bounds)))
      (should bounds)
      (should (eq (plist-get bounds :type) 'channel))
      (should (= (plist-get bounds :start) 7))
      (should (= (plist-get bounds :end) 11)))))

(ert-deftest taut-compose-test-bounds-emoji ()
  "Test parsing of emoji bounds."
  (with-temp-buffer
    (taut-compose-mode)
    (insert "feeling :smi")
    (let ((bounds (taut-compose--capf-bounds)))
      (should bounds)
      (should (eq (plist-get bounds :type) 'emoji))
      (should (= (plist-get bounds :start) 9))
      (should (= (plist-get bounds :end) 13)))))

(ert-deftest taut-compose-test-bounds-invalid ()
  "Test that spaces or invalid prefix triggers are not completed."
  (with-temp-buffer
    (taut-compose-mode)
    (insert "hello @ alice")
    (should-not (taut-compose--capf-bounds)))
  (with-temp-buffer
    (taut-compose-mode)
    (insert "email@domain")
    (should-not (taut-compose--capf-bounds))))

(ert-deftest taut-compose-test-capf-candidates ()
  "Test that Capf returns correct collection of candidates."
  (taut-model-clear-all)
  (taut-model-add-user (make-taut-user :id "U_TEST" :username "testuser" :real-name "Test User"))
  (taut-model-add-channel (make-taut-channel :id "C_TEST" :name "testchannel" :type 'public))
  
  (with-temp-buffer
    (taut-compose-mode)
    (insert "@")
    (let ((capf-res (taut-compose-capf)))
      (should capf-res)
      (let ((collection (nth 2 capf-res)))
        (should (member "@testuser" collection)))))
  
  (with-temp-buffer
    (taut-compose-mode)
    (insert "#")
    (let ((capf-res (taut-compose-capf)))
      (should capf-res)
      (let ((collection (nth 2 capf-res)))
        (should (member "#testchannel" collection)))))

  (clrhash taut-custom-emojis)
  (puthash "super-custom-emoji" "https://example.com/custom.png" taut-custom-emojis)
  (with-temp-buffer
    (taut-compose-mode)
    (insert ":")
    (let ((capf-res (taut-compose-capf)))
      (should capf-res)
      (let ((collection (nth 2 capf-res)))
        (should (member ":smile:" collection))
        (should (member ":super-custom-emoji:" collection))))))

(ert-deftest taut-compose-test-capf-exit-function ()
  "Test that the exit-function successfully applies Slack markup text properties."
  (taut-model-clear-all)
  (taut-model-add-user (make-taut-user :id "U_TEST" :username "testuser" :real-name "Test User"))
  (taut-model-add-channel (make-taut-channel :id "C_TEST" :name "testchannel" :type 'public))
  
  (with-temp-buffer
    (taut-compose-mode)
    (insert "@testuser")
    (let* ((capf-res (taut-compose-capf))
           (exit-fn (plist-get (nthcdr 3 capf-res) :exit-function)))
      (should exit-fn)
      (funcall exit-fn "@testuser" 'finished)
      (should (equal (buffer-string) "@testuser"))
      (should (equal (get-text-property 1 'taut-compose-markup) "<@U_TEST|testuser>"))
      (should (equal (taut-compose--get-text-with-markup) "<@U_TEST|testuser>"))))

  (with-temp-buffer
    (taut-compose-mode)
    (insert "#testchannel")
    (let* ((capf-res (taut-compose-capf))
           (exit-fn (plist-get (nthcdr 3 capf-res) :exit-function)))
      (should exit-fn)
      (funcall exit-fn "#testchannel" 'finished)
      (should (equal (buffer-string) "#testchannel"))
      (should (equal (get-text-property 1 'taut-compose-markup) "<#C_TEST|testchannel>"))
      (should (equal (taut-compose--get-text-with-markup) "<#C_TEST|testchannel>")))))

(ert-deftest taut-compose-test-annotations ()
  "Test Corfu annotation support in Capf."
  (taut-model-clear-all)
  (taut-model-add-user (make-taut-user :id "U_TEST" :username "testuser" :real-name "Test User"))
  (taut-model-add-channel (make-taut-channel :id "C_TEST" :name "testchannel" :type 'public :topic "Channel topic"))
  
  (with-temp-buffer
    (taut-compose-mode)
    (insert "@")
    (let* ((capf-res (taut-compose-capf))
           (annotation-fn (plist-get (nthcdr 3 capf-res) :annotation-function)))
      (should annotation-fn)
      (should (equal (funcall annotation-fn "@testuser") "  (Test User)"))))

  (with-temp-buffer
    (taut-compose-mode)
    (insert "#")
    (let* ((capf-res (taut-compose-capf))
           (annotation-fn (plist-get (nthcdr 3 capf-res) :annotation-function)))
      (should annotation-fn)
      (should (equal (funcall annotation-fn "#testchannel") "  [Channel topic]"))))

  (clrhash taut-custom-emojis)
  (puthash "super-custom-emoji" "https://example.com/custom.png" taut-custom-emojis)
  (with-temp-buffer
    (taut-compose-mode)
    (insert ":")
    (let* ((capf-res (taut-compose-capf))
           (annotation-fn (plist-get (nthcdr 3 capf-res) :annotation-function)))
      (should annotation-fn)
      ;; smile is standard, taut-emoji-translate translates it to emoji
      (should (string-match-p "  " (funcall annotation-fn ":smile:")))
      (should (equal (funcall annotation-fn ":super-custom-emoji:") "  [custom]")))))

(ert-deftest taut-compose-from-atuin-history-test ()
  "Test taut-compose-from-atuin-history."
  ;; 1. Test when Atuin binary is not found / not executable
  (cl-letf (((symbol-function 'executable-find) (lambda (_bin) nil))
            ((symbol-function 'file-executable-p) (lambda (_path) nil)))
    (with-temp-buffer
      (taut-compose-mode)
      (should-error (taut-compose-from-atuin-history) :type 'user-error)))

  ;; 2. Test when Atuin binary is found and we complete some selections
  (let ((prompt-count 0))
    (cl-letf (((symbol-function 'executable-find) (lambda (_bin) "/usr/bin/atuin"))
              ((symbol-function 'file-executable-p) (lambda (_path) t))
              ((symbol-function 'shell-command-to-string) (lambda (_cmd) "ls -la\ngit status\nssh machine\n"))
              ((symbol-function 'completing-read)
               (lambda (_prompt _collection &optional _predicate _require-match _initial-input _hist _def _inherit-input-method)
                 (setq prompt-count (1+ prompt-count))
                 (cond
                  ((= prompt-count 1) "git status")
                  ((= prompt-count 2) "ls -la")
                  (t "")))))
      (with-temp-buffer
        (taut-compose-mode)
        (taut-compose-from-atuin-history)
        (should (equal (buffer-string) "```bash\n# @taut-runnable\ngit status\nls -la\n```\n"))))))

(provide 'test-taut-compose)
;;; test-taut-compose.el ends here

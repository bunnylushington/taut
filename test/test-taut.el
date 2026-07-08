;;; test-taut.el --- Unit tests for taut.el global commands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; Unit tests for Taut global commands (taut-send-region, taut-send-buffer, etc.).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'taut)
(require 'taut-model)
(require 'taut-test-fixtures)

(ert-deftest taut-detect-language-for-mode-test ()
  "Test that major-mode to code language string mapping works robustly."
  ;; Known mappings in alist
  (should (equal (taut--detect-language-for-mode 'emacs-lisp-mode) "elisp"))
  (should (equal (taut--detect-language-for-mode 'python-mode) "python"))
  (should (equal (taut--detect-language-for-mode 'sh-mode) "bash"))
  
  ;; Unknown mode fallback (should strip "-mode" suffix)
  (should (equal (taut--detect-language-for-mode 'rust-mode) "rust"))
  (should (equal (taut--detect-language-for-mode 'lisp-mode) "lisp"))
  (should (equal (taut--detect-language-for-mode 'fundamental-mode) "fundamental")))

(ert-deftest taut-select-recipient-test ()
  "Test interactive recipient selection via completing-read."
  (taut-initialize-mock-data)
  (let ((completing-read-called nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &optional _predicate _require-match _initial-input _hist _def _inherit-input-method)
                 (setq completing-read-called t)
                 (should (string-match-p "Channel/DM" prompt))
                 ;; Select Bob's DM channel
                 (car (cl-find-if (lambda (cand)
                                    (string-match-p "bob" (car cand)))
                                  collection)))))
      (let ((selected-id (taut-select-recipient)))
        (should completing-read-called)
        (should (equal selected-id "C_BOB_DM"))))))

(ert-deftest taut-send-region-test ()
  "Test sending selected region as a code block."
  (taut-initialize-mock-data)
  (let ((taut-bot-token nil) ; Force mock fallback
        (taut-current-user-id "U_ME")
        (refreshed-buffers nil))
    (cl-letf (((symbol-function 'taut-message-refresh)
               (lambda () (push (buffer-name) refreshed-buffers)))
              ((symbol-function 'taut-select-recipient)
               (lambda (&optional _prompt) "C_GENERAL")))
      
      (with-temp-buffer
        (insert "def hello_world():\n    print(\"Hello, Taut!\")\n")
        (python-mode)
        ;; Send the region
        (taut-send-region (point-min) (point-max))
        
        ;; Verify message was added to General channel
        (let* ((msgs (taut-model-get-messages "C_GENERAL"))
               (sent-msg (car (last msgs))))
          (should sent-msg)
          (should (equal (taut-message-user-id sent-msg) "U_ME"))
          (should (equal (taut-message-text sent-msg)
                         "```python\ndef hello_world():\n    print(\"Hello, Taut!\")\n\n```")))))))

(ert-deftest taut-send-buffer-test ()
  "Test sending the active buffer as a file snippet."
  (taut-initialize-mock-data)
  (let ((taut-bot-token nil) ; Force mock fallback
        (taut-current-user-id "U_ME")
        (refreshed-buffers nil))
    (cl-letf (((symbol-function 'taut-message-refresh)
               (lambda () (push (buffer-name) refreshed-buffers)))
              ((symbol-function 'taut-select-recipient)
               (lambda (&optional _prompt) "C_DEV")))
      
      ;; Create a "dirty" dummy buffer (not saved to disk)
      (let ((buf (generate-new-buffer "my-script.sh")))
        (with-current-buffer buf
          (shell-script-mode)
          (insert "#!/bin/bash\necho \"dirty state buffer contents\"\n")
          
          (unwind-protect
              (progn
                (taut-send-buffer)
                
                ;; Verify message was added to C_DEV
                (let* ((msgs (taut-model-get-messages "C_DEV"))
                       (sent-msg (car (last msgs))))
                  (should sent-msg)
                  (should (equal (taut-message-user-id sent-msg) "U_ME"))
                  (should (equal (taut-message-text sent-msg) "Shared a file: my-script.sh"))
                  
                  ;; Verify file attachment properties
                  (let* ((files (taut-message-files sent-msg))
                         (file-att (car files)))
                    (should (equal (cdr (assoc 'name file-att)) "my-script.sh"))
                    (should (equal (cdr (assoc 'mimetype file-att)) "text/x-sh"))
                    
                    ;; Verify that the local cache file exists and contains the dirty buffer content
                    (let* ((url (cdr (assoc 'url_private_download file-att)))
                           (local-path (taut-media-file-path url)))
                      (should (file-exists-p local-path))
                      (should (equal (taut-message--read-file-string local-path)
                                     "#!/bin/bash\necho \"dirty state buffer contents\"\n"))
                      
                      ;; Cleanup local mock cache file
                      (delete-file local-path)))))
            ;; Cleanup buffer
            (kill-buffer buf)))))))

(provide 'test-taut)
;;; test-taut.el ends here

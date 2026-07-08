;;; test-taut-shell-steps.el --- Tests for interactive shell steps panel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us

;;; Code:

(require 'ert)
(require 'taut-shell-steps)

(ert-deftest taut-shell-steps-initialization-and-rendering-test ()
  "Test that taut-shell-steps initializes and renders correctly."
  (let ((cmds '("git status" "npm install" "npm run test"))
        (temp-dir (make-temp-file "taut-steps-test-" t)))
    (unwind-protect
        (save-window-excursion
          (taut-shell-steps-open cmds temp-dir)
          (let ((buf (get-buffer "*Taut Shell Steps*")))
            (should buf)
            (with-current-buffer buf
              ;; Verify buffer-local variables are set
              (should (equal taut-shell-steps-directory (expand-file-name temp-dir)))
              (should (= (length taut-shell-steps-data) 3))
              (should (equal (plist-get (car taut-shell-steps-data) :cmd) "git status"))
              
              ;; Verify rendered layout contains header and steps
              (let ((rendered (buffer-string)))
                (should (string-match-p "🚀 TAUT INTERACTIVE SHELL STEPS PANEL" rendered))
                (should (string-match-p "Execution Directory:" rendered))
                (should (string-match-p "\\[1\\]" rendered))
                (should (string-match-p "git status" rendered))
                (should (string-match-p "\\[2\\]" rendered))
                (should (string-match-p "npm install" rendered))
                (should (string-match-p "\\[3\\]" rendered))
                (should (string-match-p "npm run test" rendered))
                (should (string-match-p "\\[Run\\]" rendered))
                (should (string-match-p "\\[Edit\\]" rendered))
                (should (string-match-p "\\[Del\\]" rendered)))
              
              ;; Clean up
              (kill-buffer buf))))
      (delete-directory temp-dir))))

(ert-deftest taut-shell-steps-manipulation-test ()
  "Test step editing, adding, deleting, and re-ordering."
  (let ((cmds '("echo foo" "echo bar")))
    (save-window-excursion
      (taut-shell-steps-open cmds)
      (let ((buf (get-buffer "*Taut Shell Steps*")))
        (should buf)
        (with-current-buffer buf
          ;; Test Editing step 1 in-place (mocking read-string)
          (cl-letf (((symbol-function 'read-string) (lambda (_prompt initial) (concat initial " edited"))))
            (taut-shell-steps-edit-idx 1)
            (should (equal (plist-get (car taut-shell-steps-data) :cmd) "echo foo edited"))
            (should (string-match-p "echo foo edited" (buffer-string))))

          ;; Test Adding step (mocking read-string)
          (cl-letf (((symbol-function 'read-string) (lambda (_prompt) "echo baz")))
            (taut-shell-steps-add-step)
            (should (= (length taut-shell-steps-data) 3))
            (should (equal (plist-get (nth 2 taut-shell-steps-data) :cmd) "echo baz")))

          ;; Test Deleting step (mocking yes-or-no-p)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
            (taut-shell-steps-delete-idx 2) ; delete "echo bar"
            (should (= (length taut-shell-steps-data) 2))
            (should (equal (plist-get (car taut-shell-steps-data) :cmd) "echo foo edited"))
            ;; Check that step indexing is normalized back to consecutive integers (1, 2)
            (should (= (plist-get (car taut-shell-steps-data) :idx) 1))
            (should (= (plist-get (cadr taut-shell-steps-data) :idx) 2))
            (should (equal (plist-get (cadr taut-shell-steps-data) :cmd) "echo baz")))

          (kill-buffer buf))))))

(ert-deftest taut-shell-steps-reset-test ()
  "Test resetting all step statuses back to Pending and restoring directory."
  (let* ((cmds '("echo foo" "echo bar"))
         (initial-dir (make-temp-file "taut-steps-reset-initial-" t))
         (new-dir (make-temp-file "taut-steps-reset-new-" t)))
    (unwind-protect
        (save-window-excursion
          (taut-shell-steps-open cmds initial-dir)
          (let ((buf (get-buffer "*Taut Shell Steps*")))
            (should buf)
            (with-current-buffer buf
              ;; Artificially change directory and set statuses
              (setq taut-shell-steps-directory new-dir)
              (setq taut-shell-steps-data
                    (mapcar (lambda (s)
                              (plist-put s :status "Success"))
                            taut-shell-steps-data))
              (taut-shell-steps-render)

              (should (equal taut-shell-steps-directory new-dir))
              (should (equal (plist-get (car taut-shell-steps-data) :status) "Success"))

              ;; Trigger reset
              (taut-shell-steps-reset)

              ;; Verify statuses and directory are restored
              (should (equal taut-shell-steps-directory (expand-file-name initial-dir)))
              (should (equal (plist-get (car taut-shell-steps-data) :status) "Pending"))
              (should (equal (plist-get (cadr taut-shell-steps-data) :status) "Pending"))

              (kill-buffer buf))))
      (delete-directory initial-dir t)
      (delete-directory new-dir t))))

(ert-deftest taut-shell-steps-change-dir-create-test ()
  "Test that changing to a non-existent directory prompts for creation."
  (let* ((cmds '("echo foo"))
         (temp-base (make-temp-file "taut-steps-cd-test-" t))
         (non-existent-dir (expand-file-name "nested/subdir/here" temp-base)))
    (unwind-protect
        (save-window-excursion
          (taut-shell-steps-open cmds temp-base)
          (let ((buf (get-buffer "*Taut Shell Steps*")))
            (should buf)
            (with-current-buffer buf
              ;; Mock read-directory-name to return our non-existent directory
              ;; Mock yes-or-no-p to return t (approving creation)
              (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) non-existent-dir))
                        ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (taut-shell-steps-change-dir)
                ;; Verify that the directory was created and directory was updated
                (should (file-directory-p non-existent-dir))
                (should (equal taut-shell-steps-directory non-existent-dir))))
            (kill-buffer buf)))
      (delete-directory temp-base t))))

(provide 'test-taut-shell-steps)
;;; test-taut-shell-steps.el ends here

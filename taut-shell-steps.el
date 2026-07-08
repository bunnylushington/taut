;;; taut-shell-steps.el --- Interactive steps management panel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us
;; Keywords: comm, tools

;;; Commentary:
;; This file implements a dedicated, premium major-mode panel `taut-shell-steps-mode`
;; that displays a beautiful, box-drawn table of steps for a runnable shell block.
;; Users can modify commands, change target execution directories, and execute
;; steps individually or sequentially in a fully interactive, self-contained way.

;;; Code:

(require 'cl-lib)

(defgroup taut-shell-steps nil
  "Interactive shell steps panel for Taut."
  :group 'taut)

(defface taut-shell-steps-header
  '((((background dark))  :foreground "#ff8c00" :weight bold)
    (((background light)) :foreground "#e67e22" :weight bold))
  "Face for steps panel headers.")

(defface taut-shell-steps-status-pending
  '((t :foreground "#8a8a8a"))
  "Face for pending status.")

(defface taut-shell-steps-status-running
  '((t :foreground "#f1c40f" :weight bold))
  "Face for running status.")

(defface taut-shell-steps-status-success
  '((t :foreground "#2ecc71" :weight bold))
  "Face for success status.")

(defface taut-shell-steps-status-failed
  '((t :foreground "#e74c3c" :weight bold))
  "Face for failed status.")

(defvar-local taut-shell-steps-data nil
  "List of plists representing steps: (:idx 1 :cmd \"git status\" :status \"Pending\").")

(defvar-local taut-shell-steps-directory nil
  "The directory in which steps will be run.")

(defvar taut-shell-steps-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "q" #'quit-window)
    (define-key map "c" #'taut-shell-steps-change-dir)
    (define-key map "e" #'taut-shell-steps-edit-at-point)
    (define-key map "r" #'taut-shell-steps-run-at-point)
    (define-key map "a" #'taut-shell-steps-add-step)
    (define-key map "d" #'taut-shell-steps-delete-at-point)
    (define-key map "u" #'taut-shell-steps-move-up-at-point)
    (define-key map "o" #'taut-shell-steps-move-down-at-point)
    (define-key map "j" #'next-line)
    (define-key map "k" #'previous-line)
    (define-key map "g" #'taut-shell-steps-reset)
    (define-key map "R" #'taut-shell-steps-run-all)
    map)
  "Keymap for `taut-shell-steps-mode'.")

(defun taut-shell-steps--trim (str)
  "Trim leading and trailing whitespace from STR."
  (if str
      (replace-regexp-in-string "\\`[ \t\n\r]+" "" (replace-regexp-in-string "[ \t\n\r]+\\'" "" str))
    ""))

(defun taut-shell-steps--handle-cd (cmd)
  "If CMD is a cd command, update `taut-shell-steps-directory' and return the new path, else nil."
  (let ((trimmed (taut-shell-steps--trim cmd)))
    (when (string-match "^cd\\s-+\\(.+\\)$" trimmed)
      (let* ((raw-path (match-string 1 trimmed))
             (clean-path (if (and (string-prefix-p "\"" raw-path) (string-suffix-p "\"" raw-path))
                             (substring raw-path 1 (1- (length raw-path)))
                           (if (and (string-prefix-p "'" raw-path) (string-suffix-p "'" raw-path))
                               (substring raw-path 1 (1- (length raw-path)))
                             raw-path)))
             (substituted (substitute-in-file-name clean-path))
             (new-dir (expand-file-name substituted taut-shell-steps-directory)))
        new-dir))))

;;;###autoload
(defun taut-shell-steps-open (commands &optional initial-dir)
  "Open the interactive Taut Shell Steps panel with COMMANDS in INITIAL-DIR."
  (interactive)
  (let* ((buf (get-buffer-create "*Taut Shell Steps*"))
         (dir (or initial-dir default-directory "~/")))
    (with-current-buffer buf
      (taut-shell-steps-mode)
      (setq taut-shell-steps-directory (expand-file-name dir))
      ;; Initialize data structure
      (setq taut-shell-steps-data nil)
      (let ((idx 1))
        (dolist (cmd commands)
          (push (list :idx idx :cmd cmd :status "Pending") taut-shell-steps-data)
          (setq idx (1+ idx))))
      (setq taut-shell-steps-data (nreverse taut-shell-steps-data))
      (taut-shell-steps-render))
    (select-window (display-buffer buf))))

(defun taut-shell-steps-render ()
  "Render the full steps table with borders and interactive text properties."
  (let ((inhibit-read-only t)
        (saved-point (point)))
    (erase-buffer)
    
    ;; Boxless Header with clean spacing
    (insert (propertize "🚀 TAUT INTERACTIVE SHELL STEPS PANEL\n" 'face 'taut-shell-steps-header))
    
    ;; Directory line
    (let* ((dir-label "📂 Execution Directory: ")
           (dir-path (abbreviate-file-name taut-shell-steps-directory))
           (dir-btn " [Change] "))
      (insert (propertize dir-label 'face '(:weight bold))
              (propertize dir-path 'face '(:foreground "#3a86ff"))
              (propertize dir-btn
                          'face '(:weight bold :foreground "#ff8c00")
                          'mouse-face 'highlight
                          'help-echo "Click to change execution directory"
                          'keymap (let ((m (make-sparse-keymap)))
                                    (define-key m [mouse-1] #'taut-shell-steps-change-dir)
                                    (define-key m (kbd "RET") #'taut-shell-steps-change-dir)
                                    m))
              "\n"))
    
    ;; Actions help line
    (let* ((help-str "💡 [r/click] Run  [e] Edit  [a] Add  [d] Delete  [u/o] Move Up/Dn  [g] Reset  [R] Run All  [q] Quit"))
      (insert (propertize help-str 'face '(:slant italic :foreground "#8a8a8a")) "\n\n"))
    
    ;; Table Headers
    ;; Columns: No (5), Command (fill), Status (12), Action (22)
    (let* ((width 90)
           (col-no-w 5)
           (col-status-w 12)
           (col-action-w 22)
           ;; Command column width takes remaining space exactly
           (col-cmd-w (- width col-no-w col-status-w col-action-w 3))
           (border-top (concat "┌" (make-string col-no-w ?─) "┬" (make-string col-cmd-w ?─) "┬" (make-string col-status-w ?─) "┬" (make-string col-action-w ?─) "┐\n"))
           (border-mid (concat "├" (make-string col-no-w ?─) "┼" (make-string col-cmd-w ?─) "┼" (make-string col-status-w ?─) "┼" (make-string col-action-w ?─) "┤\n"))
           (border-bot (concat "└" (make-string col-no-w ?─) "┴" (make-string col-cmd-w ?─) "┴" (make-string col-status-w ?─) "┴" (make-string col-action-w ?─) "┘\n")))
      
      (insert border-top)
      ;; Column headers
      (insert "│" (format " %-3s " "No.") "│" (format (format " %%-%ds " (- col-cmd-w 2)) "Command") "│" (format " %-10s " "Status") "│" (format (format " %%-%ds " (- col-action-w 2)) "Actions") "│\n")
      (insert border-mid)
      
      ;; Render each step row
      (dolist (step taut-shell-steps-data)
        (let* ((idx (plist-get step :idx))
               (cmd (plist-get step :cmd))
               (status (plist-get step :status))
               
               ;; Format step number
               (no-str (format " [%d] " idx))
               
               ;; Truncate/format command text
               (cmd-disp (if (> (string-width cmd) (- col-cmd-w 2))
                             (concat (substring cmd 0 (- col-cmd-w 5)) "...")
                           cmd))
               (cmd-str (format (format " %%-%ds " (- col-cmd-w 2)) cmd-disp))
               
               ;; Format status with face
               (status-face (cond
                             ((equal status "Pending") 'taut-shell-steps-status-pending)
                             ((equal status "Running") 'taut-shell-steps-status-running)
                             ((equal status "Success") 'taut-shell-steps-status-success)
                             ((equal status "Failed") 'taut-shell-steps-status-failed)
                             (t 'default)))
               (status-str (propertize (format " %-10s " status) 'face status-face))
               
               ;; Action buttons text
               (run-btn (propertize "[Run]"
                                    'face '(:weight bold :foreground "#2ecc71")
                                    'mouse-face 'highlight
                                    'help-echo "Run this step"
                                    'taut-step-idx idx
                                    'keymap (let ((m (make-sparse-keymap)))
                                              (define-key m [mouse-1] #'taut-shell-steps-run-click)
                                              (define-key m (kbd "RET") #'taut-shell-steps-run-click)
                                              m)))
               (edit-btn (propertize "[Edit]"
                                     'face '(:weight bold :foreground "#3498db")
                                     'mouse-face 'highlight
                                     'help-echo "Edit this step"
                                     'taut-step-idx idx
                                     'keymap (let ((m (make-sparse-keymap)))
                                               (define-key m [mouse-1] #'taut-shell-steps-edit-click)
                                               (define-key m (kbd "RET") #'taut-shell-steps-edit-click)
                                               m)))
               (del-btn (propertize "[Del]"
                                    'face '(:weight bold :foreground "#e74c3c")
                                    'mouse-face 'highlight
                                    'help-echo "Delete this step"
                                    'taut-step-idx idx
                                    'keymap (let ((m (make-sparse-keymap)))
                                              (define-key m [mouse-1] #'taut-shell-steps-delete-click)
                                              (define-key m (kbd "RET") #'taut-shell-steps-delete-click)
                                              m)))
               (action-str (concat " " run-btn " " edit-btn " " del-btn)))
          
          ;; Insert row with cells separated by │
          (let ((row-start (point)))
            (insert "│" no-str "│" cmd-str "│" status-str "│" (format (format " %%-%ds " (- col-action-w 2)) action-str) "│\n")
            ;; Put some row-level property to help point-based actions
            (add-text-properties row-start (point) (list 'taut-step-idx idx)))))
      
      (insert border-bot))
    (goto-char (max (point-min) (min saved-point (point-max))))))

(defun taut-shell-steps-run-click ()
  "Click/RET handler for running a step."
  (interactive)
  (let ((idx (get-text-property (point) 'taut-step-idx)))
    (when idx
      (taut-shell-steps-run-idx idx))))

(defun taut-shell-steps-edit-click ()
  "Click/RET handler for editing a step."
  (interactive)
  (let ((idx (get-text-property (point) 'taut-step-idx)))
    (when idx
      (taut-shell-steps-edit-idx idx))))

(defun taut-shell-steps-delete-click ()
  "Click/RET handler for deleting a step."
  (interactive)
  (let ((idx (get-text-property (point) 'taut-step-idx)))
    (when idx
      (taut-shell-steps-delete-idx idx))))

(defun taut-shell-steps-run-at-point ()
  "Run the step under point."
  (interactive)
  (let ((idx (get-text-property (point) 'taut-step-idx)))
    (if idx
        (taut-shell-steps-run-idx idx)
      (message "No step under point."))))

(defun taut-shell-steps-edit-at-point ()
  "Edit the step under point."
  (interactive)
  (let ((idx (get-text-property (point) 'taut-step-idx)))
    (if idx
        (taut-shell-steps-edit-idx idx)
      (message "No step under point."))))

(defun taut-shell-steps-delete-at-point ()
  "Delete the step under point."
  (interactive)
  (let ((idx (get-text-property (point) 'taut-step-idx)))
    (if idx
        (taut-shell-steps-delete-idx idx)
      (message "No step under point."))))

(defun taut-shell-steps-run-idx (idx)
  "Asynchronously execute the command for step IDX in `taut-shell-steps-directory`."
  (let ((step (cl-find-if (lambda (s) (= (plist-get s :idx) idx)) taut-shell-steps-data)))
    (when step
      (let* ((cmd (plist-get step :cmd))
             (cd-dir (taut-shell-steps--handle-cd cmd)))
        (if cd-dir
            (progn
              (setq taut-shell-steps-directory cd-dir)
              (taut-shell-steps--update-status idx "Success")
              (message "Changed directory to %s" cd-dir))
          (let* ((dir taut-shell-steps-directory)
                 (buf (current-buffer))
                 ;; Mark as Running
                 (_ (taut-shell-steps--update-status idx "Running"))
                 ;; Run asynchronously
                 (proc (let ((default-directory dir))
                         (taut-runnable-cmd-execute cmd))))
            (when proc
              ;; Set sentinel to catch completion
              (set-process-sentinel
               proc
               (lambda (_p event)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (let ((status (if (string-match-p "finished" event) "Success" "Failed")))
                       (taut-shell-steps--update-status idx status)))))))))))))

(defun taut-shell-steps--update-status (idx status)
  "Update status of step IDX to STATUS and re-render."
  (setq taut-shell-steps-data
        (mapcar (lambda (s)
                  (if (= (plist-get s :idx) idx)
                      (plist-put s :status status)
                    s))
                taut-shell-steps-data))
  (taut-shell-steps-render))

(defun taut-shell-steps-edit-idx (idx)
  "Prompt to edit the command string for step IDX."
  (let* ((step (cl-find-if (lambda (s) (= (plist-get s :idx) idx)) taut-shell-steps-data))
         (cmd (and step (plist-get step :cmd))))
    (when cmd
      (let ((new-cmd (read-string (format "Edit step [%d]: " idx) cmd)))
        (unless (string-empty-p new-cmd)
          (setq taut-shell-steps-data
                (mapcar (lambda (s)
                          (if (= (plist-get s :idx) idx)
                              (plist-put s :cmd new-cmd)
                            s))
                        taut-shell-steps-data))
          (taut-shell-steps-render)
          (message "Step %d updated." idx))))))

(defun taut-shell-steps-delete-idx (idx)
  "Delete step IDX from the list."
  (when (yes-or-no-p (format "Delete step [%d]? " idx))
    (setq taut-shell-steps-data (cl-remove-if (lambda (s) (= (plist-get s :idx) idx)) taut-shell-steps-data))
    ;; Re-index steps
    (let ((new-idx 1))
      (setq taut-shell-steps-data
            (mapcar (lambda (s)
                      (let ((updated (plist-put s :idx new-idx)))
                        (setq new-idx (1+ new-idx))
                        updated))
                    taut-shell-steps-data)))
    (taut-shell-steps-render)
    (message "Step %d deleted." idx)))

(defun taut-shell-steps-add-step ()
  "Interactively add a new command step at the end of the list."
  (interactive)
  (let ((new-cmd (read-string "Add command step: ")))
    (unless (string-empty-p new-cmd)
      (let* ((new-idx (1+ (length taut-shell-steps-data)))
             (new-step (list :idx new-idx :cmd new-cmd :status "Pending")))
        (setq taut-shell-steps-data (append taut-shell-steps-data (list new-step)))
        (taut-shell-steps-render)
        (message "Added step [%d]." new-idx)))))

(defun taut-shell-steps-move-up-at-point ()
  "Move the step under point up by one position."
  (interactive)
  (let ((idx (get-text-property (point) 'taut-step-idx)))
    (when (and idx (> idx 1))
      (let* ((step-curr (cl-find-if (lambda (s) (= (plist-get s :idx) idx)) taut-shell-steps-data))
             (step-prev (cl-find-if (lambda (s) (= (plist-get s :idx) (1- idx))) taut-shell-steps-data)))
        (when (and step-curr step-prev)
          ;; Swap commands and statuses
          (let ((curr-cmd (plist-get step-curr :cmd))
                (curr-status (plist-get step-curr :status))
                (prev-cmd (plist-get step-prev :cmd))
                (prev-status (plist-get step-prev :status)))
            (plist-put step-curr :cmd prev-cmd)
            (plist-put step-curr :status prev-status)
            (plist-put step-prev :cmd curr-cmd)
            (plist-put step-prev :status curr-status))
          (taut-shell-steps-render)
          ;; Move cursor to the new line for this step
          (goto-char (point-min))
          (search-forward (format " [%d] " (1- idx)) nil t)
          (message "Moved step %d up." idx))))))

(defun taut-shell-steps-move-down-at-point ()
  "Move the step under point down by one position."
  (interactive)
  (let ((idx (get-text-property (point) 'taut-step-idx)))
    (when (and idx (< idx (length taut-shell-steps-data)))
      (let* ((step-curr (cl-find-if (lambda (s) (= (plist-get s :idx) idx)) taut-shell-steps-data))
             (step-next (cl-find-if (lambda (s) (= (plist-get s :idx) (1+ idx))) taut-shell-steps-data)))
        (when (and step-curr step-next)
          ;; Swap commands and statuses
          (let ((curr-cmd (plist-get step-curr :cmd))
                (curr-status (plist-get step-curr :status))
                (next-cmd (plist-get step-next :cmd))
                (next-status (plist-get step-next :status)))
            (plist-put step-curr :cmd next-cmd)
            (plist-put step-curr :status next-status)
            (plist-put step-next :cmd curr-cmd)
            (plist-put step-next :status curr-status))
          (taut-shell-steps-render)
          ;; Move cursor to the new line for this step
          (goto-char (point-min))
          (search-forward (format " [%d] " (1+ idx)) nil t)
          (message "Moved step %d down." idx))))))

(defun taut-shell-steps-reset ()
  "Reset the status of all steps to Pending."
  (interactive)
  (setq taut-shell-steps-data
        (mapcar (lambda (s)
                  (plist-put s :status "Pending"))
                taut-shell-steps-data))
  (taut-shell-steps-render)
  (message "All steps reset to Pending."))

(defun taut-shell-steps-change-dir ()
  "Prompt to change the execution directory."
  (interactive)
  (let ((new-dir (read-directory-name "Set execution directory: " taut-shell-steps-directory)))
    (when (file-directory-p new-dir)
      (setq taut-shell-steps-directory (expand-file-name new-dir))
      (taut-shell-steps-render)
      (message "Execution directory set to %s" taut-shell-steps-directory))))

(defun taut-shell-steps-run-all ()
  "Sequentially execute all remaining pending steps from start to finish."
  (interactive)
  (let ((pending (cl-remove-if-not (lambda (s) (equal (plist-get s :status) "Pending")) taut-shell-steps-data)))
    (if (null pending)
        (message "No pending steps to run.")
      (taut-shell-steps--run-sequentially pending))))

(defun taut-shell-steps--run-sequentially (steps)
  "Helper to run STEPS sequentially."
  (when steps
    (let* ((first (car steps))
           (rest (cdr steps))
           (idx (plist-get first :idx))
           (cmd (plist-get first :cmd))
           (cd-dir (taut-shell-steps--handle-cd cmd)))
      (if cd-dir
          (progn
            (setq taut-shell-steps-directory cd-dir)
            (taut-shell-steps--update-status idx "Success")
            (message "Changed directory to %s" cd-dir)
            (taut-shell-steps--run-sequentially rest))
        (let* ((dir taut-shell-steps-directory)
               (buf (current-buffer))
               (_ (taut-shell-steps--update-status idx "Running"))
               (proc (let ((default-directory dir))
                       (taut-runnable-cmd-execute cmd))))
          (when proc
            (set-process-sentinel
             proc
             (lambda (_p event)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (let ((success (string-match-p "finished" event)))
                     (taut-shell-steps--update-status idx (if success "Success" "Failed"))
                     (if success
                         (taut-shell-steps--run-sequentially rest)
                       (message "Run All aborted due to failure on step %d." idx)))))))))))))

(define-derived-mode taut-shell-steps-mode special-mode "Taut-Steps"
  "Major mode for the Taut interactive shell steps table."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(provide 'taut-shell-steps)
;;; taut-shell-steps.el ends here

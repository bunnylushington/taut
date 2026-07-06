;;; taut-thread.el --- Thread Discussion Buffer for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us
;; Keywords: comm, slack

;;; Commentary:
;; This file implements a dedicated side-by-side Thread Discussion buffer
;; for the Taut Slack client, allowing users to read and reply to threads
;; without losing the main channel context.

;;; Code:

(require 'taut-model)
(require 'taut-message)
(require 'taut-api)
(require 'taut-compose)

(declare-function taut-dispatch "taut-transient")
(declare-function taut-message-upload-file "taut-message")
(declare-function taut-message-edit "taut-message")
(declare-function taut-search-quick "taut-search")


;;;; Faces

(defface taut-thread-root-header
  '((((background dark))  :background "#4a154b" :foreground "#f78af2" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (((background light)) :background "#611f69" :foreground "#ffffff" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (t                    :background "#611f69" :foreground "#ffffff" :weight bold :box (:line-width (2 . -1) :style flat-button)))
  "Face for the thread root message header label."
  :group 'taut-faces)

;;;; Buffer-Local Variables

(defvar-local taut-current-thread-ts nil
  "The thread-ts representing the active thread in this buffer.")

(defvar-local taut-current-channel-id nil
  "The channel-id associated with the active thread in this buffer.")

;;;; Major Mode Definition

(defvar taut-thread-mode-map (make-sparse-keymap)
  "Keymap for `taut-thread-mode`.")

(define-key taut-thread-mode-map (kbd "r") #'taut-message-reply-normal)
(define-key taut-thread-mode-map (kbd "R") #'taut-message-reply-quote)
(define-key taut-thread-mode-map (kbd "b") #'taut-message-toggle-star)
(define-key taut-thread-mode-map (kbd "*") #'taut-message-toggle-star)
(define-key taut-thread-mode-map (kbd "n") #'taut-message-next)
(define-key taut-thread-mode-map (kbd "p") #'taut-message-previous)
(define-key taut-thread-mode-map (kbd "g") #'taut-thread-refresh)
(define-key taut-thread-mode-map (kbd "u") #'taut-message-upload-file)
(define-key taut-thread-mode-map (kbd "q") #'taut-thread-close)
(define-key taut-thread-mode-map (kbd "v") #'taut-message-view-at-point)
(define-key taut-thread-mode-map (kbd "e") #'taut-message-edit)
(define-key taut-thread-mode-map (kbd "s") #'taut-message-save-at-point)
(define-key taut-thread-mode-map (kbd "c") #'taut-message-copy-at-point)
(define-key taut-thread-mode-map (kbd "H") #'taut-huddle-join)
(define-key taut-thread-mode-map (kbd "?") #'taut-dispatch)
(define-key taut-thread-mode-map (kbd "/") #'taut-search-quick)

(define-derived-mode taut-thread-mode special-mode "Taut-Thread"
  "Major mode for the Taut side-by-side Thread Discussion view.

\\{taut-thread-mode-map}"
  (setq buffer-read-only t
        word-wrap t
        wrap-prefix "         ")
  (setq-local view-read-only nil)
  (when (and (boundp 'view-mode) view-mode)
    (view-mode -1))
  (visual-line-mode 1))

;;;; Rendering Engine

(defun taut-thread-refresh (&optional fetch-p)
  "Redraw the thread buffer contents.
If FETCH-P is non-nil (or when called interactively), fetch latest
replies from API first."
  (interactive "P")
  (when (and (or fetch-p (called-interactively-p 'any))
             taut-current-thread-ts
             (boundp 'taut-bot-token)
             taut-bot-token)
    (let (root-chan-id)
      (maphash
       (lambda (chan-id msgs)
         (let ((found (cl-find taut-current-thread-ts msgs :key #'taut-message-ts :test #'equal)))
           (when found
             (setq root-chan-id chan-id))))
       taut-messages)
      (when root-chan-id
        (with-local-quit
          (ignore-errors (taut-api-fetch-replies root-chan-id taut-current-thread-ts))))))
  (when taut-current-thread-ts
    (let ((inhibit-read-only t)
          (old-point (point))
          (at-end (eobp)))
      (erase-buffer)
      (taut-thread--render-thread taut-current-thread-ts)
      (if at-end
          (goto-char (point-max))
        (goto-char (min old-point (point-max)))))))

(defun taut-thread--render-thread (thread-ts)
  "Render root message and replies for THREAD-TS."
  ;; Find the root message from all channels
  (let (root-msg root-chan-id)
    (maphash
     (lambda (chan-id msgs)
       (let ((found (cl-find thread-ts msgs :key #'taut-message-ts :test #'equal)))
         (when found
           (setq root-msg found
                 root-chan-id chan-id))))
     taut-messages)

    (if (null root-msg)
        (insert "  Error: Could not locate thread root message.\n")
      (let ((replies (taut-model-get-thread-replies thread-ts))
            (chan (taut-api-get-or-fetch-channel root-chan-id)))
        
        ;; Header Banner
        (insert (propertize " 💬 THREAD DISCUSSION " 'face 'taut-thread-root-header)
                (propertize (format " in #%s\n\n" (if chan (or (taut-channel-name chan) "unknown") "unknown")) 'face 'font-lock-comment-face))

        ;; Root Message
        (insert (propertize "Root Message:\n" 'face '(:weight bold :height 0.9 :foreground "#8a8a8a")))
        (taut-message--render-message-line root-msg)
        
        ;; Replies Header
        (insert (propertize "Replies:\n" 'face '(:weight bold :height 0.9 :foreground "#8a8a8a"))
                "\n")

        ;; Replies list
        (if (null replies)
            (insert "  No replies in this thread yet. Write a reply with `r`!\n")
          (dolist (reply replies)
            (taut-message--render-message-line reply)))))))

;;;; Actions & Window Layout Management

(defun taut-thread-open (thread-ts &optional channel-id)
  "Open the thread discussion buffer for THREAD-TS in a split window on the right.
If CHANNEL-ID is provided, use it directly. Otherwise, attempt to resolve it
using local cache fallback strategies."
  (taut-ensure-consolidated-workspace)
  (let* ((buf-name "*Taut Thread*")
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'taut-thread-mode)
        (taut-thread-mode))
      (setq taut-current-thread-ts thread-ts)
      ;; Query associated channel and fetch latest thread replies if online
      (let ((chan-id channel-id))
        (unless chan-id
          ;; Fallback 1: search replies hash table for any message with this thread-ts
          (let ((replies (gethash thread-ts taut-threads)))
            (when replies
              (setq chan-id (taut-message-channel-id (car replies))))))
        (unless chan-id
          ;; Fallback 2: search taut-messages
          (maphash (lambda (cid msgs)
                     (when (cl-some (lambda (msg) (equal (taut-message-ts msg) thread-ts)) msgs)
                       (setq chan-id cid)))
                   taut-messages))
        (when chan-id
          (setq taut-current-channel-id chan-id))
        (when (and chan-id (boundp 'taut-bot-token) taut-bot-token)
          (ignore-errors (taut-api-fetch-replies chan-id thread-ts))))
      (taut-thread-refresh))

    ;; Window management: place thread window on the right side of the main area
    (let ((window (get-buffer-window buf)))
      (unless window
        ;; Find a suitable window to split (ignore sidebar if possible)
        (let* ((sidebar-buf (get-buffer "*Taut Sidebar*"))
               (sidebar-win (and sidebar-buf (get-buffer-window sidebar-buf)))
               (target-win (if (and sidebar-win (eq (selected-window) sidebar-win))
                               (next-window sidebar-win)
                             (selected-window))))
          ;; Split selected window to place thread on the right
          (let ((right-window (split-window target-win nil 'right)))
            (set-window-buffer right-window buf)
            (setq window right-window))))
      (select-window window)
      (goto-char (point-max))
      ;; Refresh conversation buffers to update parent highlighting
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (eq major-mode 'taut-message-mode)
            (taut-message-refresh))))
      buf)))

(defun taut-thread-send ()
  "Start composing a thread reply using the compose buffer."
  (interactive)
  (unless taut-current-thread-ts
    (error "No thread is currently active in this buffer"))
  (let ((chan-id (or taut-current-channel-id "C_UNKNOWN")))
    (if (fboundp 'taut-compose-open)
        (taut-compose-open chan-id taut-current-thread-ts)
      (error "Composer is not loaded"))))

(defun taut-thread-close ()
  "Close/Delete the active thread discussion window."
  (interactive)
  (let ((window (get-buffer-window "*Taut Thread*")))
    (if window
        (delete-window window)
      (bury-buffer))
    ;; Refresh conversation buffers to clear highlighting
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'taut-message-mode)
          (taut-message-refresh))))))

(defun taut-thread-refresh-all ()
  "Refresh all active `taut-thread-mode` buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'taut-thread-mode)
          (taut-thread-refresh))))))

;; Hook auto-updates
(add-hook 'taut-model-updated-hook #'taut-thread-refresh-all)

(provide 'taut-thread)
;;; taut-thread.el ends here

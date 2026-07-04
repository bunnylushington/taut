;;; taut-inbox.el --- Unified Gnus-style Inbox for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Google DeepMind

;; Author: Antigravity
;; Keywords: comm, slack

;;; Commentary:
;; This file implements the Gnus-style unified Inbox for the Taut Slack client.
;; It compiles unread DMs, channel mentions, and thread updates into a
;; single chronological feed, giving the user a highly visible action list.

;;; Code:

(require 'taut-model)

(declare-function taut-dispatch "taut-transient")

;;;; Faces

(defface taut-inbox-unread-star
  '((((background dark))  :foreground "#ff4a80" :weight bold)
    (((background light)) :foreground "#e01e5a" :weight bold)
    (t                    :foreground "#e01e5a" :weight bold))
  "Face for the unread indicator asterisk."
  :group 'taut-faces)

(defface taut-inbox-time
  '((((background dark))  :foreground "#a0aec0" :weight normal)
    (((background light)) :foreground "#8a8a8a" :weight normal)
    (t                    :foreground "#8a8a8a" :weight normal))
  "Face for timestamps in the inbox list."
  :group 'taut-faces)

(defface taut-inbox-title
  '((((background dark))  :foreground "#ffffff" :weight bold)
    (((background light)) :foreground "#1d1c1d" :weight bold)
    (t                    :foreground "#1d1c1d" :weight bold))
  "Face for the inbox source title (channel name or user name)."
  :group 'taut-faces)

(defface taut-inbox-snippet
  '((((background dark))  :foreground "#cbd5e0" :weight normal)
    (((background light)) :foreground "#555555" :weight normal)
    (t                    :foreground "#555555" :weight normal))
  "Face for message preview text."
  :group 'taut-faces)

(defface taut-inbox-type-dm
  '((((background dark))  :background "#1a365d" :foreground "#90cdf4" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (((background light)) :background "#e3f2fd" :foreground "#0d47a1" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (t                    :background "#e3f2fd" :foreground "#0d47a1" :weight bold :box (:line-width (2 . -1) :style flat-button)))
  "Face for [DM] labels."
  :group 'taut-faces)

(defface taut-inbox-type-mention
  '((((background dark))  :background "#742a2a" :foreground "#feb2b2" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (((background light)) :background "#fbe9e7" :foreground "#b71c1c" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (t                    :background "#fbe9e7" :foreground "#b71c1c" :weight bold :box (:line-width (2 . -1) :style flat-button)))
  "Face for [Mention] labels."
  :group 'taut-faces)

(defface taut-inbox-type-thread
  '((((background dark))  :background "#22543d" :foreground "#9ae6b4" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (((background light)) :background "#e8f5e9" :foreground "#1b5e20" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (t                    :background "#e8f5e9" :foreground "#1b5e20" :weight bold :box (:line-width (2 . -1) :style flat-button)))
  "Face for [Thread] labels."
  :group 'taut-faces)

;;;; Major Mode Definition

(defvar taut-inbox-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'taut-inbox-activate)
    (define-key map (kbd "<mouse-1>") #'taut-inbox-mouse-activate)
    (define-key map (kbd "d") #'taut-inbox-mark-read)
    (define-key map (kbd "e") #'taut-inbox-mark-read) ; Alternative archive/dismiss key
    (define-key map (kbd "g") #'taut-inbox-refresh)
    (define-key map (kbd "q") #'taut-inbox-bury)
    (define-key map (kbd "?") #'taut-dispatch)
    map)
  "Keymap for `taut-inbox-mode`.")

(define-derived-mode taut-inbox-mode special-mode "Taut-Inbox"
  "Major mode for the Taut unified Inbox.

\\{taut-inbox-mode-map}"
  (setq buffer-read-only t
        truncate-lines t)
  (hl-line-mode 1))

;;;; Rendering Engine

(defun taut-inbox-refresh ()
  "Redraw the inbox buffer contents if it exists and is visible."
  (interactive)
  (let ((buf (get-buffer "*Taut Inbox*")))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (old-point (point)))
          (erase-buffer)
          (taut-inbox--render-feed)
          (goto-char (min old-point (point-max))))))))

(defun taut-inbox--render-feed ()
  "Query the model and render inbox rows into the current buffer."
  (insert (propertize "  UNIFIED INBOX  " 'face '(:weight bold :underline t :height 1.1))
          (propertize (format " (%s items)\n\n" (length (taut-model-get-inbox-items))) 'face 'font-lock-comment-face))
  
  (let ((items (taut-model-get-inbox-items)))
    (if (null items)
        (insert "  ✨ All caught up! No active DMs, mentions, or thread updates.\n")
      (dolist (item items)
        (taut-inbox--render-row item)))))

(defun taut-inbox--render-row (item)
  "Render a single inbox row for ITEM."
  (let* ((row-start (point))
         (type (taut-inbox-item-type item))
         ;; Unread indicator
         (unread-marker (propertize "● " 'face 'taut-inbox-unread-star))
         ;; Stylize Timestamp (Slack ts is epoch in seconds, e.g., 1688474251.0001)
         (time-str (taut-inbox--format-ts (taut-inbox-item-ts item)))
         (time-part (propertize (format "[%s]" time-str) 'face 'taut-inbox-time))
         ;; Stylize Source Type Badge
         (badge-part (cond
                      ((eq type 'dm)             (propertize "   DM   " 'face 'taut-inbox-type-dm))
                      ((eq type 'mention)        (propertize " MENTION" 'face 'taut-inbox-type-mention))
                      ((eq type 'thread-update)  (propertize " THREAD " 'face 'taut-inbox-type-thread))))
         ;; Title (Channel or sender)
         (title-part (propertize (or (taut-inbox-item-title item) "unknown") 'face 'taut-inbox-title))
         ;; Sender user name
         (sender (taut-model-get-user (taut-inbox-item-user-id item)))
         (sender-name (format "@%s" (if sender (or (taut-user-username sender) "unknown") "unknown")))
         ;; Excerpt
         (snippet-part (propertize (format "%s: %S" sender-name (or (taut-inbox-item-snippet item) ""))
                                   'face 'taut-inbox-snippet)))

    ;; Construct row:
    ;; •  [12:34]  [ DM ]  DM: @alice  @alice: "Hey review..."
    (insert "  " unread-marker " " time-part "  " badge-part "  " title-part "  " snippet-part "\n")
    
    ;; Apply text properties to the entire row for click actions
    (add-text-properties row-start (point)
                         (list 'taut-inbox-item item
                               'mouse-face 'highlight))))

(defun taut-inbox--format-ts (ts-str)
  "Convert a Slack TS-STR (e.g. \"1688474251.0001\") into a human-readable HH:MM."
  (if (and ts-str (string-match "^\\([0-9]+\\)" ts-str))
      (let* ((epoch (string-to-number (match-string 1 ts-str)))
             (time-val (seconds-to-time epoch)))
        (format-time-string "%H:%M" time-val))
    "--:--"))

;;;; Actions and Interactivity

(defun taut-inbox-activate ()
  "Activate/Open the inbox item under the cursor."
  (interactive)
  (let ((item (get-text-property (point) 'taut-inbox-item)))
    (if (null item)
        (message "No item at point.")
      (let ((chan-id (taut-inbox-item-channel-id item))
            (thread-ts (taut-inbox-item-thread-ts item))
            (type (taut-inbox-item-type item)))
        (cond
         ((eq type 'thread-update)
          ;; Mark thread read first and open thread
          (taut-model-mark-thread-read thread-ts)
          (if (fboundp 'taut-thread-open)
              (funcall 'taut-thread-open thread-ts)
            (message "Opening thread %s" thread-ts)))
         (t
          ;; DM or Mention: mark channel read and open channel buffer
          (taut-model-mark-channel-read chan-id)
          (if (fboundp 'taut-message-open)
              (funcall 'taut-message-open chan-id)
            (message "Opening channel %s" chan-id))))))))

(defun taut-inbox-mouse-activate (event)
  "Handle mouse click EVENT in Inbox."
  (interactive "e")
  (posn-set-point (event-end event))
  (taut-inbox-activate))

(defun taut-inbox-mark-read ()
  "Dismiss/Mark read the item under the cursor."
  (interactive)
  (let ((item (get-text-property (point) 'taut-inbox-item)))
    (if (null item)
        (message "No item at point.")
      (let ((type (taut-inbox-item-type item)))
        (if (eq type 'thread-update)
            (taut-model-mark-thread-read (taut-inbox-item-thread-ts item))
          (taut-model-mark-channel-read (taut-inbox-item-channel-id item)))
        (message "Marked read.")
        (taut-inbox-refresh)))))

(defun taut-inbox-show ()
  "Display the Taut Inbox in the active central window."
  (interactive)
  (let ((buf (get-buffer-create "*Taut Inbox*")))
    (with-current-buffer buf
      (unless (eq major-mode 'taut-inbox-mode)
        (taut-inbox-mode)))
    (switch-to-buffer buf)
    (taut-inbox-refresh)
    buf))

(defun taut-inbox-bury ()
  "Bury the Taut Inbox buffer."
  (interactive)
  (bury-buffer))

;; Hook auto-updates
(add-hook 'taut-model-updated-hook #'taut-inbox-refresh)

(provide 'taut-inbox)
;;; taut-inbox.el ends here

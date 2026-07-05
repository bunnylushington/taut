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

(defface taut-inbox-filter-active
  '((((background dark))  :foreground "#ffffff" :weight bold :underline t)
    (((background light)) :foreground "#1d1c1d" :weight bold :underline t)
    (t                    :weight bold :underline t))
  "Face for the active inbox filter."
  :group 'taut-faces)

(defface taut-inbox-filter-inactive
  '((((background dark))  :foreground "#a0aec0" :weight normal)
    (((background light)) :foreground "#718096" :weight normal)
    (t                    :weight normal))
  "Face for inactive inbox filters."
  :group 'taut-faces)

(defface taut-inbox-channel-badge
  '((((background dark))  :background "#2d3748" :foreground "#cbd5e0" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (((background light)) :background "#edf2f7" :foreground "#4a5568" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (t                    :background "#edf2f7" :foreground "#4a5568" :weight bold :box (:line-width (2 . -1) :style flat-button)))
  "Face for channel badges in the inbox list."
  :group 'taut-faces)

;;;; Major Mode Definition

(defvar-local taut-inbox-filter 'all
  "The current active filter in the Taut Inbox.
Can be \\='all, \\='unreads, \\='dms, \\='mentions, or \\='threads.")

(defvar taut-inbox-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'taut-inbox-activate)
    (define-key map (kbd "<mouse-1>") #'taut-inbox-mouse-activate)
    (define-key map (kbd "d") #'taut-inbox-mark-read)
    (define-key map (kbd "e") #'taut-inbox-mark-read) ; Alternative archive/dismiss key
    (define-key map (kbd "g") #'taut-inbox-refresh)
    (define-key map (kbd "q") #'taut-inbox-bury)
    (define-key map (kbd "a") #'taut-inbox-filter-all)
    (define-key map (kbd "u") #'taut-inbox-filter-unreads)
    (define-key map (kbd "D") #'taut-inbox-filter-dms)
    (define-key map (kbd "m") #'taut-inbox-filter-mentions)
    (define-key map (kbd "t") #'taut-inbox-filter-threads)
    (define-key map (kbd "?") #'taut-dispatch)
    map)
  "Keymap for `taut-inbox-mode`.")

(define-derived-mode taut-inbox-mode special-mode "Taut-Inbox"
  "Major mode for the Taut unified Inbox.

\\{taut-inbox-mode-map}"
  (setq buffer-read-only t
        truncate-lines t)
  (setq-local taut-inbox-filter 'all)
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

(defun taut-inbox--render-header ()
  "Render a beautiful header with navigation and active filter indication."
  (insert (propertize "================================================================================\n" 'face 'font-lock-comment-face))
  (insert (propertize "  💬 TAUT ACTIVITY INBOX\n" 'face '(:weight bold :height 1.2 :foreground "#e01e5a")))
  (insert (propertize "================================================================================\n" 'face 'font-lock-comment-face))
  
  ;; Render filters bar
  (insert "  Filters: ")
  (let ((all-face (if (eq taut-inbox-filter 'all) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (unreads-face (if (eq taut-inbox-filter 'unreads) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (dms-face (if (eq taut-inbox-filter 'dms) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (mentions-face (if (eq taut-inbox-filter 'mentions) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (threads-face (if (eq taut-inbox-filter 'threads) 'taut-inbox-filter-active 'taut-inbox-filter-inactive)))
    (insert (propertize "[a] All" 'face all-face 'help-echo "Show all activity") "  •  "
            (propertize "[u] Unreads" 'face unreads-face 'help-echo "Show unreads only") "  •  "
            (propertize "[D] DMs" 'face dms-face 'help-echo "Show direct messages only") "  •  "
            (propertize "[m] Mentions" 'face mentions-face 'help-echo "Show mentions only") "  •  "
            (propertize "[t] Threads" 'face threads-face 'help-echo "Show thread updates only")))
  (insert "\n")
  (insert "  Active: " (propertize (upcase (symbol-name taut-inbox-filter)) 'face '(:weight bold :foreground "#36c5f0")) "\n")
  (insert (propertize "--------------------------------------------------------------------------------\n\n" 'face 'font-lock-comment-face)))

(defun taut-inbox--render-feed ()
  "Query the model and render inbox rows into the current buffer."
  (taut-inbox--render-header)
  
  (let* ((all-items (taut-model-get-activity-items))
         ;; Filter items based on the active filter state
         (filtered-items
          (cl-remove-if-not
           (lambda (item)
             (let ((type (taut-inbox-item-type item))
                   (is-read (taut-inbox-item-is-read item)))
               (cond
                ((eq taut-inbox-filter 'all)      t)
                ((eq taut-inbox-filter 'unreads)  (not is-read))
                ((eq taut-inbox-filter 'dms)      (eq type 'dm))
                ((eq taut-inbox-filter 'mentions) (eq type 'mention))
                ((eq taut-inbox-filter 'threads)  (eq type 'thread-update))
                (t t))))
           all-items))
         ;; Sort items: Unreads first, then by timestamp descending
         (sorted-items
          (sort filtered-items
                (lambda (a b)
                  (let ((read-a (taut-inbox-item-is-read a))
                        (read-b (taut-inbox-item-is-read b)))
                    (cond
                     ;; If one is unread and the other is read, unread comes first
                     ((and (not read-a) read-b) t)
                     ((and read-a (not read-b)) nil)
                     ;; Otherwise, sort by timestamp descending
                     (t (string> (or (taut-inbox-item-ts a) "")
                                 (or (taut-inbox-item-ts b) "")))))))))
    
    (if (null sorted-items)
        (insert "  ✨ No activity matches this filter.\n")
      (let ((current-date-group nil))
        (dolist (item sorted-items)
          (let ((date-group (taut-inbox--format-date-group (taut-inbox-item-ts item))))
            (unless (equal date-group current-date-group)
              (setq current-date-group date-group)
              (insert "\n  " (propertize date-group 'face '(:weight bold :foreground "#36c5f0" :underline t)) "\n\n"))
            (taut-inbox--render-row item)))))))

(defun taut-inbox--render-row (item)
  "Render a single inbox row for ITEM."
  (let* ((row-start (point))
         (type (taut-inbox-item-type item))
         (is-read (taut-inbox-item-is-read item))
         ;; Unread indicator
         (unread-marker (if is-read
                            (propertize "  " 'face 'default)
                          (propertize "● " 'face 'taut-inbox-unread-star)))
         ;; Stylize Timestamp (Slack ts is epoch in seconds)
         (time-str (taut-inbox--format-relative-date (taut-inbox-item-ts item)))
         (time-part (propertize (format "[%s]" time-str) 'face 'taut-inbox-time))
         ;; Stylize Source Type Badge
         (badge-part (cond
                      ((eq type 'dm)             (propertize " 👤 DM " 'face 'taut-inbox-type-dm))
                      ((eq type 'mention)        (propertize " @ MENTION " 'face 'taut-inbox-type-mention))
                      ((eq type 'thread-update)  (propertize " 💬 THREAD " 'face 'taut-inbox-type-thread))))
         ;; Sender user name
         (sender (taut-model-get-user (taut-inbox-item-user-id item)))
         (sender-display (if sender
                             (or (taut-user-real-name sender) (taut-user-username sender) "unknown")
                           "unknown"))
         (sender-part (propertize sender-display 'face 'taut-inbox-title))
         ;; Channel badge if any
         (chan-id (taut-inbox-item-channel-id item))
         (chan (taut-model-get-channel chan-id))
         (channel-part
          (if (and chan (not (eq (taut-channel-type chan) 'dm)))
              (let* ((prefix (if (eq (taut-channel-type chan) 'private) "🔒" "#"))
                     (chan-name (or (taut-channel-name chan) "unknown")))
                (concat " in " (propertize (format "%s%s" prefix chan-name) 'face 'taut-inbox-channel-badge) " "))
            " "))
         ;; Excerpt
         (clean-text (taut-inbox--clean-snippet (taut-inbox-item-snippet item)))
         (snippet-part (propertize clean-text 'face 'taut-inbox-snippet)))

    ;; Construct row:
    ;; •  [Today 14:35]  👤 DM  Greg Rhoades: definitely interested. slack in emacs is...
    ;; •  [Thursday]  💬 THREAD  Tony Akens  in  #ip4g-product-dev : if y'all don't have access to gr...
    (insert "  " unread-marker " " time-part "  " badge-part "  " sender-part channel-part ": " snippet-part "\n")
    
    ;; Apply text properties to the entire row for click actions
    (add-text-properties row-start (point)
                         (list 'taut-inbox-item item
                               'mouse-face 'highlight))))

(defun taut-inbox--format-relative-date (ts-str)
  "Format Slack timestamp TS-STR into a relative date string.
Includes representations like Today, Yesterday, day of week, or date."
  (if (and ts-str (string-match "^\\([0-9]+\\)" ts-str))
      (let* ((epoch (string-to-number (match-string 1 ts-str)))
             (now (float-time))
             (diff (- now epoch))
             (time-val (seconds-to-time epoch)))
        (cond
         ((< diff 86400)
          (format-time-string "Today %H:%M" time-val))
         ((< diff 172800)
          (format-time-string "Yesterday %H:%M" time-val))
         ((< diff 604800)
          (format-time-string "%A %H:%M" time-val))
         (t
          (format-time-string "%b %d" time-val))))
    "--:--"))

(defun taut-inbox--format-date-group (ts-str)
  "Get the date grouping header for TS-STR.
Categorizes timestamps into Today, Yesterday, Weekday, or Month."
  (if (and ts-str (string-match "^\\([0-9]+\\)" ts-str))
      (let* ((epoch (string-to-number (match-string 1 ts-str)))
             (now (float-time))
             (diff (- now epoch))
             (time-val (seconds-to-time epoch)))
        (cond
         ((< diff 86400) "Today")
         ((< diff 172800) "Yesterday")
         ((< diff 604800) (format-time-string "%A" time-val))
         (t (format-time-string "%B %d" time-val))))
    "Older Activity"))

(defun taut-inbox--clean-snippet (text)
  "Clean and truncate TEXT for display as an inbox snippet."
  (if text
      (let* ((clean (replace-regexp-in-string "[\n\r\t ]+" " " text))
             (trimmed (replace-regexp-in-string "^\\s-+\\|\\s-+$" "" clean)))
        (if (> (length trimmed) 80)
            (concat (substring trimmed 0 80) "...")
          trimmed))
    ""))

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

(defun taut-inbox-filter-all ()
  "Show all activity in the inbox."
  (interactive)
  (setq taut-inbox-filter 'all)
  (taut-inbox-refresh))

(defun taut-inbox-filter-unreads ()
  "Show only unread items in the inbox."
  (interactive)
  (setq taut-inbox-filter 'unreads)
  (taut-inbox-refresh))

(defun taut-inbox-filter-dms ()
  "Show only DMs in the inbox."
  (interactive)
  (setq taut-inbox-filter 'dms)
  (taut-inbox-refresh))

(defun taut-inbox-filter-mentions ()
  "Show only mentions in the inbox."
  (interactive)
  (setq taut-inbox-filter 'mentions)
  (taut-inbox-refresh))

(defun taut-inbox-filter-threads ()
  "Show only thread updates in the inbox."
  (interactive)
  (setq taut-inbox-filter 'threads)
  (taut-inbox-refresh))

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

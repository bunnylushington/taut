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

(defvar taut-use-icons)

(declare-function taut-dispatch "taut-transient")
(declare-function taut-message-open "taut-message" (chan-id &optional other-window))
(declare-function taut-thread-open "taut-thread" (thread-ts &optional channel-id))

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

(defface taut-inbox-type-channel
  '((((background dark))  :background "#4a154b" :foreground "#ffffff" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (((background light)) :background "#f4ecef" :foreground "#4a154b" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (t                    :background "#4a154b" :foreground "#ffffff" :weight bold :box (:line-width (2 . -1) :style flat-button)))
  "Face for [Channel] labels."
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
  "The current active filter in Slack Activity.
Can be \\='all, \\='unreads, \\='dms, \\='mentions, or \\='threads.")

(defvar taut-inbox-mode-map (make-sparse-keymap)
  "Keymap for `taut-inbox-mode`.")

(define-key taut-inbox-mode-map (kbd "RET") #'taut-inbox-activate)
(define-key taut-inbox-mode-map (kbd "<mouse-1>") #'taut-inbox-mouse-activate)
(define-key taut-inbox-mode-map (kbd "d") #'taut-inbox-mark-read)
(define-key taut-inbox-mode-map (kbd "e") #'taut-inbox-mark-read) ; Alternative archive/dismiss key
(define-key taut-inbox-mode-map (kbd "g") #'taut-inbox-refresh)
(define-key taut-inbox-mode-map (kbd "M") #'taut-inbox-mark-channel-read)
(define-key taut-inbox-mode-map (kbd "q") #'taut-inbox-bury)
(define-key taut-inbox-mode-map (kbd "a") #'taut-inbox-filter-all)
(define-key taut-inbox-mode-map (kbd "u") #'taut-inbox-filter-unreads)
(define-key taut-inbox-mode-map (kbd "D") #'taut-inbox-filter-dms)
(define-key taut-inbox-mode-map (kbd "m") #'taut-inbox-filter-mentions)
(define-key taut-inbox-mode-map (kbd "t") #'taut-inbox-filter-threads)
(define-key taut-inbox-mode-map (kbd "?") #'taut-dispatch)

(define-derived-mode taut-inbox-mode special-mode "Slack Activity"
  "Major mode for Taut Slack Activity.

\\{taut-inbox-mode-map}"
  (setq buffer-read-only t
        truncate-lines t)
  (setq-local taut-inbox-filter 'all)
  (hl-line-mode 1))

;;;; Rendering Engine

(defun taut-inbox-refresh ()
  "Redraw the Slack Activity buffer contents if it exists and is visible."
  (interactive)
  (let ((buf (get-buffer "*Slack Activity*")))
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
  (insert (propertize "  💬 SLACK ACTIVITY\n" 'face '(:weight bold :height 1.2 :foreground "#e01e5a")))
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
  (unless taut-inbox-filter
    (setq taut-inbox-filter 'all))
  (taut-inbox--render-header)
  
  (let* ((all-items (if (fboundp 'taut-model-get-activity-items)
                        (taut-model-get-activity-items)
                      (taut-model-get-inbox-items)))
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
         ;; Sort items: chronologically descending by timestamp
         (sorted-items
          (sort filtered-items
                (lambda (a b)
                  (string> (or (taut-inbox-item-ts a) "")
                           (or (taut-inbox-item-ts b) ""))))))
    
    (if (null sorted-items)
        (insert "  ✨ No activity matches this filter.\n")
      (let ((current-date-group nil))
        (dolist (item sorted-items)
          (let ((date-group (taut-inbox--format-date-group (taut-inbox-item-ts item))))
            (unless (equal date-group current-date-group)
              (setq current-date-group date-group)
              (insert "\n  " (propertize date-group 'face '(:weight bold :foreground "#36c5f0" :underline t)) "\n\n"))
            (taut-inbox--render-row item)))))))

(defun taut-inbox--get-icon-badge (type)
  "Get a stylized icon badge for TYPE.
Supports DM, MENTION, THREAD-UPDATE, and CHANNEL types."
  (if (and (boundp 'taut-use-icons) taut-use-icons (fboundp 'nerd-icons-octicon))
      (cond
       ((eq type 'dm)
        (propertize (concat " " (nerd-icons-octicon "nf-oct-person" :face 'taut-inbox-type-dm) " DM ")
                    'face 'taut-inbox-type-dm))
       ((eq type 'mention)
        (propertize (concat " " (nerd-icons-octicon "nf-oct-mention" :face 'taut-inbox-type-mention) " MENTION ")
                    'face 'taut-inbox-type-mention))
       ((eq type 'thread-update)
        (propertize (concat " " (nerd-icons-octicon "nf-oct-comment_discussion" :face 'taut-inbox-type-thread) " THREAD ")
                    'face 'taut-inbox-type-thread))
       ((eq type 'channel)
        (propertize (concat " " (nerd-icons-octicon "nf-oct-hash" :face 'taut-inbox-type-channel) " CHANNEL ")
                    'face 'taut-inbox-type-channel))
       (t
        (propertize (concat " " (nerd-icons-octicon "nf-oct-comment" :face 'taut-inbox-type-channel) " CHAT ")
                    'face 'taut-inbox-type-channel)))
    (if (and (boundp 'taut-use-icons) taut-use-icons (fboundp 'all-the-icons-octicon))
        (cond
         ((eq type 'dm)
          (propertize (concat " " (all-the-icons-octicon "person" :face 'taut-inbox-type-dm) " DM ")
                      'face 'taut-inbox-type-dm))
         ((eq type 'mention)
          (propertize (concat " " (all-the-icons-octicon "mention" :face 'taut-inbox-type-mention) " MENTION ")
                      'face 'taut-inbox-type-mention))
         ((eq type 'thread-update)
          (propertize (concat " " (all-the-icons-octicon "comment-discussion" :face 'taut-inbox-type-thread) " THREAD ")
                      'face 'taut-inbox-type-thread))
         ((eq type 'channel)
          (propertize (concat " " (all-the-icons-octicon "tag" :face 'taut-inbox-type-channel) " CHANNEL ")
                      'face 'taut-inbox-type-channel))
         (t
          (propertize (concat " " (all-the-icons-octicon "comment" :face 'taut-inbox-type-channel) " CHAT ")
                      'face 'taut-inbox-type-channel)))
      ;; Unicode fallback symbols
      (cond
       ((eq type 'dm)             (propertize " 👤 DM " 'face 'taut-inbox-type-dm))
       ((eq type 'mention)        (propertize " @ MENTION " 'face 'taut-inbox-type-mention))
       ((eq type 'thread-update)  (propertize " 💬 THREAD " 'face 'taut-inbox-type-thread))
       ((eq type 'channel)        (propertize " ♯ CHANNEL " 'face 'taut-inbox-type-channel))
       (t                         (propertize " 💬 CHAT " 'face 'taut-inbox-type-channel))))))

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
         (badge-part (taut-inbox--get-icon-badge type))
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
         (unread-cnt (and (taut-inbox-item-unread-count item)
                          (taut-inbox-item-unread-count item)))
         (snippet-prefix (if (and unread-cnt (> unread-cnt 1))
                             (propertize (format "[%d unreads] " unread-cnt)
                                         'face '(:weight bold :foreground "#ff4a80"))
                           ""))
         (snippet-part (concat snippet-prefix (propertize clean-text 'face 'taut-inbox-snippet))))

    ;; Construct row:
    ;; •  [Today 14:35]  👤 DM  Greg Rhoades: definitely interested. slack in emacs is...
    ;; •  [Thursday]  💬 THREAD  Tony Akens  in  #ip4g-product-dev : if y'all don't have access to gr...
    (insert "  " unread-marker " " time-part "  " badge-part "  " sender-part channel-part ": " snippet-part)
    
    ;; Apply text properties to the row (excluding newline) for clicks
    (add-text-properties row-start (point)
                         (list 'taut-inbox-item item
                               'mouse-face 'highlight))
    (insert "\n")))

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
          (let ((item-year (format-time-string "%Y" time-val))
                (current-year (format-time-string "%Y")))
            (if (equal item-year current-year)
                (format-time-string "%b %d" time-val)
              (format-time-string "%b %d, %Y" time-val))))))
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
         (t
          (let ((item-year (format-time-string "%Y" time-val))
                (current-year (format-time-string "%Y")))
            (if (equal item-year current-year)
                (format-time-string "%B %d" time-val)
              (format-time-string "%B %d, %Y" time-val))))))
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
              (funcall 'taut-thread-open thread-ts chan-id)
            (message "Opening thread %s" thread-ts)))
         (t
          ;; DM or Mention: mark channel read and open channel buffer
          (taut-model-mark-channel-read chan-id)
          (if (fboundp 'taut-message-open)
              (funcall 'taut-message-open chan-id t)
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

(defun taut-inbox-mark-channel-read ()
  "Mark all messages in the channel under the cursor as read."
  (interactive)
  (let ((item (get-text-property (point) 'taut-inbox-item)))
    (if (null item)
        (message "No item at point.")
      (let ((chan-id (taut-inbox-item-channel-id item)))
        (if (null chan-id)
            (message "No channel associated with this item.")
          (taut-model-mark-channel-read chan-id)
          (message "Marked all messages in channel as read.")
          (taut-inbox-refresh))))))

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

(defun taut-inbox--focus-buffer (buf-or-name)
  "Find and focus a window, tab, or frame displaying BUF-OR-NAME.
Returns non-nil if the buffer was found and focused."
  (let* ((buf (get-buffer buf-or-name))
         (found nil))
    (when buf
      ;; 1. Check if it's already visible in any window on any frame
      (let ((win (get-buffer-window buf t)))
        (when win
          (select-frame-set-input-focus (window-frame win))
          (select-window win)
          (setq found t)))
      
      ;; 2. If not found, search through tabs of all frames
      (unless found
        (when (and (require 'tab-bar nil t)
                   (fboundp 'tab-bar-tabs))
          (catch 'done
            (dolist (frame (frame-list))
              ;; Get tabs for this frame
              (let ((tabs (frame-parameter frame 'tabs)))
                (dolist (tab tabs)
                  ;; A tab is (tab (name . "...") ... (ws . <window-state>))
                  (when (eq (car tab) 'tab)
                    (let* ((props (cdr tab))
                           (ws (cdr (assq 'ws props)))
                           (tab-name (cdr (assq 'name props))))
                      (when (and ws tab-name)
                        (let ((bufs (and (fboundp 'window-state-buffers)
                                         (window-state-buffers ws))))
                          (when (member (buffer-name buf) bufs)
                            ;; Switch to the frame
                            (select-frame-set-input-focus frame)
                            ;; Switch to the tab
                            (with-selected-frame frame
                              (tab-bar-select-tab-by-name tab-name))
                            ;; Now buffer should be visible in some window of the frame
                            (let ((win (get-buffer-window buf frame)))
                              (when win
                                (select-window win)))
                            (setq found t)
                            (throw 'done t))))))))))))
    found)))

(defun taut-inbox-show ()
  "Display the Slack Activity in the active central window."
  (interactive)
  (let ((buf (get-buffer-create "*Slack Activity*"))
        (sidebar-win (get-buffer-window "*Taut Sidebar*")))
    (with-current-buffer buf
      (unless (eq major-mode 'taut-inbox-mode)
        (taut-inbox-mode)))
    (unless (taut-inbox--focus-buffer buf)
      (cond
       ((and sidebar-win (eq (selected-window) sidebar-win))
        (select-window (next-window sidebar-win))
        (switch-to-buffer buf))
       (t
        (switch-to-buffer buf))))
    (taut-inbox-refresh)
    buf))

(defun taut-inbox-bury ()
  "Bury the Slack Activity buffer."
  (interactive)
  (bury-buffer))

;; Hook auto-updates
(add-hook 'taut-model-updated-hook #'taut-inbox-refresh)

(provide 'taut-inbox)
;;; taut-inbox.el ends here

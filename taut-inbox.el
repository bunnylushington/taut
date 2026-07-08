;;; taut-inbox.el --- Unified Gnus-style Inbox for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us
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
(declare-function taut-search-quick "taut-search")
(declare-function taut-setup-strict-windows "taut")

(defvar taut-strict-windows)


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
  "Face for [Channel-Thread] labels."
  :group 'taut-faces)

(defface taut-inbox-type-dm-thread
  '((((background dark))  :background "#312e81" :foreground "#c7d2fe" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (((background light)) :background "#e0e7ff" :foreground "#4338ca" :weight bold :box (:line-width (2 . -1) :style flat-button))
    (t                    :background "#e0e7ff" :foreground "#4338ca" :weight bold :box (:line-width (2 . -1) :style flat-button)))
  "Face for [DM-Thread] labels."
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
  "The current active filter in Slack Inbox.
Can be \\='all, \\='unreads, \\='dms, \\='mentions, \\='threads, or \\='code.")

(defvar-local taut-inbox-date-filter 'last-7
  "The current active date filter in Slack Inbox.
Can be \\='today, \\='last-7, \\='last-30, or \\='all.")

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
(define-key taut-inbox-mode-map (kbd "c") #'taut-inbox-filter-code)
(define-key taut-inbox-mode-map (kbd "1") #'taut-inbox-date-filter-today)
(define-key taut-inbox-mode-map (kbd "2") #'taut-inbox-date-filter-last-7)
(define-key taut-inbox-mode-map (kbd "3") #'taut-inbox-date-filter-last-30)
(define-key taut-inbox-mode-map (kbd "4") #'taut-inbox-date-filter-all)
(define-key taut-inbox-mode-map (kbd "?") #'taut-dispatch)
(define-key taut-inbox-mode-map (kbd "/") #'taut-search-quick)

(define-derived-mode taut-inbox-mode special-mode "Slack Inbox"
  "Major mode for Taut Slack Inbox.

\\{taut-inbox-mode-map}"
  (setq buffer-read-only t
        truncate-lines t)
  (setq-local taut-inbox-filter 'all)
  (setq-local taut-inbox-date-filter 'last-7)
  (hl-line-mode 1))

;;;; Rendering Engine

(defun taut-inbox--find-item-point (item-id)
  "Find the start of the line displaying the item with ITEM-ID in the current buffer.
Returns nil if not found."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (let ((item (get-text-property (point) 'taut-inbox-item)))
          (if (and item (equal (taut-inbox-item-id item) item-id))
              (setq found (line-beginning-position))
            (forward-line 1))))
      found)))

(defun taut-inbox-refresh ()
  "Redraw the Slack Inbox buffer contents if it exists and is visible."
  (interactive)
  (let ((buf (get-buffer "*Slack Inbox*")))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (old-point (point))
              (win (get-buffer-window buf t))
              (win-point nil)
              (current-item (get-text-property (point) 'taut-inbox-item))
              (current-item-id nil))
          (when current-item
            (setq current-item-id (taut-inbox-item-id current-item)))
          (when win
            (setq win-point (window-point win))
            (with-selected-window win
              (let ((win-item (get-text-property (point) 'taut-inbox-item)))
                (when win-item
                  (setq current-item-id (taut-inbox-item-id win-item))))))
          (setq-local header-line-format
                      (concat
                       (propertize " 📥 SLACK INBOX" 'face '(:weight bold :foreground "#e01e5a"))
                       (propertize (format "  |  Filter: %s  |  Date: %s"
                                           (upcase (symbol-name (or taut-inbox-filter 'all)))
                                           (upcase (symbol-name (or taut-inbox-date-filter 'last-7))))
                                   'face 'font-lock-comment-face)))
          (erase-buffer)
          (taut-inbox--render-feed)
          (let ((target-point nil))
            (when current-item-id
              (setq target-point (taut-inbox--find-item-point current-item-id)))
            (unless target-point
              (setq target-point (min (or win-point old-point) (point-max))))
            (goto-char target-point)
            (when win
              (set-window-point win target-point))
            (force-mode-line-update t)))))))

(defun taut-inbox--render-header ()
  "Render a beautiful header with navigation and active filter indication."
  (insert "\n")
  ;; Render filters bar
  (insert "  Filters: ")
  (let ((all-face (if (eq taut-inbox-filter 'all) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (unreads-face (if (eq taut-inbox-filter 'unreads) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (dms-face (if (eq taut-inbox-filter 'dms) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (mentions-face (if (eq taut-inbox-filter 'mentions) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (threads-face (if (eq taut-inbox-filter 'threads) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (code-face (if (eq taut-inbox-filter 'code) 'taut-inbox-filter-active 'taut-inbox-filter-inactive)))
    (insert (propertize "[a] All" 'face all-face 'help-echo "Show all activity") "  •  "
            (propertize "[u] Unreads" 'face unreads-face 'help-echo "Show unreads only") "  •  "
            (propertize "[D] DMs" 'face dms-face 'help-echo "Show direct messages only") "  •  "
            (propertize "[m] Mentions" 'face mentions-face 'help-echo "Show mentions only") "  •  "
            (propertize "[t] Threads" 'face threads-face 'help-echo "Show thread updates only") "  •  "
            (propertize "[c] Code" 'face code-face 'help-echo "Show messages with code blocks only")))
  (insert "\n")

  ;; Render date filters bar
  (insert "     Date: ")
  (let ((today-face (if (eq taut-inbox-date-filter 'today) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (last-7-face (if (eq taut-inbox-date-filter 'last-7) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (last-30-face (if (eq taut-inbox-date-filter 'last-30) 'taut-inbox-filter-active 'taut-inbox-filter-inactive))
        (all-date-face (if (eq taut-inbox-date-filter 'all) 'taut-inbox-filter-active 'taut-inbox-filter-inactive)))
    (insert (propertize "[1] Today" 'face today-face 'help-echo "Filter by today") "  •  "
            (propertize "[2] Last 7 Days" 'face last-7-face 'help-echo "Filter by last 7 days") "  •  "
            (propertize "[3] Last 30 Days" 'face last-30-face 'help-echo "Filter by last 30 days") "  •  "
            (propertize "[4] All Time" 'face all-date-face 'help-echo "Show all time")))
  (insert "\n")

  (insert (propertize "--------------------------------------------------------------------------------\n\n" 'face 'font-lock-comment-face)))

(defun taut-inbox--render-feed ()
  "Query the model and render inbox rows into the current buffer."
  (unless taut-inbox-filter
    (setq taut-inbox-filter 'all))
  (unless taut-inbox-date-filter
    (setq taut-inbox-date-filter 'last-7))
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
               (and
                (cond
                 ((eq taut-inbox-filter 'all)      t)
                 ((eq taut-inbox-filter 'unreads)  (not is-read))
                 ((eq taut-inbox-filter 'dms)      (eq type 'dm))
                 ((eq taut-inbox-filter 'mentions) (eq type 'mention))
                 ((eq taut-inbox-filter 'threads)  (eq type 'thread-update))
                 ((eq taut-inbox-filter 'code)     (taut-inbox-item-has-code-p item))
                 (t t))
                (taut-inbox--item-matches-date-filter-p item))))
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

(defun taut-inbox--get-icon-badge (type &optional item)
  "Get a stylized icon badge for TYPE.
Supports DM, MENTION, THREAD-UPDATE (distinguishing channel vs DM threads), and CHANNEL types."
  (let* ((chan-id (and item (taut-inbox-item-channel-id item)))
         (chan (and chan-id (taut-api-get-or-fetch-channel chan-id)))
         (is-dm-thread (and (eq type 'thread-update)
                            chan
                            (eq (taut-channel-type chan) 'dm))))
    (if (and (boundp 'taut-use-icons) taut-use-icons (fboundp 'nerd-icons-octicon))
        (cond
         ((eq type 'dm)
          (propertize (concat " " (nerd-icons-octicon "nf-oct-mail" :face 'taut-inbox-type-dm) " DM ")
                      'face 'taut-inbox-type-dm))
         ((eq type 'mention)
          (propertize (concat " " (nerd-icons-octicon "nf-oct-alert" :face 'taut-inbox-type-mention) " MENTION ")
                      'face 'taut-inbox-type-mention))
         (is-dm-thread
          (propertize (concat " " (nerd-icons-octicon "nf-oct-git_branch" :face 'taut-inbox-type-dm-thread) " DM-THREAD ")
                      'face 'taut-inbox-type-dm-thread))
         ((eq type 'thread-update)
          (propertize (concat " " (nerd-icons-octicon "nf-oct-git_branch" :face 'taut-inbox-type-thread) " CH-THREAD ")
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
            (propertize (concat " " (all-the-icons-octicon "mail" :face 'taut-inbox-type-dm) " DM ")
                        'face 'taut-inbox-type-dm))
           ((eq type 'mention)
            (propertize (concat " " (all-the-icons-octicon "alert" :face 'taut-inbox-type-mention) " MENTION ")
                        'face 'taut-inbox-type-mention))
           (is-dm-thread
            (propertize (concat " " (all-the-icons-octicon "git-branch" :face 'taut-inbox-type-dm-thread) " DM-THREAD ")
                        'face 'taut-inbox-type-dm-thread))
           ((eq type 'thread-update)
            (propertize (concat " " (all-the-icons-octicon "git-branch" :face 'taut-inbox-type-thread) " CH-THREAD ")
                        'face 'taut-inbox-type-thread))
           ((eq type 'channel)
            (propertize (concat " " (all-the-icons-octicon "tag" :face 'taut-inbox-type-channel) " CHANNEL ")
                        'face 'taut-inbox-type-channel))
           (t
            (propertize (concat " " (all-the-icons-octicon "comment" :face 'taut-inbox-type-channel) " CHAT ")
                        'face 'taut-inbox-type-channel)))
        ;; Unicode fallback symbols
        (cond
         ((eq type 'dm)             (propertize " ✉ DM " 'face 'taut-inbox-type-dm))
         ((eq type 'mention)        (propertize " 🔔 MENTION " 'face 'taut-inbox-type-mention))
         (is-dm-thread              (propertize " 🧵 DM-THREAD " 'face 'taut-inbox-type-dm-thread))
         ((eq type 'thread-update)  (propertize " 🧵 CH-THREAD " 'face 'taut-inbox-type-thread))
         ((eq type 'channel)        (propertize " ♯ CHANNEL " 'face 'taut-inbox-type-channel))
         (t                         (propertize " 💬 CHAT " 'face 'taut-inbox-type-channel)))))))

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
         (time-part (propertize time-str 'face 'taut-inbox-time))
         ;; Stylize Source Type Badge
         (badge-part (taut-inbox--get-icon-badge type item))
         ;; Sender user name
         (sender (taut-model-get-user (taut-inbox-item-user-id item)))
         (sender-display (if sender
                             (or (taut-user-real-name sender) (taut-user-username sender) "unknown")
                           "unknown"))
         (sender-part (propertize sender-display 'face 'taut-inbox-title))
         ;; Channel badge if any
         (chan-id (taut-inbox-item-channel-id item))
         (chan (taut-api-get-or-fetch-channel chan-id))
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
    ;; •  14:35  ✉ DM  Greg Rhoades: definitely interested. slack in emacs is...
    ;; •  14:35  🧵 CH-THREAD  Tony Akens  in  #ip4g-product-dev : if y'all don't have access to gr...
    (insert "  " unread-marker " " time-part "  " badge-part "  " sender-part channel-part ": " snippet-part)
    
    ;; Apply text properties to the row (excluding newline) for clicks
    (add-text-properties row-start (point)
                         (list 'taut-inbox-item item
                               'mouse-face 'highlight))
    (insert "\n")))

(defun taut-inbox--item-matches-date-filter-p (item)
  "Return non-nil if ITEM matches the current `taut-inbox-date-filter`."
  (let ((ts (taut-inbox-item-ts item)))
    (if (or (null taut-inbox-date-filter) (eq taut-inbox-date-filter 'all))
        t
      (if (and ts (string-match "^\\([0-9]+\\)" ts))
          (let* ((epoch (string-to-number (match-string 1 ts)))
                 (now (float-time))
                 (time-val (seconds-to-time epoch))
                 (now-val (seconds-to-time now))
                 (item-days (time-to-days time-val))
                 (now-days (time-to-days now-val))
                 (day-diff (- now-days item-days)))
            (cond
             ((eq taut-inbox-date-filter 'today)
              (<= day-diff 0))
             ((eq taut-inbox-date-filter 'last-7)
              (< day-diff 7))
             ((eq taut-inbox-date-filter 'last-30)
              (< day-diff 30))
             (t t)))
        nil))))

(defun taut-inbox--format-relative-date (ts-str)
  "Format Slack timestamp TS-STR into a short time representation (HH:MM)."
  (if (and ts-str (string-match "^\\([0-9]+\\)" ts-str))
      (let* ((epoch (string-to-number (match-string 1 ts-str)))
             (time-val (seconds-to-time epoch)))
        (format-time-string "%H:%M" time-val))
    "--:--"))

(defun taut-inbox--format-date-group (ts-str)
  "Get the date grouping header for TS-STR.
Categorizes timestamps into Today, Yesterday, Weekday, or Month."
  (if (and ts-str (string-match "^\\([0-9]+\\)" ts-str))
      (let* ((epoch (string-to-number (match-string 1 ts-str)))
             (now (float-time))
             (time-val (seconds-to-time epoch))
             (now-val (seconds-to-time now))
             (item-days (time-to-days time-val))
             (now-days (time-to-days now-val))
             (day-diff (- now-days item-days)))
        (cond
         ((<= day-diff 0) "Today")
         ((= day-diff 1) "Yesterday")
         ((< day-diff 7) (format-time-string "%A" time-val))
         (t
          (let ((item-year (format-time-string "%Y" time-val))
                (current-year (format-time-string "%Y" now-val)))
            (if (equal item-year current-year)
                (format-time-string "%B %d" time-val)
              (format-time-string "%B %d, %Y" time-val))))))
    "Older Activity"))

(defun taut-inbox-date-filter-today ()
  "Filter inbox to show only today's activity."
  (interactive)
  (setq taut-inbox-date-filter 'today)
  (taut-inbox-refresh))

(defun taut-inbox-date-filter-last-7 ()
  "Filter inbox to show only last 7 days' activity."
  (interactive)
  (setq taut-inbox-date-filter 'last-7)
  (taut-inbox-refresh))

(defun taut-inbox-date-filter-last-30 ()
  "Filter inbox to show only last 30 days' activity."
  (interactive)
  (setq taut-inbox-date-filter 'last-30)
  (taut-inbox-refresh))

(defun taut-inbox-date-filter-all ()
  "Filter inbox to show all activity."
  (interactive)
  (setq taut-inbox-date-filter 'all)
  (taut-inbox-refresh))

(defun taut-inbox--clean-snippet (text)
  "Clean and truncate TEXT for display as an inbox snippet."
  (if text
      (let* ((clean (replace-regexp-in-string "[\n\r\t ]+" " " text))
             ;; Translate user mentions (e.g. <@U12345> or <@U12345|alice>)
             (clean (replace-regexp-in-string
                     "<@\\([^>|]+\\)\\(|\\([^>]+\\)\\)?>"
                     (lambda (m)
                       (let* ((uid (match-string 1 m))
                              (label (match-string 3 m))
                              (user (gethash uid taut-users))
                              (username (or label (if user (taut-user-username user) uid) uid)))
                         (format "@%s" username)))
                     clean
                     t))
             ;; Translate channel mentions (e.g. <#C12345> or <#C12345|general>)
             (clean (replace-regexp-in-string
                     "<#\\([^>|]+\\)\\(|\\([^>]+\\)\\)?>"
                     (lambda (m)
                       (let* ((cid (match-string 1 m))
                              (label (match-string 3 m))
                              (chan (taut-model-get-channel cid))
                              (name (or label (if chan (taut-channel-name chan) cid) cid)))
                         (format "#%s" name)))
                     clean
                     t))
             ;; Translate standard Slack links (e.g. <https://github.com|github> or <https://github.com>)
             (clean (replace-regexp-in-string
                     "<\\([^@#!>|][^>|]*\\)\\(|\\([^>]+\\)\\)?>"
                     (lambda (m)
                       (let ((url (match-string 1 m))
                             (label (match-string 3 m)))
                         (or label url)))
                     clean
                     t))
             ;; Translate emoji shortcodes (e.g. :thumbsup:)
             (clean (replace-regexp-in-string
                     ":\\([a-zA-Z0-9_+-]+\\):"
                     (lambda (m)
                       (let ((emoji-name (match-string 1 m)))
                         (save-match-data
                           (taut-emoji-translate emoji-name))))
                     clean
                     t))
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

(defun taut-inbox--move-to-next-item ()
  "Move point to the next line with a non-nil 'taut-inbox-item property.
Returns the item if found, nil otherwise."
  (let ((found nil)
        (moved t))
    (while (and (not found) moved)
      (setq moved (= (forward-line 1) 0))
      (when (and moved (not (eobp)))
        (let ((item (get-text-property (point) 'taut-inbox-item)))
          (when item
            (setq found item)))))
    found))

(defun taut-inbox--move-to-prev-item ()
  "Move point to the previous line with a non-nil 'taut-inbox-item property.
Returns the item if found, nil otherwise."
  (let ((found nil)
        (moved t))
    (while (and (not found) moved)
      (setq moved (= (forward-line -1) 0))
      (when moved
        (let ((item (get-text-property (point) 'taut-inbox-item)))
          (when item
            (setq found item)))))
    found))

;;;###autoload
(defun taut-inbox-next ()
  "Move to the next visible message in the Slack Inbox buffer and activate it."
  (interactive)
  (let ((buf (get-buffer "*Slack Inbox*")))
    (if (null buf)
        (message "Slack Inbox buffer does not exist.")
      (let ((win (get-buffer-window buf t)))
        (if (null win)
            (message "Slack Inbox window is not visible.")
          (with-selected-window win
            (with-current-buffer buf
              (let ((orig-point (point)))
                (if (taut-inbox--move-to-next-item)
                    (progn
                      (recenter)
                      (taut-inbox-activate))
                  (goto-char orig-point)
                  (message "End of Slack Inbox."))))))))))

;;;###autoload
(defun taut-inbox-prev ()
  "Move to the previous visible message in the Slack Inbox buffer and activate it."
  (interactive)
  (let ((buf (get-buffer "*Slack Inbox*")))
    (if (null buf)
        (message "Slack Inbox buffer does not exist.")
      (let ((win (get-buffer-window buf t)))
        (if (null win)
            (message "Slack Inbox window is not visible.")
          (with-selected-window win
            (with-current-buffer buf
              (let ((orig-point (point)))
                (if (taut-inbox--move-to-prev-item)
                    (progn
                      (recenter)
                      (taut-inbox-activate))
                  (goto-char orig-point)
                  (message "Beginning of Slack Inbox."))))))))))

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

(defun taut-inbox-item-has-code-p (item)
  "Return t if ITEM contains a code block (triple backticks)."
  (let* ((snippet (taut-inbox-item-snippet item))
         (has-code-in-snippet (and snippet (string-match-p "```" snippet))))
    (or has-code-in-snippet
        (let ((msg (taut-model-get-message-by-ts (taut-inbox-item-id item))))
          (and msg
               (taut-message-text msg)
               (string-match-p "```" (taut-message-text msg)))))))

(defun taut-inbox-filter-code ()
  "Show only items containing code blocks in the inbox."
  (interactive)
  (setq taut-inbox-filter 'code)
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
  "Display the Slack Inbox in the active central window."
  (interactive)
  (if (and (boundp 'taut-strict-windows) taut-strict-windows)
      (let ((activity-win (get-buffer-window "*Slack Inbox*")))
        (if activity-win
            (select-window activity-win)
          (taut-setup-strict-windows)
          (setq activity-win (get-buffer-window "*Slack Inbox*"))
          (when activity-win
            (select-window activity-win)))
        (get-buffer "*Slack Inbox*"))
    (taut-ensure-consolidated-workspace)
    (let ((buf (get-buffer-create "*Slack Inbox*"))
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
      buf)))

(defun taut-inbox-bury ()
  "Bury the Slack Inbox buffer."
  (interactive)
  (bury-buffer))

;; Hook auto-updates
(add-hook 'taut-model-updated-hook #'taut-inbox-refresh)

(provide 'taut-inbox)
;;; taut-inbox.el ends here

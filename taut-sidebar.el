;;; taut-sidebar.el --- Elegant Sidebar UI for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us
;; Keywords: comm, slack

;;; Commentary:
;; This file implements the collapsible sidebar for the Taut Slack client,
;; showing Starred conversations, public/private channels, direct messages,
;; and active threads. It hooks into `taut-model-updated-hook` for auto-updates.

;;; Code:

(require 'taut-model)

(declare-function taut-dispatch "taut-transient")
(declare-function taut-inbox-show "taut-inbox")

;;;; Faces

(defface taut-sidebar-header
  '((((background dark))  :foreground "#a0aec0" :weight bold :height 0.9)
    (((background light)) :foreground "#555555" :weight bold :height 0.9)
    (t                    :foreground "#555555" :weight bold :height 0.9))
  "Face for sidebar section headers."
  :group 'taut-faces)

(defface taut-sidebar-channel
  '((((background dark))  :foreground "#cbd5e0" :weight normal)
    (((background light)) :foreground "#1d1c1d" :weight normal)
    (t                    :foreground "#1d1c1d" :weight normal))
  "Face for standard channels in the sidebar."
  :group 'taut-faces)

(defface taut-sidebar-channel-unread
  '((((background dark))  :foreground "#ffffff" :weight bold)
    (((background light)) :foreground "#1264a3" :weight bold)
    (t                    :foreground "#1264a3" :weight bold))
  "Face for channels with unread messages in the sidebar."
  :group 'taut-faces)

(defface taut-sidebar-badge-mention
  '((t :background "#e03e3e" :foreground "#ffffff" :weight bold :box (:line-width (2 . -1) :style flat-button)))
  "Face for mention counts in the sidebar."
  :group 'taut-faces)

(defface taut-sidebar-badge-unread
  '((((background dark))  :foreground "#a0aec0" :weight bold)
    (((background light)) :foreground "#555555" :weight bold)
    (t                    :foreground "#555555" :weight bold))
  "Face for simple unread counts in the sidebar."
  :group 'taut-faces)

(defface taut-sidebar-status-online
  '((t :foreground "#2eb67d"))
  "Face for online indicator."
  :group 'taut-faces)

(defface taut-sidebar-status-away
  '((t :foreground "#ecb22e"))
  "Face for away indicator."
  :group 'taut-faces)

(defface taut-sidebar-status-offline
  '((t :foreground "#8a8a8a"))
  "Face for offline indicator."
  :group 'taut-faces)

;;;; Configuration & State

(defcustom taut-sidebar-width 30
  "Default width of the Taut sidebar."
  :type 'integer
  :group 'taut)

(defcustom taut-use-icons t
  "When non-nil, use icon packages if available (e.g. nerd-icons)."
  :type 'boolean
  :group 'taut)

(defvar taut-sidebar-section-state
  '((starred . t)
    (bookmarks . t)
    (channels . t)
    (dms . t)
    (threads . t)
    (hidden . nil))
  "Alist tracking whether sections are expanded (t) or collapsed (nil).")

;;;; Major Mode Definition

(defvar taut-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'taut-sidebar-activate)
    (define-key map (kbd "<mouse-1>") #'taut-sidebar-mouse-activate)
    (define-key map (kbd "g") #'taut-sidebar-refresh)
    (define-key map (kbd "TAB") #'taut-sidebar-toggle-section-at-point)
    (define-key map (kbd "M") #'taut-sidebar-mark-all-read)
    (define-key map (kbd "h") #'taut-sidebar-toggle-channel-hidden)
    (define-key map (kbd "q") #'taut-sidebar-bury)
    (define-key map (kbd "i") #'taut-inbox-show)
    (define-key map (kbd "?") #'taut-dispatch)
    map)
  "Keymap for `taut-sidebar-mode`.")

(define-derived-mode taut-sidebar-mode special-mode "Taut-Sidebar"
  "Major mode for the Taut collapsible sidebar.

\\{taut-sidebar-mode-map}"
  (setq buffer-read-only t
        truncate-lines t
        cursor-type nil)
  (hl-line-mode 1))

;;;; Rendering Engine

(defun taut-sidebar--get-icon (type)
  "Get icon for TYPE (e.g., `public', `private', `dm', `thread')."
  (or (and taut-use-icons (fboundp 'nerd-icons-octicon)
           (condition-case nil
               (cond
                ((eq type 'public) (concat (nerd-icons-octicon "nf-oct-hash" :face 'taut-sidebar-channel) " "))
                ((eq type 'private) (concat (nerd-icons-octicon "nf-oct-lock" :face 'taut-sidebar-channel) " "))
                ((eq type 'dm) (concat (nerd-icons-octicon "nf-oct-person" :face 'taut-sidebar-channel) " "))
                ((eq type 'thread) (concat (nerd-icons-octicon "nf-oct-comment_discussion" :face 'taut-sidebar-channel) " "))
                ((eq type 'star) (concat (nerd-icons-octicon "nf-oct-star" :face 'warning) " "))
                ((eq type 'bookmark) (concat (nerd-icons-octicon "nf-oct-bookmark" :face 'success) " "))
                ((eq type 'group) (concat (nerd-icons-octicon "nf-oct-people" :face 'taut-sidebar-channel) " ")))
             (error nil)))
      (and taut-use-icons (fboundp 'all-the-icons-octicon)
           (condition-case nil
               (cond
                ((eq type 'public) (concat (all-the-icons-octicon "tag" :face 'taut-sidebar-channel) " "))
                ((eq type 'private) (concat (all-the-icons-octicon "lock" :face 'taut-sidebar-channel) " "))
                ((eq type 'dm) (concat (all-the-icons-octicon "person" :face 'taut-sidebar-channel) " "))
                ((eq type 'thread) (concat (all-the-icons-octicon "comment-discussion" :face 'taut-sidebar-channel) " "))
                ((eq type 'star) (concat (all-the-icons-octicon "star" :face 'warning) " "))
                ((eq type 'bookmark) (concat (all-the-icons-octicon "bookmark" :face 'success) " "))
                ((eq type 'group) (concat (all-the-icons-octicon "people" :face 'taut-sidebar-channel) " ")))
             (error nil)))
      ;; Unicode fallback symbols
      (cond
       ((eq type 'public) "# ")
       ((eq type 'private) "🔒 ")
       ((eq type 'dm) "👤 ")
       ((eq type 'thread) "💬 ")
       ((eq type 'star) "⭐ ")
       ((eq type 'bookmark) "🔖 ")
       ((eq type 'group) "👥 ")
       (t " "))))

(defun taut-sidebar--get-section-label (sym label)
  "Get the label for section SYM. Uses nice icons if available."
  (or (and taut-use-icons (fboundp 'nerd-icons-octicon)
           (condition-case nil
               (cond
                ((eq sym 'starred) (concat (nerd-icons-octicon "nf-oct-star" :face 'warning) " STARRED"))
                ((eq sym 'bookmarks) (concat (nerd-icons-octicon "nf-oct-bookmark" :face 'success) " BOOKMARKS"))
                ((eq sym 'channels) (concat (nerd-icons-octicon "nf-oct-hash" :face 'taut-sidebar-channel) " CHANNELS"))
                ((eq sym 'dms) (concat (nerd-icons-octicon "nf-oct-mail" :face 'taut-sidebar-channel) " DIRECT MESSAGES"))
                ((eq sym 'threads) (concat (nerd-icons-octicon "nf-oct-comment_discussion" :face 'taut-sidebar-channel) " THREADS"))
                ((eq sym 'hidden) (concat (nerd-icons-octicon "nf-oct-eye_closed" :face 'font-lock-comment-face) " HIDDEN")))
             (error nil)))
      (and taut-use-icons (fboundp 'all-the-icons-octicon)
           (condition-case nil
               (cond
                ((eq sym 'starred) (concat (all-the-icons-octicon "star" :face 'warning) " STARRED"))
                ((eq sym 'bookmarks) (concat (all-the-icons-octicon "bookmark" :face 'success) " BOOKMARKS"))
                ((eq sym 'channels) (concat (all-the-icons-octicon "tag" :face 'taut-sidebar-channel) " CHANNELS"))
                ((eq sym 'dms) (concat (all-the-icons-octicon "mail" :face 'taut-sidebar-channel) " DIRECT MESSAGES"))
                ((eq sym 'threads) (concat (all-the-icons-octicon "comment-discussion" :face 'taut-sidebar-channel) " THREADS"))
                ((eq sym 'hidden) (concat (all-the-icons-octicon "eye-closed" :face 'font-lock-comment-face) " HIDDEN")))
             (error nil)))
      label))

(defun taut-sidebar-refresh ()
  "Redraw the sidebar buffer contents if it exists."
  (interactive)
  (let ((buf (get-buffer "*Taut Sidebar*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (old-point (point)))
          (erase-buffer)
          (taut-sidebar--render-sections)
          (goto-char (min old-point (point-max))))))))

(defun taut-sidebar--get-inbox-unread-count ()
  "Count total unread items in the activity feed."
  (if (fboundp 'taut-model-get-activity-items)
      (let ((items (taut-model-get-activity-items)))
        (cl-count-if-not #'taut-inbox-item-is-read items))
    0))

(defun taut-sidebar--thread-is-hidden-p (th-ts)
  "Return non-nil if thread TH-TS is from a hidden channel."
  (let ((chan-id nil))
    ;; Try replies first
    (let ((replies (gethash th-ts taut-threads)))
      (when replies
        (setq chan-id (taut-message-channel-id (car replies)))))
    ;; Fallback to searching taut-messages
    (unless chan-id
      (maphash (lambda (cid msgs)
                 (when (cl-some (lambda (msg) (equal (taut-message-ts msg) th-ts)) msgs)
                   (setq chan-id cid)))
               taut-messages))
    (when chan-id
      (let ((chan (taut-model-get-channel chan-id)))
        (and chan (taut-channel-is-hidden chan))))))

(defun taut-sidebar--render-sections ()
  "Render all sections to the current buffer."
  ;; Add a line or two of space at the top of the sidebar
  (insert "\n\n")
  ;; Render the Slack Activity shortcut with unread badge at the top
  (let* ((inbox-unread-count (taut-sidebar--get-inbox-unread-count))
         (has-unreads (> inbox-unread-count 0))
         (face (if has-unreads 'taut-sidebar-channel-unread 'taut-sidebar-channel))
         (icon (or (and taut-use-icons (fboundp 'nerd-icons-octicon)
                        (concat (nerd-icons-octicon "nf-oct-inbox" :face face) " "))
                   (and taut-use-icons (fboundp 'all-the-icons-octicon)
                        (concat (all-the-icons-octicon "inbox" :face face) " "))
                   "📥 ")))
    (let ((start (point)))
      (insert "  " icon (propertize "Slack Activity" 'face face))
      (when has-unreads
        (insert (propertize (format " (%d)" inbox-unread-count)
                            'face 'taut-sidebar-badge-unread)))
      (add-text-properties start (point)
                           (list 'taut-sidebar-action #'taut-inbox-show
                                 'mouse-face 'highlight)))
    (insert "\n\n"))
  (let ((channels (taut-model-get-channels-list))
        starred public-chans dms hidden-chans
        normal-threads hidden-threads)
    
    ;; Split channels into logical lists
    (dolist (chan channels)
      (cond
       ((taut-channel-is-hidden chan) (push chan hidden-chans))
       ((taut-channel-is-starred chan) (push chan starred))
       ((eq (taut-channel-type chan) 'dm)
        (when (or (> (or (taut-channel-unread-count chan) 0) 0)
                  (> (or (taut-channel-mention-count chan) 0) 0)
                  (taut-model-channel-active-last-30-days-p (taut-channel-id chan)))
          (push chan dms)))
       (t (push chan public-chans))))

    ;; Split threads into normal and hidden
    (dolist (th-ts taut-watched-threads)
      (if (taut-sidebar--thread-is-hidden-p th-ts)
          (push th-ts hidden-threads)
        (push th-ts normal-threads)))

    (setq starred (nreverse starred)
          public-chans (nreverse public-chans)
          dms (nreverse dms)
          hidden-chans (nreverse hidden-chans)
          normal-threads (nreverse normal-threads)
          hidden-threads (nreverse hidden-threads))

    ;; Render each section
    (taut-sidebar--render-section 'starred "★ STARRED" starred)
    (taut-sidebar--render-section 'channels "♯ CHANNELS" public-chans)
    (taut-sidebar--render-section 'dms "✉ DIRECT MESSAGES" dms)
    (taut-sidebar--render-bookmarks)
    (taut-sidebar--render-section-threads normal-threads)
    (taut-sidebar--render-section-hidden hidden-chans hidden-threads)))

(defun taut-sidebar--render-section (sym label items)
  "Render a single collapsible section identified by SYM, with LABEL and ITEMS."
  (let* ((expanded (alist-get sym taut-sidebar-section-state))
         (indicator (if expanded "▼" "▶"))
         (display-label (taut-sidebar--get-section-label sym label)))
    ;; Insert Header
    (insert (propertize (format "%s %s\n" indicator display-label)
                        'face 'taut-sidebar-header
                        'mouse-face 'highlight
                        'taut-section sym))
    
    (when expanded
      (if (null items)
          (insert "  (none)\n")
        (dolist (chan items)
          (taut-sidebar--render-channel-line chan))))
    (insert "\n")))

(defun taut-sidebar--resolve-display-names (chan-name)
  "Resolve comma-separated usernames to real names if available."
  (let* ((parts (split-string chan-name ",\\s-*"))
         (resolved (mapcar (lambda (part)
                             (let ((user (taut-model-get-user-by-username part)))
                               (if user
                                   (taut-user-real-name user)
                                 part)))
                           parts)))
    resolved))

(defun taut-sidebar--render-channel-line (chan)
  "Insert a stylized line representing CHAN in the sidebar."
  (let* ((has-unreads (> (taut-channel-unread-count chan) 0))
         (has-mentions (> (taut-channel-mention-count chan) 0))
         (channel-face (if has-unreads 'taut-sidebar-channel-unread 'taut-sidebar-channel))
         (chan-name (or (taut-channel-name chan) "unknown"))
         (chan-line-start (point))
         (resolved-names (taut-sidebar--resolve-display-names chan-name))
         (is-dm (eq (taut-channel-type chan) 'dm)))
    
    (if (and is-dm (> (length resolved-names) 1))
        ;; Multi-participant DM: Render names separated by newlines.
        (progn
          (insert "  " (taut-sidebar--get-icon 'group))
          (insert (propertize (car resolved-names) 'face channel-face))
          (dolist (name (cdr resolved-names))
            (insert "\n     ") ; Indented to line up with text under group icon
            (insert (propertize name 'face channel-face))))
      ;; Single participant DM or standard channel
      (let ((name-prefix (if is-dm
                             (let ((user (taut-model-get-user-by-username chan-name)))
                               (if user
                                   (taut-sidebar--user-status-indicator user)
                                 (taut-sidebar--get-icon 'dm)))
                           (taut-sidebar--get-icon (taut-channel-type chan)))))
        (insert "  " name-prefix)
        (insert (propertize (car resolved-names) 'face channel-face))))
    
    ;; Append Badge if there are unreads/mentions
    (cond
     (has-mentions
       (insert (propertize (format " %d " (taut-channel-mention-count chan))
                           'face 'taut-sidebar-badge-mention)))
     (has-unreads
      (insert (propertize (format " (%d)" (taut-channel-unread-count chan))
                          'face 'taut-sidebar-badge-unread))))
    
    ;; Add properties for clicking/activating
    (add-text-properties chan-line-start (point)
                         (list 'taut-channel-id (taut-channel-id chan)
                               'mouse-face 'highlight))
    (insert "\n")))

(defun taut-sidebar--render-section-threads (threads)
  "Render the Threads section with specified THREADS."
  (let* ((sym 'threads)
         (expanded (alist-get sym taut-sidebar-section-state))
         (indicator (if expanded "▼" "▶"))
         (display-label (taut-sidebar--get-section-label sym "THREADS")))
    (insert (propertize (format "%s %s\n" indicator display-label)
                        'face 'taut-sidebar-header
                        'mouse-face 'highlight
                        'taut-section sym))
    (when expanded
      (if (null threads)
          (insert "  (no watched threads)\n")
        (dolist (th-ts threads)
          ;; Render a summary line for each watched thread
          (let* ((replies (gethash th-ts taut-threads))
                 (unread-reply-count (cl-count-if #'taut-message-is-unread replies))
                 (has-unreads (> unread-reply-count 0))
                 (line-start (point))
                 (chan-id (and replies (taut-message-channel-id (car replies)))))
            (insert "  " (if has-unreads "● " (taut-sidebar--get-icon 'thread)))
            (let ((ts-suffix (if (and th-ts (>= (length th-ts) 5)) (substring th-ts -5) (or th-ts ""))))
              (insert (propertize (format "Thread %s" ts-suffix)
                                  'face (if has-unreads 'taut-sidebar-channel-unread 'taut-sidebar-channel))))
            (when has-unreads
              (insert (propertize (format " (%d)" unread-reply-count)
                                  'face 'taut-sidebar-badge-unread)))
            (add-text-properties line-start (point)
                                 (list 'taut-thread-ts th-ts
                                       'taut-channel-id chan-id
                                       'mouse-face 'highlight))
            (insert "\n")))))
    (insert "\n")))

(defun taut-sidebar--render-section-hidden (hidden-chans hidden-threads)
  "Render the HIDDEN section, displaying HIDDEN-CHANS and HIDDEN-THREADS."
  (let* ((sym 'hidden)
         (expanded (alist-get sym taut-sidebar-section-state))
         (indicator (if expanded "▼" "▶"))
         (display-label (taut-sidebar--get-section-label sym "HIDDEN")))
    (insert (propertize (format "%s %s\n" indicator display-label)
                        'face 'taut-sidebar-header
                        'mouse-face 'highlight
                        'taut-section sym))
    (when expanded
      (if (and (null hidden-chans) (null hidden-threads))
          (insert "  (none)\n")
        ;; Render channels
        (dolist (chan hidden-chans)
          (taut-sidebar--render-channel-line chan))
        ;; Render threads from hidden channels
        (dolist (th-ts hidden-threads)
          (let* ((replies (gethash th-ts taut-threads))
                 (unread-reply-count (cl-count-if #'taut-message-is-unread replies))
                 (has-unreads (> unread-reply-count 0))
                 (line-start (point))
                 (chan-id (and replies (taut-message-channel-id (car replies)))))
            (insert "  " (if has-unreads "● " (taut-sidebar--get-icon 'thread)))
            (let ((ts-suffix (if (and th-ts (>= (length th-ts) 5)) (substring th-ts -5) (or th-ts ""))))
              (insert (propertize (format "Thread %s" ts-suffix)
                                  'face (if has-unreads 'taut-sidebar-channel-unread 'taut-sidebar-channel))))
            (when has-unreads
              (insert (propertize (format " (%d)" unread-reply-count)
                                  'face 'taut-sidebar-badge-unread)))
            (add-text-properties line-start (point)
                                 (list 'taut-thread-ts th-ts
                                       'taut-channel-id chan-id
                                       'mouse-face 'highlight))
            (insert "\n")))))
    (insert "\n")))

(defun taut-sidebar--user-status-indicator (user)
  "Return a stylized string indicating USER's status."
  (let ((presence (and user (taut-user-presence user))))
    (cond
     ((eq presence 'online)  (propertize "● " 'face 'taut-sidebar-status-online))
     ((eq presence 'away)    (propertize "○ " 'face 'taut-sidebar-status-away))
     (t                      (propertize "○ " 'face 'taut-sidebar-status-offline)))))

;;;; Interaction Handlers

(defun taut-sidebar-mark-all-read ()
  "Mark all messages in the current channel under point as read."
  (interactive)
  (let ((chan-id (get-text-property (point) 'taut-channel-id)))
    (if (null chan-id)
        (message "No channel at point.")
      (taut-model-mark-channel-read chan-id)
      (message "Marked all messages in channel as read.")
      (taut-sidebar-refresh))))

(defun taut-sidebar-toggle-channel-hidden ()
  "Toggle the hidden status of the channel at point."
  (interactive)
  (let ((chan-id (get-text-property (point) 'taut-channel-id)))
    (if (null chan-id)
        (message "No channel at point.")
      (let ((chan (taut-model-get-channel chan-id)))
        (if (null chan)
            (message "No channel found for %s" chan-id)
          (let ((new-state (not (taut-channel-is-hidden chan))))
            (setf (taut-channel-is-hidden chan) new-state)
            (when (fboundp 'taut-cache-save-channel)
              (taut-cache-save-channel chan))
            (message "Channel '%s' is now %s."
                     (taut-channel-name chan)
                     (if new-state "hidden" "visible"))
            (taut-sidebar-refresh)))))))

(defun taut-sidebar-activate ()
  "Handle selection of whatever is under the cursor."
  (interactive)
  (let ((chan-id (get-text-property (point) 'taut-channel-id))
        (thread-ts (get-text-property (point) 'taut-thread-ts))
        (bookmark-msg (get-text-property (point) 'taut-bookmark-msg))
        (section (get-text-property (point) 'taut-section))
        (action (get-text-property (point) 'taut-sidebar-action)))
    (cond
     (action
      (call-interactively action))
     (bookmark-msg
      (taut-sidebar-open-bookmark bookmark-msg))
     (chan-id
      (taut-sidebar-open-channel chan-id))
     (thread-ts
      (taut-sidebar-open-thread thread-ts chan-id))
     (section
      (taut-sidebar-toggle-section section)))))

(defun taut-sidebar-mouse-activate (event)
  "Handle mouse click EVENT in sidebar."
  (interactive "e")
  (posn-set-point (event-end event))
  (taut-sidebar-activate))

(defun taut-sidebar-toggle-section-at-point ()
  "Toggle section expansion if cursor is on a section header."
  (interactive)
  (let ((section (get-text-property (point) 'taut-section)))
    (if section
        (taut-sidebar-toggle-section section)
      (message "Not on a section header."))))

(defun taut-sidebar-toggle-section (sym)
  "Toggle expansion of section SYM."
  (let ((curr (alist-get sym taut-sidebar-section-state)))
    (setf (alist-get sym taut-sidebar-section-state) (not curr))
    (taut-sidebar-refresh)))

(defun taut-sidebar--render-bookmarks ()
  "Render the Bookmarks section."
  (let* ((sym 'bookmarks)
         (expanded (alist-get sym taut-sidebar-section-state))
         (indicator (if expanded "▼" "▶"))
         (display-label (taut-sidebar--get-section-label sym "BOOKMARKS"))
         (items (taut-model-get-starred-messages)))
    (insert (propertize (format "%s %s\n" indicator display-label)
                        'face 'taut-sidebar-header
                        'mouse-face 'highlight
                        'taut-section sym))
    (when expanded
      (if (null items)
          (insert "  (no bookmarks)\n")
        (dolist (msg items)
          (let* ((user (taut-model-get-user (taut-message-user-id msg)))
                 (username (if user (or (taut-user-username user) "unknown") "unknown"))
                 (text (taut-message-text msg))
                 ;; Clean up text: replace newlines with spaces and
                 ;; limit snippet size.
                 (snippet (replace-regexp-in-string "\n" " " (or text "")))
                 (snippet (if (> (length snippet) 30) (concat (substring snippet 0 27) "...") snippet))
                 (line-start (point)))
            (insert "  " (taut-sidebar--get-icon 'star))
            (insert (propertize (format "@%s: " username) 'face 'font-lock-comment-face))
            (insert (propertize snippet 'face 'taut-sidebar-channel))
            (add-text-properties line-start (point)
                                 (list 'taut-bookmark-msg msg
                                       'mouse-face 'highlight))
            (insert "\n")))))
    (insert "\n")))

(defun taut-sidebar-open-bookmark (msg)
  "Open the conversation containing MSG and move point to it."
  (let* ((chan-id (taut-message-channel-id msg))
         (thread-ts (taut-message-thread-ts msg))
         (msg-ts (taut-message-ts msg)))
    (if (not chan-id)
        (message "Error: Bookmarked message has no channel reference.")
      (let ((buf (taut-sidebar-open-channel chan-id)))
        (when (buffer-live-p buf)
          (let ((win (get-buffer-window buf)))
            (when win
              (select-window win))
            (with-current-buffer buf
              (let ((pos (point-min))
                    (found nil))
                (while (and (not found) (< pos (point-max)))
                  (let ((next-pos (next-single-property-change pos 'taut-message-ts)))
                    (if (equal (get-text-property pos 'taut-message-ts) msg-ts)
                        (progn
                          (goto-char pos)
                          (setq found t))
                      (setq pos (or next-pos (point-max))))))
                ;; If the message wasn't found in the main channel history (e.g. it is inside a thread reply),
                ;; and this is a thread reply, open the thread side panel which will fetch and display it!
                (when (and thread-ts (not (equal thread-ts msg-ts)))
                  (taut-sidebar-open-thread thread-ts))))))))))

(defun taut-sidebar-open-channel (chan-id)
  "Open conversation buffer for CHAN-ID in the adjacent window."
  (if (fboundp 'taut-message-open)
      (funcall 'taut-message-open chan-id)
    (message "Opening channel %s (taut-message-open not loaded yet)" chan-id)))

(defun taut-sidebar-open-thread (thread-ts &optional channel-id)
  "Open thread discussion buffer for THREAD-TS."
  (if (fboundp 'taut-thread-open)
      (funcall 'taut-thread-open thread-ts channel-id)
    (message "Opening thread %s (taut-thread-open not loaded yet)" thread-ts)))

;;;; Sidebar Show / Management

(defun taut-sidebar-show ()
  "Launch or display the Taut Sidebar."
  (interactive)
  (let ((buf (get-buffer-create "*Taut Sidebar*")))
    (with-current-buffer buf
      (unless (eq major-mode 'taut-sidebar-mode)
        (taut-sidebar-mode)))
    
    ;; Split frame to display sidebar on the left
    (let ((window (get-buffer-window buf)))
      (if window
          ;; Ensure already open sidebar has the correct width
          (let ((delta (- taut-sidebar-width (window-total-width window))))
            (when (/= delta 0)
              (ignore-errors
                (window-resize window delta t))))
        (let ((left-window (split-window (frame-root-window) taut-sidebar-width 'left)))
          (set-window-buffer left-window buf)
          (set-window-dedicated-p left-window t)
          (let ((delta (- taut-sidebar-width (window-total-width left-window))))
            (when (/= delta 0)
              (ignore-errors
                (window-resize left-window delta t))))
          (setq window left-window)))
      (taut-sidebar-refresh)
      (select-window window)
      window)))

(defun taut-sidebar-bury ()
  "Bury or hide the Taut Sidebar."
  (interactive)
  (let ((buf (get-buffer "*Taut Sidebar*")))
    (when buf
      (let ((window (get-buffer-window buf)))
        (if window
            (delete-window window)
          (bury-buffer buf))))))

;; Hook sidebar auto-updates
(add-hook 'taut-model-updated-hook #'taut-sidebar-refresh)

(provide 'taut-sidebar)
;;; taut-sidebar.el ends here

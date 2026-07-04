;;; taut-sidebar.el --- Elegant Sidebar UI for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Google DeepMind

;; Author: Antigravity
;; Keywords: comm, slack

;;; Commentary:
;; This file implements the collapsible sidebar for the Taut Slack client,
;; showing Starred conversations, public/private channels, direct messages,
;; and active threads. It hooks into `taut-model-updated-hook` for auto-updates.

;;; Code:

(require 'taut-model)

(declare-function taut-dispatch "taut-transient")

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

(defvar taut-sidebar-section-state
  '((starred . t)
    (channels . t)
    (dms . t)
    (threads . t))
  "Alist tracking whether sections are expanded (t) or collapsed (nil).")

;;;; Major Mode Definition

(defvar taut-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'taut-sidebar-activate)
    (define-key map (kbd "<mouse-1>") #'taut-sidebar-mouse-activate)
    (define-key map (kbd "g") #'taut-sidebar-refresh)
    (define-key map (kbd "TAB") #'taut-sidebar-toggle-section-at-point)
    (define-key map (kbd "q") #'taut-sidebar-bury)
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

(defun taut-sidebar--render-sections ()
  "Render all sections to the current buffer."
  (let ((channels (taut-model-get-channels-list))
        starred public-chans dms)
    
    ;; Split channels into logical lists
    (dolist (chan channels)
      (cond
       ((taut-channel-is-starred chan) (push chan starred))
       ((eq (taut-channel-type chan) 'dm) (push chan dms))
       (t (push chan public-chans))))

    (setq starred (nreverse starred)
          public-chans (nreverse public-chans)
          dms (nreverse dms))

    ;; Render each section
    (taut-sidebar--render-section 'starred "★ STARRED" starred)
    (taut-sidebar--render-section 'channels "♯ CHANNELS" public-chans)
    (taut-sidebar--render-section 'dms "✉ DIRECT MESSAGES" dms)
    (taut-sidebar--render-section-threads)))

(defun taut-sidebar--render-section (sym label items)
  "Render a single collapsible section identified by SYM, with LABEL and ITEMS."
  (let* ((expanded (alist-get sym taut-sidebar-section-state))
         (indicator (if expanded "▼" "▶")))
    ;; Insert Header
    (insert (propertize (format "%s %s\n" indicator label)
                        'face 'taut-sidebar-header
                        'mouse-face 'highlight
                        'taut-section sym))
    
    (when expanded
      (if (null items)
          (insert "  (none)\n")
        (dolist (chan items)
          (taut-sidebar--render-channel-line chan))))
    (insert "\n")))

(defun taut-sidebar--render-channel-line (chan)
  "Insert a stylized line representing CHAN in the sidebar."
  (let* ((has-unreads (> (taut-channel-unread-count chan) 0))
         (has-mentions (> (taut-channel-mention-count chan) 0))
         (channel-face (if has-unreads 'taut-sidebar-channel-unread 'taut-sidebar-channel))
         (chan-name (or (taut-channel-name chan) "unknown"))
         (name-prefix (if (eq (taut-channel-type chan) 'dm)
                          (let ((user (taut-model-get-user-by-username chan-name)))
                            (taut-sidebar--user-status-indicator user))
                        "# "))
         (chan-line-start (point)))
    
    (insert "  " name-prefix)
    (insert (propertize chan-name 'face channel-face))
    
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

(defun taut-sidebar--render-section-threads ()
  "Render the Threads section separately."
  (let* ((sym 'threads)
         (expanded (alist-get sym taut-sidebar-section-state))
         (indicator (if expanded "▼" "▶")))
    (insert (propertize (format "%s 💬 THREADS\n" indicator)
                        'face 'taut-sidebar-header
                        'mouse-face 'highlight
                        'taut-section sym))
    (when expanded
      (if (null taut-watched-threads)
          (insert "  (no watched threads)\n")
        (dolist (th-ts taut-watched-threads)
          ;; Render a summary line for each watched thread
          (let* ((replies (gethash th-ts taut-threads))
                 (unread-reply-count (cl-count-if #'taut-message-is-unread replies))
                 (has-unreads (> unread-reply-count 0))
                 (line-start (point)))
            (insert "  " (if has-unreads "● " "  "))
            (let ((ts-suffix (if (and th-ts (>= (length th-ts) 5)) (substring th-ts -5) (or th-ts ""))))
              (insert (propertize (format "Thread %s" ts-suffix)
                                  'face (if has-unreads 'taut-sidebar-channel-unread 'taut-sidebar-channel))))
            (when has-unreads
              (insert (propertize (format " (%d)" unread-reply-count)
                                  'face 'taut-sidebar-badge-unread)))
            (add-text-properties line-start (point)
                                 (list 'taut-thread-ts th-ts
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

(defun taut-sidebar-activate ()
  "Handle selection of whatever is under the cursor."
  (interactive)
  (let ((chan-id (get-text-property (point) 'taut-channel-id))
        (thread-ts (get-text-property (point) 'taut-thread-ts))
        (section (get-text-property (point) 'taut-section)))
    (cond
     (chan-id
      (taut-sidebar-open-channel chan-id))
     (thread-ts
      (taut-sidebar-open-thread thread-ts))
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

(defun taut-sidebar-open-channel (chan-id)
  "Open conversation buffer for CHAN-ID in the adjacent window."
  (if (fboundp 'taut-message-open)
      (funcall 'taut-message-open chan-id)
    (message "Opening channel %s (taut-message-open not loaded yet)" chan-id)))

(defun taut-sidebar-open-thread (thread-ts)
  "Open thread discussion buffer for THREAD-TS."
  (if (fboundp 'taut-thread-open)
      (funcall 'taut-thread-open thread-ts)
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

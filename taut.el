;;; taut.el --- Modern, elegant Slack client entry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: comm, slack

;;; Commentary:
;; `taut` is a lightweight, elegant, and UX-focused Slack client for Emacs.
;; This file serves as the main orchestrator, setting up the coordinate windows
;; layout and offering an interactive Mock Driver for UI demonstration and testing.

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

(require 'taut-model)
(require 'taut-cache)

(defcustom taut-websocket-load-path "/Users/bunnylushington/.emacs.d/straight/build/websocket/"
  "Directory path to the `websocket' library installation."
  :type 'directory
  :group 'taut)

;; Ensure websocket is in the load-path before we require taut-socket
(when (and taut-websocket-load-path (file-directory-p taut-websocket-load-path))
  (add-to-list 'load-path taut-websocket-load-path))
(require 'taut-sidebar)
(require 'taut-inbox)
(require 'taut-message)
(require 'taut-thread)
(require 'taut-api)
(require 'taut-socket)
(require 'taut-transient)
(require 'taut-compose)
(require 'taut-search)

;;;; Global Keybindings for Jumper
(define-key taut-sidebar-mode-map (kbd "j") #'taut-jump)
(define-key taut-inbox-mode-map (kbd "j") #'taut-jump)
(define-key taut-message-mode-map (kbd "j") #'taut-jump)
(define-key taut-thread-mode-map (kbd "j") #'taut-jump)

;;;; Global Keybindings for navigation
(define-key taut-sidebar-mode-map (kbd "S") #'taut-sidebar-show)
(define-key taut-sidebar-mode-map (kbd "I") #'taut-inbox-show)
(define-key taut-sidebar-mode-map (kbd "C") #'taut-focus-chat)

(define-key taut-inbox-mode-map (kbd "S") #'taut-sidebar-show)
(define-key taut-inbox-mode-map (kbd "I") #'taut-inbox-show)
(define-key taut-inbox-mode-map (kbd "C") #'taut-focus-chat)

(define-key taut-message-mode-map (kbd "S") #'taut-sidebar-show)
(define-key taut-message-mode-map (kbd "I") #'taut-inbox-show)
(define-key taut-message-mode-map (kbd "C") #'taut-focus-chat)

(define-key taut-thread-mode-map (kbd "S") #'taut-sidebar-show)
(define-key taut-thread-mode-map (kbd "I") #'taut-inbox-show)
(define-key taut-thread-mode-map (kbd "C") #'taut-focus-chat)

;;;; Global Minor Mode / Initialization Commands

;;;###autoload
(defalias 'taut #'taut-connect)

;;;###autoload
(defun taut-connect ()
  "Connect Taut to the real Slack API and start the live workspace."
  (interactive)
  (taut-api-load-tokens-from-authinfo)
  (unless taut-bot-token
    (setq taut-bot-token (read-string "Enter Slack Token (xoxp-... or xoxb-...): ")))
  
  (message "Taut: Connecting to Slack...")
  
  ;; Load from SQLite cache if available for instant startup experience
  (let ((has-cache (and (fboundp 'taut-cache--available-p) (taut-cache--available-p))))
    (when has-cache
      (taut-cache-load-all)
      ;; Display layout instantly while we sync in the background
      (delete-other-windows)
      (taut-sidebar-show)
      (taut-inbox-show)
      (redisplay t))

    (condition-case err
        (progn
          ;; Test Auth and set our user ID
          (taut-api-test-auth)
          
          ;; If we didn't have cache, clear memory. Otherwise, we keep memory
          ;; and sync updates incrementally!
          (unless has-cache
            (taut-model-clear-all))
          
          ;; Fetch live workspace updates
          (taut-api-fetch-users)
          (taut-api-fetch-channels)
          (taut-api-fetch-active-presences)
          (taut-api-fetch-starred)
          (taut-api-fetch-inbox-history)
          
          ;; If app token is configured, establish Socket Mode WebSocket connection
          (when taut-app-token
            (ignore-errors (taut-socket-connect)))
          
          ;; Split and display layout (if not already displayed)
          (unless has-cache
            (delete-other-windows)
            (taut-sidebar-show)
            (taut-inbox-show))
          (message "Taut: Successfully connected! Click/RET on a channel to read it."))
      (error
       ;; If background sync failed but we have cache loaded, don't crash
       (if has-cache
           (message "Taut Warning: Live sync failed (%s), operating in offline/cached mode." (error-message-string err))
         (error "Taut Connection Failed: %s" (error-message-string err)))))))

;;;###autoload
(defun taut-inbox ()
  "Open or focus the Taut Unified Inbox workspace."
  (interactive)
  (taut-inbox-show))

;;;###autoload
(defun taut-sidebar ()
  "Toggle or focus the Taut Sidebar."
  (interactive)
  (taut-sidebar-show))

;;;###autoload
(defun taut-jump ()
  "Jump to any Slack channel, group, or DM using interactive completion."
  (interactive)
  (let* ((channels (taut-model-get-channels-list))
         (candidates nil))
    (dolist (chan channels)
      (let* ((chan-name (taut-channel-name chan))
             (chan-id (taut-channel-id chan))
             (chan-type (taut-channel-type chan))
             (unreads (taut-channel-unread-count chan))
             (mentions (taut-channel-mention-count chan))
             (starred (taut-channel-is-starred chan))
             ;; Indicators
             (star-indicator (if starred "★ " "  "))
             (type-indicator (cond
                              ((eq chan-type 'dm)
                               (let* ((user (taut-model-get-user-by-username chan-name))
                                      (presence (and user (taut-user-presence user))))
                                 (cond
                                  ((eq presence 'online) "● ")
                                  ((eq presence 'away)   "○ ")
                                  (t                     "  "))))
                              ((eq chan-type 'private) "🔒 ")
                              (t "# ")))
             ;; Badges
             (badge (cond
                     ((and mentions (> mentions 0)) (format " (%d mentions! ✉)" mentions))
                     ((and unreads (> unreads 0)) (format " (%d unreads)" unreads))
                     (t "")))
             ;; Complete formatted string
             (display-string (format "%s%s%s%s" star-indicator type-indicator chan-name badge)))
        (push (cons display-string chan-id) candidates)))
    (if (null candidates)
        (message "Taut: No channels or DMs available to jump to.")
      (let* ((reversed-candidates (nreverse candidates))
             (choice (completing-read "Jump to Channel/DM: " reversed-candidates nil t))
             (chan-id (cdr (assoc choice reversed-candidates))))
        (when chan-id
          (taut-message-open chan-id)
          (message "Jumped to %s!" choice))))))

;;;###autoload
(defun taut-quit ()
  "Hard quit Taut.
Stop simulators, close WebSocket, kill buffers, and restore windows."
  (interactive)
  ;; 1. Stop background simulation
  (when (fboundp 'taut-mock-stop)
    (taut-mock-stop))
  
  ;; 2. Disconnect WebSocket
  (when (fboundp 'taut-socket-disconnect)
    (taut-socket-disconnect))
  
  ;; 3. Identify and collect all Taut buffers
  (let ((taut-modes '(taut-sidebar-mode taut-inbox-mode taut-message-mode taut-thread-mode taut-compose-mode taut-socket-status-mode))
        (buffers-to-kill nil))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (memq major-mode taut-modes)
          (push buf buffers-to-kill))))
    
    ;; 4. Tab and Frame Cleanup
    (unless noninteractive
      ;; Delete all "Taut" frames
      (let ((taut-frames (cl-remove-if-not (lambda (f) (equal (frame-parameter f 'name) taut-workspace-name)) (frame-list))))
        (dolist (f taut-frames)
          (if (> (length (frame-list)) 1)
              (ignore-errors (delete-frame f))
            ;; If it is the last frame, rename it back to default or "Emacs" and display scratch
            (set-frame-name "Emacs")
            (dolist (win (window-list f))
              (set-window-buffer win (get-buffer-create "*scratch*"))))))
      
      ;; Delete all "Taut" tabs in all remaining frames
      (dolist (frame (frame-list))
        (with-selected-frame frame
          (while (taut--tab-exists-p taut-workspace-name)
            (ignore-errors
              (taut--close-tab-by-name taut-workspace-name))))))
    
    ;; 5. Delete remaining Taut windows on other frames/tabs
    (dolist (frame (frame-list))
      (dolist (win (window-list frame))
        (let ((buf (window-buffer win)))
          (when (memq (buffer-local-value 'major-mode buf) taut-modes)
            (if (one-window-p nil frame)
                (progn
                  (set-window-dedicated-p win nil)
                  (set-window-buffer win (get-buffer-create "*scratch*")))
              (ignore-errors (delete-window win)))))))
    
    ;; 6. Kill all identified Taut buffers
    (dolist (buf buffers-to-kill)
      (kill-buffer buf))
    
    (message "Taut: Hard quit complete.")))

;;;###autoload
(defun taut-setup-strict-windows ()
  "Set up the strict window layout.
If the frame is in landscape orientation, arranges windows in three columns:
Sidebar on the left, Activity in the middle, and Chat/Thread on the right.
If the frame is in portrait orientation, stacks them vertically in three rows."
  (interactive)
  (taut-ensure-consolidated-workspace)
  (let* ((sidebar-buf (get-buffer-create "*Taut Sidebar*"))
         (activity-buf (get-buffer-create "*Slack Activity*")))
    (with-current-buffer sidebar-buf
      (unless (eq major-mode 'taut-sidebar-mode)
        (taut-sidebar-mode)))
    (with-current-buffer activity-buf
      (unless (eq major-mode 'taut-inbox-mode)
        (taut-inbox-mode)))
    
    (dolist (win (window-list))
      (set-window-dedicated-p win nil))
    (delete-other-windows)
    (let ((window-min-width 1)
          (window-min-height 1))
      (let* ((chat-buf (cl-find-if (lambda (b)
                                    (and (not (equal (buffer-name b) "*Slack Activity*"))
                                         (not (equal (buffer-name b) "*Taut Sidebar*"))
                                         (or (eq (buffer-local-value 'major-mode b) 'taut-message-mode)
                                             (eq (buffer-local-value 'major-mode b) 'taut-thread-mode))))
                                  (buffer-list)))
             (frame-w (window-total-width (frame-root-window)))
             (frame-h (window-total-height (frame-root-window)))
             (is-landscape (> frame-w frame-h)))
        
        (if is-landscape
            (let* ((sidebar-w (or (and (boundp 'taut-sidebar-width) taut-sidebar-width) 30))
                   (activity-w (or (and (boundp 'taut-activity-width) taut-activity-width) 50))
                   (chat-w (or (and (boundp 'taut-chat-width) taut-chat-width) 120))
                   (total-needed (+ sidebar-w activity-w chat-w)))
              (if (>= frame-w total-needed)
                  ;; 1. Horizontal layout (precise widths)
                  (let* ((left-win (selected-window))
                         (middle-win (split-window left-win sidebar-w 'right))
                         (right-win (split-window middle-win (- (window-size middle-win t) chat-w) 'right)))
                    (set-window-buffer left-win sidebar-buf)
                    (set-window-buffer middle-win activity-buf)
                    (set-window-buffer right-win (or chat-buf (get-buffer-create "*scratch*")))
                    
                    (set-window-dedicated-p left-win t)
                    (set-window-dedicated-p middle-win t)
                    (set-window-dedicated-p right-win nil)
                    
                    ;; Set and preserve sidebar window size
                    (let ((delta (- sidebar-w (window-size left-win t))))
                      (when (/= delta 0)
                        (ignore-errors (window-resize left-win delta t))))
                    (window-preserve-size left-win t t)
                    
                    ;; Set and preserve chat window size
                    (let ((delta (- chat-w (window-size right-win t))))
                      (when (/= delta 0)
                        (ignore-errors (window-resize right-win delta t))))
                    (window-preserve-size right-win t t)
                    
                    (select-window middle-win))
                
                ;; 2. Horizontal layout (proportional fallback for narrow screens)
                (let* ((scale (/ (float frame-w) (float total-needed)))
                       (s-w (max 10 (round (* sidebar-w scale))))
                       (a-w (max 15 (round (* activity-w scale))))
                       (left-win (selected-window))
                       (middle-win (split-window left-win s-w 'right))
                       (right-win (split-window middle-win a-w 'right)))
                  (set-window-buffer left-win sidebar-buf)
                  (set-window-buffer middle-win activity-buf)
                  (set-window-buffer right-win (or chat-buf (get-buffer-create "*scratch*")))
                  
                  (set-window-dedicated-p left-win t)
                  (set-window-dedicated-p middle-win t)
                  (set-window-dedicated-p right-win nil)
                  
                  (let ((delta (- s-w (window-size left-win t))))
                    (when (/= delta 0) (ignore-errors (window-resize left-win delta t))))
                  (window-preserve-size left-win t t)
                  
                  (let ((delta (- a-w (window-size middle-win t))))
                    (when (/= delta 0) (ignore-errors (window-resize middle-win delta t))))
                  (window-preserve-size middle-win t t)
                  
                  (select-window middle-win))))
          
          ;; 3. Vertical/Portrait layout (stacked rows)
          (let* ((real-h (frame-height))
                 (sidebar-h (max 6 (round (* frame-h 0.15))))
                 (activity-h (max 10 (round (* frame-h 0.25)))))
            ;; Scale down heights if the physical frame height is too small to fit the simulated heights
            (when (< real-h (+ sidebar-h activity-h 4))
              (let ((scale (/ (float real-h) (float frame-h))))
                (setq sidebar-h (max 3 (round (* sidebar-h scale))))
                (setq activity-h (max 4 (round (* activity-h scale))))))
            (let* ((top-win (selected-window))
                   (middle-win (split-window top-win sidebar-h 'below))
                   (bottom-win (split-window middle-win activity-h 'below)))
              (set-window-buffer top-win sidebar-buf)
              (set-window-buffer middle-win activity-buf)
              (set-window-buffer bottom-win (or chat-buf (get-buffer-create "*scratch*")))
              
              (set-window-dedicated-p top-win t)
              (set-window-dedicated-p middle-win t)
              (set-window-dedicated-p bottom-win nil)
              
              ;; Set and preserve sidebar (top) window height
              (let ((delta (- sidebar-h (window-size top-win))))
                (when (/= delta 0)
                  (ignore-errors (window-resize top-win delta nil))))
              (window-preserve-size top-win nil t)
              
              ;; Set and preserve activity (middle) window height
              (let ((delta (- activity-h (window-size middle-win))))
                (when (/= delta 0)
                  (ignore-errors (window-resize middle-win delta nil))))
              (window-preserve-size middle-win nil t)
              
              (select-window middle-win))))))))

;;;###autoload
(defun taut-get-chat-window ()
  "Return the non-dedicated window in the current frame, creating one if necessary."
  (let ((sidebar-win (get-buffer-window "*Taut Sidebar*"))
        (activity-win (get-buffer-window "*Slack Activity*"))
        (chat-win nil))
    (dolist (win (window-list))
      (unless (window-dedicated-p win)
        (setq chat-win win)))
    (if (or (null chat-win) (null sidebar-win) (null activity-win))
        (progn
          (taut-setup-strict-windows)
          (setq chat-win nil)
        (dolist (win (window-list))
            (unless (window-dedicated-p win)
              (setq chat-win win)))
          chat-win)
      chat-win)))

;;;###autoload
(defun taut-focus-chat ()
  "Select and focus the Taut chat/message window."
  (interactive)
  (let ((win (taut-get-chat-window)))
    (when win
      (select-window win))))

;;;###autoload
(defun taut-reset-layout ()
  "Reset the Taut window layout.
Resets the sidebar window to `taut-sidebar-width`. If consolidation
is enabled, also rebalances the remaining windows in the active tab/frame."
  (interactive)
  (if (and (boundp 'taut-strict-windows) taut-strict-windows)
      (taut-setup-strict-windows)
    (let* ((sidebar-buf (get-buffer "*Taut Sidebar*"))
           (sidebar-win (and sidebar-buf (get-buffer-window sidebar-buf))))
      (if (taut-consolidate-method)
          ;; If consolidation is active, balance all windows first,
          ;; then restore the sidebar to its configured width.
          (progn
            (balance-windows)
            (when sidebar-win
              (let ((delta (- taut-sidebar-width (window-total-width sidebar-win))))
                (when (/= delta 0)
                  (ignore-errors
                    (window-resize sidebar-win delta t))))))
        ;; If not consolidating, just restore the sidebar width
        (when sidebar-win
          (let ((delta (- taut-sidebar-width (window-total-width sidebar-win))))
            (when (/= delta 0)
              (ignore-errors
                (window-resize sidebar-win delta t)))))))))

(defvar taut-mock-timer nil
  "Timer object running the background simulator (obsolete stub).")

;;;###autoload
(defun taut-mock-stop ()
  "Stop the background simulator (obsolete stub to prevent autoload failures)."
  (interactive)
  (message "Taut: Mock simulator is disabled in this version."))

;;;###autoload
(defun taut-mock-start ()
  "Start the background simulator (obsolete stub to prevent autoload failures)."
  (interactive)
  (message "Taut: Mock simulator is disabled in this version."))

;;;###autoload
(defun taut-reload ()
  "Reload all Taut Emacs Lisp source files dynamically.
This loads the latest source (.el) files to ensure that any edits
are immediately applied, even if older byte-compiled (.elc) files exist."
  (interactive)
  (let ((modules '("taut-model"
                   "taut-cache"
                   "taut-api"
                   "taut-sidebar"
                   "taut-inbox"
                   "taut-message"
                   "taut-thread"
                   "taut-socket"
                   "taut-transient"
                   "taut-compose"
                   "taut"))
        (dir (file-name-directory (or (locate-library "taut") ""))))
    (dolist (module modules)
      (let ((file (expand-file-name (concat module ".el") dir)))
        (if (file-exists-p file)
            (load file nil t)
          (load module nil t))))
    (message "Taut: Successfully reloaded all source modules.")))

;;;###autoload
(defun taut-dm-open ()
  "Start or open a direct message conversation with a workspace user."
  (interactive)
  (let (users-list)
    (maphash (lambda (_id user)
               (push (cons (format "%s (%s)" (taut-user-username user) (taut-user-real-name user))
                           user)
                     users-list))
             taut-users)
    (if (null users-list)
        (message "Taut: No workspace users found.")
      (let* ((sorted-choices (sort (mapcar #'car users-list) #'string<))
             (choice (completing-read "Direct Message with User: " sorted-choices nil t))
             (user (cdr (assoc choice users-list))))
        (when user
          (let ((user-id (taut-user-id user))
                (username (taut-user-username user)))
            (message "Opening direct message with @%s..." username)
            (condition-case err
                (let ((chan-id (taut-api-open-dm user-id)))
                  ;; Open the message conversation buffer for the DM channel
                  (taut-message-open chan-id)
                  (message "Opened DM with @%s!" username))
              (error
               (message "Error opening DM: %s" (error-message-string err))))))))))

;;;###autoload
(defun taut-huddle-join ()
  "Join the Slack huddle for the channel under point or current buffer."
  (interactive)
  (let ((chan-id (or (get-text-property (point) 'taut-channel-id)
                     (and (boundp 'taut-current-channel-id) taut-current-channel-id))))
    (if (null chan-id)
        (message "Taut: No channel found at point or in this buffer.")
      (let* ((team-id (and (boundp 'taut-team-id) taut-team-id))
             (url (if team-id
                      (format "slack://channel?team=%s&id=%s" team-id chan-id)
                    (format "slack://channel?id=%s" chan-id))))
        (message "Taut: Launching Slack deep link for channel %s..." chan-id)
        (browse-url url)))))

(provide 'taut)
;;; taut.el ends here

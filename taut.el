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
(require 'taut-cache-browser)


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

;;;; Global Keybindings for Activity Feed Quick Navigation
(define-key taut-inbox-mode-map (kbd "C-n") #'taut-inbox-next)
(define-key taut-inbox-mode-map (kbd "C-p") #'taut-inbox-prev)

(define-key taut-message-mode-map (kbd "C-n") #'taut-inbox-next)
(define-key taut-message-mode-map (kbd "C-p") #'taut-inbox-prev)

(define-key taut-thread-mode-map (kbd "C-n") #'taut-inbox-next)
(define-key taut-thread-mode-map (kbd "C-p") #'taut-inbox-prev)

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
          (taut-api-fetch-custom-emojis)
          
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
         (activity-buf (get-buffer-create "*Slack Inbox*")))
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
                                    (and (not (equal (buffer-name b) "*Slack Inbox*"))
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
        (activity-win (get-buffer-window "*Slack Inbox*"))
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
(defun taut-group-dm-open ()
  "Start or open a direct message conversation with multiple workspace users."
  (interactive)
  (let (users-list)
    (maphash (lambda (_id user)
               ;; Exclude current user from selection list as is standard Slack UX
               (unless (equal (taut-user-id user) taut-current-user-id)
                 (push (cons (format "%s (%s)" (taut-user-username user) (taut-user-real-name user))
                             user)
                       users-list)))
             taut-users)
    (if (null users-list)
        (message "Taut: No other workspace users found.")
      (let* ((sorted-choices (sort (mapcar #'car users-list) #'string<))
             (choices (completing-read-multiple "Group DM with Users (comma-separated): " sorted-choices nil t))
             (selected-users (mapcar (lambda (c) (cdr (assoc c users-list))) choices))
             (user-ids (mapcar #'taut-user-id selected-users))
             (usernames (mapcar #'taut-user-username selected-users)))
        (if (null user-ids)
            (message "Taut: No users selected.")
          (message "Opening group direct message with %s..." (mapconcat #'identity usernames ", "))
          (condition-case err
              (let ((chan-id (taut-api-open-dm user-ids)))
                ;; Open the message conversation buffer for the DM channel
                (taut-message-open chan-id)
                (message "Opened Group DM with %s!" (mapconcat #'identity usernames ", ")))
            (error
             (message "Error opening Group DM: %s" (error-message-string err)))))))))


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

;;;###autoload
(defun taut-channel-create (name is-private)
  "Create a new channel with NAME.
If IS-PRIVATE is non-nil, the channel will be private."
  (interactive (list (read-string "Channel Name: ")
                     (y-or-n-p "Make Channel Private? ")))
  (let ((clean-name (replace-regexp-in-string " " "-" (downcase name))))
    (message "Creating channel #%s..." clean-name)
    (condition-case err
        (let* ((res (taut-api-create-channel clean-name is-private))
               (chan-id (cdr (assoc 'id (cdr (assoc 'channel res))))))
          (message "Channel #%s created successfully!" clean-name)
          (when chan-id
            (taut-message-open chan-id)))
      (error
       (message "Error creating channel: %s" (error-message-string err))))))

;;;###autoload
(defun taut-channel-invite (channel-id user-id)
  "Invite a user to a channel."
  (interactive
   (let* ((default-chan-id (or (get-text-property (point) 'taut-channel-id)
                               (and (boundp 'taut-current-channel-id) taut-current-channel-id)))
          (chan-choices (mapcar (lambda (c) (cons (taut-channel-name c) (taut-channel-id c)))
                                (taut-model-get-channels-list)))
          (channel-id (if default-chan-id
                          default-chan-id
                        (let ((choice (completing-read "Invite to Channel: " (mapcar #'car chan-choices) nil t)))
                          (cdr (assoc choice chan-choices)))))
          (user-choices (mapcar (lambda (u) (cons (format "%s (%s)" (taut-user-username u) (taut-user-real-name u)) (taut-user-id u)))
                                (let (users) (maphash (lambda (_k v) (push v users)) taut-users) users)))
          (user-choice (completing-read "Invite User: " (mapcar #'car user-choices) nil t))
          (user-id (cdr (assoc user-choice user-choices))))
     (list channel-id user-id)))
  (when (and channel-id user-id)
    (let ((chan-name (or (and (taut-model-get-channel channel-id) (taut-channel-name (taut-model-get-channel channel-id))) channel-id))
          (username (or (and (taut-model-get-user user-id) (taut-user-username (taut-model-get-user user-id))) user-id)))
      (message "Inviting @%s to #%s..." username chan-name)
      (condition-case err
          (progn
            (taut-api-invite-to-channel channel-id (list user-id))
            (message "Successfully invited @%s to #%s!" username chan-name))
        (error
         (message "Error inviting user: %s" (error-message-string err)))))))

;;;###autoload
(defun taut-channel-kick (channel-id user-id)
  "Remove a user from a channel."
  (interactive
   (let* ((default-chan-id (or (get-text-property (point) 'taut-channel-id)
                               (and (boundp 'taut-current-channel-id) taut-current-channel-id)))
          (chan-choices (mapcar (lambda (c) (cons (taut-channel-name c) (taut-channel-id c)))
                                (taut-model-get-channels-list)))
          (channel-id (if default-chan-id
                          default-chan-id
                        (let ((choice (completing-read "Kick from Channel: " (mapcar #'car chan-choices) nil t)))
                          (cdr (assoc choice chan-choices)))))
          (user-choices (mapcar (lambda (u) (cons (format "%s (%s)" (taut-user-username u) (taut-user-real-name u)) (taut-user-id u)))
                                (let (users) (maphash (lambda (_k v) (push v users)) taut-users) users)))
          (user-choice (completing-read "Kick User: " (mapcar #'car user-choices) nil t))
          (user-id (cdr (assoc user-choice user-choices))))
     (list channel-id user-id)))
  (when (and channel-id user-id)
    (let ((chan-name (or (and (taut-model-get-channel channel-id) (taut-channel-name (taut-model-get-channel channel-id))) channel-id))
          (username (or (and (taut-model-get-user user-id) (taut-user-username (taut-model-get-user user-id))) user-id)))
      (message "Removing @%s from #%s..." username chan-name)
      (condition-case err
          (progn
            (taut-api-kick-from-channel channel-id user-id)
            (message "Successfully removed @%s from #%s!" username chan-name))
        (error
         (message "Error removing user: %s" (error-message-string err)))))))

;;;###autoload
(defun taut-channel-set-topic (channel-id topic)
  "Set or edit the topic of a channel."
  (interactive
   (let* ((default-chan-id (or (get-text-property (point) 'taut-channel-id)
                               (and (boundp 'taut-current-channel-id) taut-current-channel-id)))
          (chan-choices (mapcar (lambda (c) (cons (taut-channel-name c) (taut-channel-id c)))
                                (taut-model-get-channels-list)))
          (channel-id (if default-chan-id
                          default-chan-id
                        (let ((choice (completing-read "Channel: " (mapcar #'car chan-choices) nil t)))
                          (cdr (assoc choice chan-choices)))))
          (chan (and channel-id (taut-model-get-channel channel-id)))
          (current-topic (and chan (taut-channel-topic chan)))
          (topic (read-string "Set Topic: " current-topic)))
     (list channel-id topic)))
  (when channel-id
    (let ((chan-name (or (and (taut-model-get-channel channel-id) (taut-channel-name (taut-model-get-channel channel-id))) channel-id)))
      (message "Setting topic for #%s..." chan-name)
      (condition-case err
          (progn
            (taut-api-set-channel-topic channel-id topic)
            (message "Topic for #%s updated successfully!" chan-name))
        (error
         (message "Error setting topic: %s" (error-message-string err)))))))

;;;###autoload
(defun taut-channel-archive (channel-id)
  "Archive (delete) a channel after confirmation."
  (interactive
   (let* ((default-chan-id (or (get-text-property (point) 'taut-channel-id)
                               (and (boundp 'taut-current-channel-id) taut-current-channel-id)))
          (chan-choices (mapcar (lambda (c) (cons (taut-channel-name c) (taut-channel-id c)))
                                (taut-model-get-channels-list)))
          (channel-id (if default-chan-id
                          default-chan-id
                        (let ((choice (completing-read "Archive Channel: " (mapcar #'car chan-choices) nil t)))
                          (cdr (assoc choice chan-choices))))))
     (list channel-id)))
  (when channel-id
    (let ((chan-name (or (and (taut-model-get-channel channel-id) (taut-channel-name (taut-model-get-channel channel-id))) channel-id)))
      (when (y-or-n-p (format "Are you absolutely sure you want to archive #%s? " chan-name))
        (message "Archiving #%s..." chan-name)
        (condition-case err
            (progn
              (taut-api-archive-channel channel-id)
              (message "Successfully archived channel #%s!" chan-name)
              ;; If we archived the current message buffer's channel, switch away
              (when (and (boundp 'taut-current-channel-id) (equal taut-current-channel-id channel-id))
                (kill-buffer (current-buffer))))
          (error
           (message "Error archiving channel: %s" (error-message-string err))))))))

;;;###autoload
(defun taut-channel-list-members (channel-id)
  "List all members of a channel."
  (interactive
   (let* ((default-chan-id (or (get-text-property (point) 'taut-channel-id)
                               (and (boundp 'taut-current-channel-id) taut-current-channel-id)))
          (chan-choices (mapcar (lambda (c) (cons (taut-channel-name c) (taut-channel-id c)))
                                (taut-model-get-channels-list)))
          (channel-id (if default-chan-id
                          default-chan-id
                        (let ((choice (completing-read "List Members of Channel: " (mapcar #'car chan-choices) nil t)))
                          (cdr (assoc choice chan-choices))))))
     (list channel-id)))
  (when channel-id
    (let ((chan-name (or (and (taut-model-get-channel channel-id) (taut-channel-name (taut-model-get-channel channel-id))) channel-id)))
      (message "Fetching members of #%s..." chan-name)
      (condition-case err
          (let* ((member-ids (taut-api-get-channel-members channel-id))
                 (buf (get-buffer-create (format "*Taut Members: #%s*" chan-name))))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (special-mode)
                (insert (propertize (format "👥 Members of #%s (%d total)\n" chan-name (length member-ids)) 'face 'bold))
                (insert (propertize "================================================================================\n" 'face 'font-lock-comment-face))
                (dolist (uid member-ids)
                  (let* ((user (taut-model-get-user uid))
                         (username (if user (taut-user-username user) uid))
                         (real-name (if user (taut-user-real-name user) "Unknown User"))
                         (presence (if user (taut-user-presence user) 'offline))
                         (indicator (cond
                                     ((and user (taut-user-is-huddling user))
                                      (propertize "🎧 " 'face 'font-lock-warning-face))
                                     ((eq presence 'online)
                                      (propertize "● " 'face 'font-lock-string-face))
                                     ((eq presence 'away)
                                      (propertize "○ " 'face 'font-lock-comment-face))
                                     (t
                                      (propertize "○ " 'face 'font-lock-comment-face)))))
                    (insert (format "  %s %s (%s) [ID: %s]\n"
                                    indicator
                                    (propertize (format "@%s" username) 'face 'bold)
                                    real-name
                                    uid))))
                (insert (propertize "================================================================================\n" 'face 'font-lock-comment-face))
                (insert "  [q] Close Panel\n")
                (setq-local truncate-lines t)
                (setq-local buffer-read-only t)))
            (message "Found %d members in #%s!" (length member-ids) chan-name)
            (pop-to-buffer buf))
        (error
         (message "Error fetching channel members: %s" (error-message-string err)))))))

(defun taut--detect-language-for-mode (mode)
  "Detect the language specifier string for major mode MODE."
  (let* ((mode-str (symbol-name mode))
         ;; Strip -mode suffix
         (base-str (if (string-suffix-p "-mode" mode-str)
                       (substring mode-str 0 -5)
                     mode-str))
         (base-sym (intern base-str))
         ;; Search in alist
         (match (cl-find-if (lambda (entry)
                              (or (eq (cdr entry) base-sym)
                                  (eq (cdr entry) mode)))
                            taut-code-block-language-alist)))
    (if match
        (car match)
      ;; Fallback to base-str if no match
      base-str)))

(defun taut-select-recipient (&optional prompt)
  "Select a recipient (channel, group, or DM) using completion.
Optional PROMPT specifies the completion prompt."
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
        (error "Taut: No channels or DMs available")
      (let* ((reversed-candidates (nreverse candidates))
             (choice (completing-read (or prompt "Send to Channel/DM: ") reversed-candidates nil t))
             (chan-id (cdr (assoc choice reversed-candidates))))
        (unless chan-id
          (error "Taut: Selection cancelled or invalid"))
        chan-id))))

;;;###autoload
(defun taut-send-region (start end)
  "Send the active region between START and END as a formatted code block.
Prompts for a Slack channel, group, or DM recipient."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         (lang (taut--detect-language-for-mode major-mode))
         (chan-id (taut-select-recipient "Send region to Channel/DM: "))
         (formatted-text (format "```%s\n%s\n```" lang code)))
    (if (and (boundp 'taut-bot-token) taut-bot-token)
        (taut-api-post-message chan-id formatted-text)
      ;; Fallback to offline/mock
      (let* ((ts (format "%d.0000" (time-convert nil 'integer)))
             (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) formatted-text)))
        (taut-model-add-message
         (make-taut-message
          :id (concat "msg_" ts)
          :channel-id chan-id
          :user-id taut-current-user-id
          :text formatted-text
          :ts ts
          :thread-ts nil
          :reply-count 0
          :is-unread nil
          :is-mention is-mention))))
    ;; Refresh active buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (or (eq major-mode 'taut-message-mode)
                  (eq major-mode 'taut-thread-mode))
          (if (eq major-mode 'taut-thread-mode)
              (taut-thread-refresh)
            (taut-message-refresh)))))
    (message "Sent region as %s code block!" lang)))

;;;###autoload
(defun taut-send-buffer ()
  "Send the current buffer as a file snippet to a chosen Slack recipient.
Uses the current buffer contents (even if unsaved/dirty)."
  (interactive)
  (let* ((buf-name (buffer-name))
         (sanitized-name (replace-regexp-in-string "[*?/\\]" "" buf-name))
         (sanitized-name (if (string-empty-p sanitized-name) "buffer" sanitized-name))
         (ext-raw (file-name-extension sanitized-name))
         (ext (and ext-raw (downcase ext-raw)))
         (prefix (if ext
                     (substring sanitized-name 0 (- (length sanitized-name) (length ext) 1))
                   sanitized-name))
         (suffix (if ext (concat "." ext) ""))
         (temp-file (make-temp-file (concat "taut-" prefix "-") nil suffix))
         (chan-id (taut-select-recipient "Send buffer as file to Channel/DM: ")))
    (unwind-protect
        (progn
          ;; Write the current state of the buffer to the temporary file
          (let ((coding-system-for-write 'utf-8))
            (write-region (point-min) (point-max) temp-file nil 'silent))
          
          (if (and (boundp 'taut-bot-token) taut-bot-token)
              (taut-api-upload-file chan-id temp-file)
            ;; Fallback to offline/mock
            (let* ((ts (format "%d.0000" (time-convert nil 'integer)))
                   (ts-id (format "%s-%04d" ts (random 10000)))
                   (mock-url (format "https://files.slack.com/files-pri/mock-%s/download/%s" ts-id sanitized-name))
                   (mimetype (cond
                              ((member ext '("png" "jpg" "jpeg" "gif")) (format "image/%s" ext))
                              ((member ext '("sh" "bash")) "text/x-sh")
                              ((member ext '("el" "py" "js" "ts" "html" "css" "txt")) "text/plain")
                              (t "text/plain")))
                   ;; Construct files alist
                   (mock-files `(((name . ,sanitized-name)
                                  (mimetype . ,mimetype)
                                  (url_private_download . ,mock-url))))
                   (local-path (taut-media-file-path mock-url)))
              ;; Copy temp file to local media cache so previews render instantly
              (copy-file temp-file local-path t)
              ;; Inject mock message
              (taut-model-add-message
               (make-taut-message
                :id (concat "msg_" ts)
                :channel-id chan-id
                :user-id taut-current-user-id
                :text (format "Shared a file: %s" sanitized-name)
                :ts ts
                :thread-ts nil
                :reply-count 0
                :is-unread nil
                :is-mention nil
                :files mock-files)))))
      ;; Cleanup temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))
    
    ;; Refresh active buffers
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (or (eq major-mode 'taut-message-mode)
                  (eq major-mode 'taut-thread-mode))
          (if (eq major-mode 'taut-thread-mode)
              (taut-thread-refresh)
            (taut-message-refresh)))))
    (message "Sent buffer as file %s!" sanitized-name)))

(provide 'taut)
;;; taut.el ends here

;;; taut.el --- Modern, elegant Slack client entry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Google DeepMind

;; Author: Antigravity
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

;;;; Global Minor Mode / Initialization Commands

;;;###autoload
(defun taut ()
  "Start Taut Slack client with a beautiful sidebar-and-inbox layout.
Initializes the system with sample conversations and opens the workspace."
  (interactive)
  (taut-initialize-mock-data)
  ;; Split and display layout
  (delete-other-windows)
  (taut-sidebar-show)
  (taut-inbox-show)
  (message "Welcome to Taut! Sidebar and Inbox loaded. Start simulation with M-x taut-mock-start."))

;;;###autoload
(defun taut-connect ()
  "Connect Taut to the real Slack API and start the live workspace."
  (interactive)
  (taut-api-load-tokens-from-authinfo)
  (unless taut-bot-token
    (setq taut-bot-token (read-string "Enter Slack Token (xoxp-... or xoxb-...): ")))
  
  (message "Taut: Connecting to Slack...")
  (condition-case err
      (progn
        ;; Test Auth and set our user ID
        (taut-api-test-auth)
        ;; Clear local data and load real workspace data
        (taut-model-clear-all)
        (taut-api-fetch-users)
        (taut-api-fetch-channels)
        (taut-api-fetch-starred)
        
        ;; If app token is configured, establish Socket Mode WebSocket connection
        (when taut-app-token
          (ignore-errors (taut-socket-connect)))
        
        ;; Split and display layout
        (delete-other-windows)
        (taut-sidebar-show)
        (taut-inbox-show)
        (message "Taut: Successfully connected! Click/RET on a channel to read it."))
    (error
     (error "Taut Connection Failed: %s" (error-message-string err)))))

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

;;;; Sample Data Initialization

(defun taut-initialize-mock-data ()
  "Pre-populate the local database with high-fidelity sample data."
  (taut-model-clear-all)

  ;; 1. Register Users
  (taut-model-add-user (make-taut-user :id "U_ME" :username "me" :real-name "Bunny Lushington" :presence 'online :is-me t))
  (taut-model-add-user (make-taut-user :id "U_ALICE" :username "alice" :real-name "Alice Smith" :presence 'online))
  (taut-model-add-user (make-taut-user :id "U_BOB" :username "bob" :real-name "Bob Jones" :presence 'away))
  (taut-model-add-user (make-taut-user :id "U_CAROL" :username "carol" :real-name "Carol Danvers" :presence 'offline))
  (taut-model-add-user (make-taut-user :id "U_DAVE" :username "dave" :real-name "Dave Bowman" :presence 'online))

  ;; 2. Register Channels
  (taut-model-add-channel (make-taut-channel :id "C_GENERAL" :name "general" :type 'public :unread-count 0 :mention-count 0 :is-starred t :topic "Company-wide announcements"))
  (taut-model-add-channel (make-taut-channel :id "C_DEV" :name "development" :type 'public :unread-count 0 :mention-count 0 :is-starred t :topic "Core technical discussion & reviews"))
  (taut-model-add-channel (make-taut-channel :id "C_IDEAS" :name "ideas" :type 'public :unread-count 0 :mention-count 0 :is-starred nil :topic "Brainstorming and blue-sky initiatives"))
  (taut-model-add-channel (make-taut-channel :id "C_ALICE_DM" :name "alice" :type 'dm :unread-count 0 :mention-count 0 :is-starred nil))
  (taut-model-add-channel (make-taut-channel :id "C_BOB_DM" :name "bob" :type 'dm :unread-count 0 :mention-count 0 :is-starred nil))

  ;; 3. Seed Conversations (Sorted by timestamp ascending)
  ;; #general
  (taut-model-add-message (make-taut-message :id "m1_1" :channel-id "C_GENERAL" :user-id "U_BOB" :text "Welcome to the new Slack workspace! Let's get things rolling." :ts "1688450000.0001" :is-unread nil))
  (taut-model-add-message (make-taut-message :id "m1_2" :channel-id "C_GENERAL" :user-id "U_ALICE" :text "Excited to be here! Check out the `development` channel." :ts "1688450100.0001" :is-unread nil))

  ;; #development
  (taut-model-add-message (make-taut-message :id "m2_1" :channel-id "C_DEV" :user-id "U_ALICE" :text "Hey team, we're building the new Emacs client *Taut*! Focus is on extreme UX and notifications." :ts "1688460000.0001" :is-unread nil))
  (taut-model-add-message (make-taut-message :id "m2_2" :channel-id "C_DEV" :user-id "U_DAVE" :text "Amazing! Will it support thread indicators?" :ts "1688460500.0001" :is-unread nil :thread-ts "1688460000.0001"))
  (taut-model-add-message (make-taut-message :id "m2_3" :channel-id "C_DEV" :user-id "U_ALICE" :text "Yes, clicking the thread replies will open a dedicated side panel." :ts "1688460600.0001" :is-unread nil :thread-ts "1688460000.0001"))

  ;; #ideas (With active thread)
  (taut-model-add-message (make-taut-message :id "m3_1" :channel-id "C_IDEAS" :user-id "U_BOB" :text "Should we run our client tests with `mix testall`?" :ts "1688470000.0001" :is-unread nil))

  ;; Direct Messages (Alice - Read, Bob - Unread DM)
  (taut-model-add-message (make-taut-message :id "mdm1_1" :channel-id "C_ALICE_DM" :user-id "U_ALICE" :text "Hey, are you free for a review today?" :ts "1688480000.0001" :is-unread nil))
  (taut-model-add-message (make-taut-message :id "mdm2_1" :channel-id "C_BOB_DM" :user-id "U_BOB" :text "Did you get that script command for standard storage migration tests?" :ts "1688490000.0001" :is-unread t))

  ;; Set up watch thread list with #development thread
  (push "1688460000.0001" taut-watched-threads))

;;;; Periodic Simulation Driver (Mock Engine)

(defvar taut-mock-timer nil
  "Timer object running the background simulator.")

(defvar taut-mock-messages-pool
  '(("@alice" "C_GENERAL" "Just finished testing the migration scripts. Looks completed!")
    ("@bob" "C_GENERAL" "Can anyone review my PR for *taut-sidebar*?")
    ("@carol" "C_DEV" "Hey <@U_ME>, can you verify if standard global mirror relationships delete cleanly?")
    ("@dave" "C_DEV" "I'm experiencing some drift on the local caches. Anyone else?")
    ("@alice" "C_ALICE_DM" "Hey! Don't miss this thread update. I just left some answers.")
    ("@bob" "C_BOB_DM" "Sent over the specs. Ping me when you get a chance.")
    ("@carol" "C_IDEAS" "What about adding support for custom theme palettes? Let's discuss.")
    ;; Thread replies to Bob's thread in C_IDEAS (which is watched)
    ("@dave" "C_IDEAS" "I tried running it, but I'm getting a timeout on socket connections." "1688470000.0001")
    ("@alice" "C_IDEAS" "Let's debug it together. Are you free?" "1688470000.0001"))
  "Pool of random mock events (sender, channel-id, text, [thread-ts]).")

;;;###autoload
(defun taut-mock-start ()
  "Start the background simulator.
Every 10 seconds, it pushes random activity (mentions, DMs, thread replies)
into Taut to show the unread counters and Inbox updating in real time."
  (interactive)
  (if taut-mock-timer
      (message "Taut simulation is already running.")
    (setq taut-mock-timer
          (run-with-timer 5 10 #'taut-mock-tick))
    (message "Taut simulation started! New events arriving every 10 seconds.")))

;;;###autoload
(defun taut-mock-stop ()
  "Stop the background simulator."
  (interactive)
  (when taut-mock-timer
    (cancel-timer taut-mock-timer)
    (setq taut-mock-timer nil)
    (message "Taut simulation stopped.")))

(defun taut-mock-tick ()
  "Execute a single step of the mock simulation."
  (let* ((event (seq-random-elt taut-mock-messages-pool))
         (sender-username (substring (nth 0 event) 1)) ; strip '@'
         (chan-id (nth 1 event))
         (text (nth 2 event))
         (thread-ts (nth 3 event))
         ;; Find user by username
         (user nil))
    (maphash
     (lambda (_id u)
       (when (equal (taut-user-username u) sender-username)
         (setq user u)))
     taut-users)

    (when user
      (let* ((now-ts (format "%d.%04d" (time-convert nil 'integer) (random 10000)))
             (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) text))
             (msg (make-taut-message
                   :id (concat "sim_msg_" now-ts)
                   :channel-id chan-id
                   :user-id (taut-user-id user)
                   :text text
                   :ts now-ts
                   :thread-ts thread-ts
                   :reply-count 0
                   :is-unread t
                   :is-mention is-mention)))
        
        ;; If it's a thread reply, make sure root is watched so it shows in inbox
        (when (and thread-ts (not (member thread-ts taut-watched-threads)))
          (push thread-ts taut-watched-threads))

        (taut-model-add-message msg)
        (message "🔔 Taut Sim: Received new message from @%s in %s"
                 sender-username
                 (let ((chan (taut-model-get-channel chan-id)))
                   (if chan (taut-channel-name chan) "unknown")))))))

;;;###autoload
(defun taut-dm-open ()
  "Start or open a direct message conversation with a workspace user."
  (interactive)
  (let (users-list)
    (maphash (lambda (_id user)
               (unless (taut-user-is-me user)
                 (push (cons (format "%s (%s)" (taut-user-username user) (taut-user-real-name user))
                             user)
                       users-list)))
             taut-users)
    (if (null users-list)
        (message "Taut: No other workspace users found.")
      (let* ((sorted-choices (sort (mapcar #'car users-list) #'string<))
             (choice (completing-read "Direct Message with User: " sorted-choices nil t))
             (user (cdr (assoc choice users-list))))
        (when user
          (let ((user-id (taut-user-id user))
                (username (taut-user-username user)))
            (message "Opening direct message with @%s..." username)
            (condition-case err
                (let ((chan-id
                       (if (and (boundp 'taut-bot-token) taut-bot-token)
                           ;; Online mode: Open DM via Slack API
                           (taut-api-open-dm user-id)
                         ;; Offline/Mock mode: Find or create mock DM channel
                         (let* ((mock-id (concat "C_" (upcase username) "_DM"))
                                (existing (taut-model-get-channel mock-id)))
                           (unless existing
                             (taut-model-add-channel
                              (make-taut-channel
                               :id mock-id
                               :name username
                               :type 'dm
                               :unread-count 0
                               :mention-count 0)))
                           mock-id))))
                  ;; Open the message conversation buffer for the DM channel
                  (taut-message-open chan-id)
                  (message "Opened DM with @%s!" username))
              (error
               (message "Error opening DM: %s" (error-message-string err))))))))))

(provide 'taut)
;;; taut.el ends here

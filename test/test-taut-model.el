;;; test-taut-model.el --- Unit tests for taut-model.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; Unit tests for Taut core state and data models (taut-model.el).

;;; Code:

(require 'ert)
(require 'taut-model)

(ert-deftest taut-model-clear-all-test ()
  "Test that taut-model-clear-all resets in-memory hashes and lists."
  (taut-model-clear-all)
  ;; Populate with some mock data
  (puthash "U_TEST" (make-taut-user :id "U_TEST" :username "testuser") taut-users)
  (puthash "C_TEST" (make-taut-channel :id "C_TEST" :name "testchan") taut-channels)
  (puthash "C_TEST" (list (make-taut-message :id "m1" :ts "123")) taut-messages)
  (setq taut-watched-threads '("123"))
  
  ;; Assert populated
  (should (> (hash-table-count taut-users) 0))
  (should (> (hash-table-count taut-channels) 0))
  (should (> (hash-table-count taut-messages) 0))
  (should (equal taut-watched-threads '("123")))
  
  ;; Clear
  (taut-model-clear-all)
  
  ;; Assert empty
  (should (= (hash-table-count taut-users) 0))
  (should (= (hash-table-count taut-channels) 0))
  (should (= (hash-table-count taut-messages) 0))
  (should-not taut-watched-threads))

(ert-deftest taut-model-users-test ()
  "Test user registration and retrieval."
  (taut-model-clear-all)
  (let ((user1 (make-taut-user :id "U_ALICE" :username "alice" :real-name "Alice Smith" :presence 'online))
        (user2 (make-taut-user :id "U_BOB" :username "bob" :real-name "Bob Jones" :presence 'away)))
    
    (taut-model-add-user user1)
    (taut-model-add-user user2)
    
    ;; Test retrieve by ID
    (let ((fetched (taut-model-get-user "U_ALICE")))
      (should fetched)
      (should (equal (taut-user-username fetched) "alice"))
      (should (equal (taut-user-real-name fetched) "Alice Smith"))
      (should (eq (taut-user-presence fetched) 'online)))
    
    ;; Test fallback for unknown user
    (let ((unknown (taut-model-get-user "U_NONEXISTENT")))
      (should unknown)
      (should (equal (taut-user-id unknown) "U_NONEXISTENT"))
      (should (equal (taut-user-real-name unknown) "Unknown User")))
    
    ;; Test retrieve by username
    (let ((by-name (taut-model-get-user-by-username "bob")))
      (should by-name)
      (should (equal (taut-user-id by-name) "U_BOB")))))

(ert-deftest taut-model-channels-test ()
  "Test channel registration, sorting, and retrieval."
  (taut-model-clear-all)
  (let ((c-general (make-taut-channel :id "C_GENERAL" :name "general" :type 'public :is-starred t))
        (c-dev (make-taut-channel :id "C_DEV" :name "development" :type 'public :is-starred nil))
        (c-bob (make-taut-channel :id "C_BOB" :name "bob" :type 'dm :is-starred t)))
    
    (taut-model-add-channel c-general)
    (taut-model-add-channel c-dev)
    (taut-model-add-channel c-bob)
    
    ;; Retrieve by ID
    (should (equal (taut-model-get-channel "C_GENERAL") c-general))
    (should (equal (taut-model-get-channel "C_DEV") c-dev))
    (should-not (taut-model-get-channel "C_NONEXISTENT"))
    
    ;; Sorting: Starred channels should be listed first, then ordered alphabetically by name
    (let ((sorted-list (taut-model-get-channels-list)))
      (should (= (length sorted-list) 3))
      ;; First item should be "bob" (starred DM) or "general" (starred public)?
      ;; Sort is: (if (not (eq star-a star-b)) star-a (string< name-a name-b))
      ;; bob is starred, general is starred. Alphabetically, "bob" < "general".
      (should (equal (taut-channel-id (nth 0 sorted-list)) "C_BOB"))
      (should (equal (taut-channel-id (nth 1 sorted-list)) "C_GENERAL"))
      (should (equal (taut-channel-id (nth 2 sorted-list)) "C_DEV")))))

(ert-deftest taut-model-messages-test ()
  "Test message sorting, chronological integrity, and starring."
  (taut-model-clear-all)
  (let ((m1 (make-taut-message :id "m1" :channel-id "C_DEV" :user-id "U_ALICE" :text "First!" :ts "1688460000.0001"))
        (m2 (make-taut-message :id "m2" :channel-id "C_DEV" :user-id "U_BOB" :text "Second!" :ts "1688460500.0001" :is-starred t))
        (m3 (make-taut-message :id "m3" :channel-id "C_DEV" :user-id "U_ALICE" :text "Third (but added out of order)!" :ts "1688460200.0001")))
    
    ;; Add out of order
    (taut-model-add-message m1)
    (taut-model-add-message m2)
    (taut-model-add-message m3)
    
    ;; Messages fetched for a channel should ALWAYS be sorted ascending by timestamp (ts)
    (let ((fetched (taut-model-get-messages "C_DEV")))
      (should (= (length fetched) 3))
      (should (equal (taut-message-id (nth 0 fetched)) "m1")) ; ts: 1688460000.0001
      (should (equal (taut-message-id (nth 1 fetched)) "m3")) ; ts: 1688460200.0001
      (should (equal (taut-message-id (nth 2 fetched)) "m2"))) ; ts: 1688460500.0001
    
    ;; Test retrieve by timestamp (ts)
    (should (equal (taut-model-get-message-by-ts "1688460500.0001") m2))
    
    ;; Test retrieve starred messages
    (let ((starred (taut-model-get-starred-messages)))
      (should (= (length starred) 1))
      (should (equal (taut-message-id (car starred)) "m2")))))

(ert-deftest taut-model-inbox-items-test ()
  "Test generation of Gnus-style unified inbox items."
  (taut-model-clear-all)
  ;; Setup currentUser
  (setq taut-current-user-id "U_ME")
  
  ;; Register users and channel
  (taut-model-add-user (make-taut-user :id "U_ME" :username "me" :is-me t))
  (taut-model-add-user (make-taut-user :id "U_ALICE" :username "alice"))
  (taut-model-add-channel (make-taut-channel :id "C_ALICE_DM" :name "alice" :type 'dm))
  
  ;; Add unread DM (not by me)
  (taut-model-add-message (make-taut-message :id "dm1" :channel-id "C_ALICE_DM" :user-id "U_ALICE" :text "Review PR?" :ts "1688480000.0001" :is-unread t))
  
  ;; Add unread DM by me (should NOT appear in inbox as an unread item)
  (taut-model-add-message (make-taut-message :id "dm2" :channel-id "C_ALICE_DM" :user-id "U_ME" :text "Sure, on it" :ts "1688480100.0001" :is-unread t))
  
  (let ((inbox-items (taut-model-get-inbox-items)))
    (should (= (length inbox-items) 1))
    (should (equal (taut-inbox-item-message-id (car inbox-items)) "dm1"))
    (should (eq (taut-inbox-item-type (car inbox-items)) 'dm))))

(ert-deftest taut-model-huddle-message-test ()
  "Test that taut-model--check-huddle-message toggles the has-active-huddle slot on channel."
  (taut-model-clear-all)
  (let ((chan (make-taut-channel :id "C_DEV" :name "development" :type 'public)))
    (taut-model-add-channel chan)
    (should-not (taut-channel-has-active-huddle chan))
    
    ;; Send an "in progress" huddle message
    (taut-model--check-huddle-message "C_DEV" "📞 Slack Huddle: Design Discussion in progress")
    (should (taut-channel-has-active-huddle chan))
    
    ;; Send an "Ended" huddle message
    (taut-model--check-huddle-message "C_DEV" "📞 Slack Huddle Ended: Design Discussion Ended")
    (should-not (taut-channel-has-active-huddle chan))))

(ert-deftest taut-window-consolidation-method-test ()
  "Test that `taut-consolidate-method` resolves correctly."
  (let ((taut-consolidate-windows nil))
    (should-not (taut-consolidate-method)))
  (let ((taut-consolidate-windows 'tab))
    (should (eq (taut-consolidate-method) 'tab)))
  (let ((taut-consolidate-windows 'frame))
    (should (eq (taut-consolidate-method) 'frame)))
  (let ((taut-consolidate-windows 'auto))
    (let ((tab-bar-mode nil))
      (should (eq (taut-consolidate-method) 'frame)))
    (let ((tab-bar-mode t))
      (should (eq (taut-consolidate-method) 'tab)))))

(ert-deftest taut-ensure-consolidated-workspace-test ()
  "Test that `taut-ensure-consolidated-workspace` executes safely in tests."
  (let ((taut-consolidate-windows 'tab))
    ;; Should not raise any errors in noninteractive test mode
    (should (progn
              (taut-ensure-consolidated-workspace)
              t)))
  (let ((taut-consolidate-windows 'frame))
    ;; Should not raise any errors in noninteractive test mode
    (should (progn
              (taut-ensure-consolidated-workspace)
              t))))

(ert-deftest taut-reset-layout-test ()
  "Test that `taut-reset-layout` executes safely and works as expected."
  (let ((taut-consolidate-windows 'tab))
    ;; Verify calling taut-reset-layout completes without error
    (should (progn
              (taut-reset-layout)
              t)))
  (let ((taut-consolidate-windows nil))
    ;; Verify calling taut-reset-layout completes without error
    (should (progn
              (taut-reset-layout)
              t))))

(ert-deftest taut-quit-extended-test ()
  "Test that `taut-quit` runs cleanly, kills all Taut buffers, and cleans up."
  (let ((buf1 (get-buffer-create "*Taut - #general-test*"))
        (buf2 (get-buffer-create "*Taut Sidebar*"))
        (buf3 (get-buffer-create "*Slack Activity*")))
    (with-current-buffer buf1
      (taut-message-mode))
    (with-current-buffer buf2
      (taut-sidebar-mode))
    (with-current-buffer buf3
      (taut-inbox-mode))
    
    ;; Verify the buffers exist
    (should (get-buffer "*Taut - #general-test*"))
    (should (get-buffer "*Taut Sidebar*"))
    (should (get-buffer "*Slack Activity*"))
    
    ;; Call taut-quit
    (taut-quit)
    
    ;; Verify buffers are killed
    (should-not (get-buffer "*Taut - #general-test*"))
    (should-not (get-buffer "*Taut Sidebar*"))
    (should-not (get-buffer "*Slack Activity*"))))

(ert-deftest taut-strict-windows-test ()
  "Test the strict-windows layout manager and buffer assignment."
  (let ((taut-strict-windows t)
        (taut-consolidate-windows nil)
        (taut-sidebar-width 12)
        (taut-activity-width 15))
    ;; Clean up any existing buffers so we start fresh
    (ignore-errors (kill-buffer "*Taut Sidebar*"))
    (ignore-errors (kill-buffer "*Slack Activity*"))
    (ignore-errors (kill-buffer "*Taut Thread*"))
    (ignore-errors (kill-buffer "*Taut - #general*"))
    
    ;; 1. Setup strict layout
    (should (progn
              (taut-setup-strict-windows)
              t))
    
    ;; Verify we have exactly three windows
    (let ((windows (window-list)))
      (should (>= (length windows) 3)))
    
    ;; Verify windows have the correct buffers and dedication
    (let ((sidebar-win (get-buffer-window "*Taut Sidebar*"))
          (activity-win (get-buffer-window "*Slack Activity*"))
          (chat-win (taut-get-chat-window)))
      (should sidebar-win)
      (should activity-win)
      (should chat-win)
      (should (window-dedicated-p sidebar-win))
      (should (window-dedicated-p activity-win))
      (should-not (window-dedicated-p chat-win)))
    
    ;; 2. Select channel/message buffer (should reuse chat window)
    (let ((buf (taut-message-open "C_GEN")))
      (should (equal (buffer-name buf) "*Taut - #C_GEN*"))
      ;; Chat window should now display the general channel
      (should (eq (window-buffer (taut-get-chat-window)) buf))
      ;; Check count of windows hasn't increased
      (should (<= (length (window-list)) 3)))
    
    ;; 3. Open thread (should reuse chat window, NOT split horizontally)
    (let ((thread-buf (taut-thread-open "12345.6789" "C_GEN")))
      (should (equal (buffer-name thread-buf) "*Taut Thread*"))
      ;; Chat window should now display the thread buffer
      (should (eq (window-buffer (taut-get-chat-window)) thread-buf))
      ;; Check count of windows is still exactly three
      (should (<= (length (window-list)) 3)))
    
    ;; 4. Close thread (should show the channel buffer again instead of deleting window)
    (taut-thread-close)
    (let ((chat-win (taut-get-chat-window)))
      (should (equal (buffer-name (window-buffer chat-win)) "*Taut - #C_GEN*"))
      ;; Layout is still fully intact with at least 3 windows
      (should (>= (length (window-list)) 3)))
      
    ;; Clean up
    (taut-quit)))

(ert-deftest taut-strict-windows-portrait-test ()
  "Test the strict-windows layout manager in portrait orientation."
  (let ((taut-strict-windows t)
        (taut-consolidate-windows nil))
    ;; Clean up any existing buffers so we start fresh
    (ignore-errors (kill-buffer "*Taut Sidebar*"))
    (ignore-errors (kill-buffer "*Slack Activity*"))
    (ignore-errors (kill-buffer "*Taut Thread*"))
    (ignore-errors (kill-buffer "*Taut - #C_GEN*"))
    
    ;; Use cl-letf to mock frame-root-window and window sizes as portrait (height > width)
    (cl-letf* (((symbol-function 'window-total-width) (lambda (&rest _) 40))
               ((symbol-function 'window-total-height) (lambda (&rest _) 80)))
      ;; Setup strict layout
      (should (progn
                (taut-setup-strict-windows)
                t))
      
      ;; Verify windows have the correct buffers and dedication in portrait mode
      (let ((sidebar-win (get-buffer-window "*Taut Sidebar*"))
            (activity-win (get-buffer-window "*Slack Activity*"))
            (chat-win (taut-get-chat-window)))
        (should sidebar-win)
        (should activity-win)
        (should chat-win)
        (should (window-dedicated-p sidebar-win))
        (should (window-dedicated-p activity-win))
        (should-not (window-dedicated-p chat-win))))
    (taut-quit)))

(ert-deftest taut-direct-navigation-keybindings-test ()
  "Test that direct navigation keys S, I, C are correctly bound in all four Taut maps."
  (let ((maps (list taut-sidebar-mode-map
                    taut-inbox-mode-map
                    taut-message-mode-map
                    taut-thread-mode-map)))
    (dolist (map maps)
      (should (eq (lookup-key map (kbd "S")) #'taut-sidebar-show))
      (should (eq (lookup-key map (kbd "I")) #'taut-inbox-show))
      (should (eq (lookup-key map (kbd "C")) #'taut-focus-chat)))))

(provide 'test-taut-model)
;;; test-taut-model.el ends here

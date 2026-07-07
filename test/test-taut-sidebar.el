;;; test-taut-sidebar.el --- Unit tests for taut-sidebar.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; Unit tests for Taut Sidebar (taut-sidebar.el).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'taut-model)
(require 'taut-sidebar)
(require 'taut-test-fixtures)

(ert-deftest taut-sidebar-render-test ()
  "Test rendering sidebar sections and correct text properties."
  (taut-initialize-mock-data)
  
  ;; Reset section state to default known state
  (setq taut-sidebar-section-state
        '((starred . t)
          (bookmarks . t)
          (channels . t)
          (dms . t)
          (threads . t)
          (hidden . nil)))
  
  ;; Render the sections in a temporary mock buffer
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (taut-use-icons nil)) ; Use unicode/plain fallbacks for simple string matching
      (cl-letf (((symbol-function 'float-time) (lambda () 1688460000.0)))
        (taut-sidebar--render-sections))
      
      (let ((content (buffer-string)))
        ;; Verify section headers are present
        (should (string-match-p "★ STARRED" content))
        (should (string-match-p "♯ CHANNELS" content))
        (should (string-match-p "✉ DIRECT MESSAGES" content))
        (should (string-match-p "THREADS" content))
        (should (string-match-p "HIDDEN" content))
        
        ;; Verify inbox activity shortcut is rendered
        (should (string-match-p "Slack Inbox" content))
        
        ;; Verify channels lists
        (should (string-match-p "general" content))
        (should (string-match-p "development" content))
        (should (string-match-p "ideas" content))
        (should (string-match-p "Bob Jones" content))
        
        ;; Verify unread badges
        ;; Bob DM is unread, should show unread count next to Bob Jones
        (should (string-match-p "Bob Jones (1)" content))
        
        ;; Check text properties for a channel
        (goto-char (point-min))
        (let ((pos (search-forward "general" nil t)))
          (should pos)
          (backward-char 3)
          (should (equal (get-text-property (point) 'taut-channel-id) "C_GENERAL")))
        
        ;; Check text properties for a thread
        (goto-char (point-min))
        (let ((pos (search-forward "by @alice" nil t)))
          (should pos)
          (backward-char 3)
          (should (equal (get-text-property (point) 'taut-thread-ts) "1688460000.0001"))
          (should (equal (get-text-property (point) 'taut-channel-id) "C_DEV")))

        ;; Check text properties for a bookmark
        (goto-char (point-min))
        (let ((pos (search-forward "on 04-jul-23" nil t)))
          (should pos)
          (backward-char 3)
          (let ((msg (get-text-property (point) 'taut-bookmark-msg)))
            (should msg)
            (should (equal (taut-message-id msg) "m2_1"))))))))

(ert-deftest taut-sidebar-toggle-section-test ()
  "Test that collapsing sections prevents rendering their items."
  (taut-initialize-mock-data)
  
  ;; Reset section state to default known state
  (setq taut-sidebar-section-state
        '((starred . t)
          (bookmarks . t)
          (channels . t)
          (dms . t)
          (threads . t)
          (hidden . nil)))
  
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (taut-use-icons nil))
      (taut-sidebar--render-sections)
      ;; When expanded, "ideas" channel should be in the buffer (since it is unstarred public)
      (should (string-match-p "ideas" (buffer-string)))))
  
  ;; Toggle section to collapsed
  (taut-sidebar-toggle-section 'channels)
  (should-not (alist-get 'channels taut-sidebar-section-state))
  
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (taut-use-icons nil))
      (taut-sidebar--render-sections)
      ;; When collapsed, "ideas" channel should NOT be in the buffer
      (should-not (string-match-p "ideas" (buffer-string)))))
  
  ;; Re-expand for subsequent tests / clean state
  (taut-sidebar-toggle-section 'channels)
  (should (alist-get 'channels taut-sidebar-section-state)))

(ert-deftest taut-sidebar-mark-read-and-hide-test ()
  "Test sidebar commands for marking as read and toggling visibility."
  (taut-initialize-mock-data)
  
  ;; Reset section state to default known state
  (setq taut-sidebar-section-state
        '((starred . t)
          (bookmarks . t)
          (channels . t)
          (dms . t)
          (threads . t)
          (hidden . nil)))
  
  (let ((chan (taut-model-get-channel "C_BOB_DM")))
    (should chan)
    ;; Initially has 1 unread message from Bob
    (should (equal (taut-channel-unread-count chan) 1))
    
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (taut-use-icons nil))
        (taut-sidebar--render-sections)
        (goto-char (point-min))
        ;; Move to Bob channel line (resolved name Bob Jones)
        (should (search-forward "Bob Jones" nil t))
        (backward-char 4)
        ;; Verify taut-channel-id text property is present
        (should (equal (get-text-property (point) 'taut-channel-id) "C_BOB_DM"))
        
        ;; Call mark all read command
        (taut-sidebar-mark-all-read)
        ;; Check that unread-count is now 0
        (should (equal (taut-channel-unread-count chan) 0)))))
  
  ;; Test toggling hidden state of a public channel (e.g. C_IDEAS)
  (let ((chan-ideas (taut-model-get-channel "C_IDEAS")))
    (should chan-ideas)
    (should-not (taut-channel-is-hidden chan-ideas))
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (taut-use-icons nil))
        (taut-sidebar--render-sections)
        (goto-char (point-min))
        (should (search-forward "ideas" nil t))
        (backward-char 3)
        (should (equal (get-text-property (point) 'taut-channel-id) "C_IDEAS"))
        
        ;; Mock cache saving function if it exists
        (cl-letf (((symbol-function 'taut-cache-save-channel) (lambda (_c) nil)))
          (taut-sidebar-toggle-channel-hidden))
        
        ;; Now ideas channel should be hidden
        (should (taut-channel-is-hidden chan-ideas))
        
        ;; Toggle back to visible
        (cl-letf (((symbol-function 'taut-cache-save-channel) (lambda (_c) nil)))
          (taut-sidebar-toggle-channel-hidden))
        (should-not (taut-channel-is-hidden chan-ideas))))))

(ert-deftest taut-sidebar-activate-test ()
  "Test activating channel, thread, and section items under cursor."
  (taut-initialize-mock-data)
  
  (let ((opened-channel nil)
        (opened-thread nil)
        (toggled-section nil))
    (cl-letf (((symbol-function 'taut-sidebar-open-channel)
               (lambda (chan-id) (setq opened-channel chan-id)))
              ((symbol-function 'taut-sidebar-open-thread)
               (lambda (ts &optional _cid) (setq opened-thread ts)))
              ((symbol-function 'taut-sidebar-toggle-section)
               (lambda (sec) (setq toggled-section sec))))
      
      (with-temp-buffer
        ;; 1. Activate channel
        (insert "  general\n")
        (add-text-properties 3 10 (list 'taut-channel-id "C_GENERAL"))
        (goto-char 5)
        (taut-sidebar-activate)
        (should (equal opened-channel "C_GENERAL"))
        
        ;; 2. Activate thread
        (erase-buffer)
        (insert "  Thread 12345\n")
        (add-text-properties 3 14 (list 'taut-thread-ts "1688460000.0001" 'taut-channel-id "C_DEV"))
        (goto-char 5)
        (taut-sidebar-activate)
        (should (equal opened-thread "1688460000.0001"))
        
        ;; 3. Activate section
        (erase-buffer)
        (insert "▼ ★ STARRED\n")
        (add-text-properties 1 11 (list 'taut-section 'starred))
        (goto-char 5)
        (taut-sidebar-activate)
        (should (eq toggled-section 'starred))))))

(ert-deftest taut-sidebar-thread-recentness-test ()
  "Test filtering of threads based on 14-day recentness limit."
  (taut-initialize-mock-data)
  ;; Current simulated time: Jul 18, 2023 04:40:00 UTC (1689655200.0)
  ;; 14 days ago is: 1689655200.0 - 1209600 = 1688445600.0
  (let ((current-time 1689655200.0))
    ;; 1. Root ts is 1688460000.0001 (Jul 4, 2023), which is > 1688445600.0 -> recent!
    (should (taut-sidebar--thread-recent-p "1688460000.0001" current-time))
    
    ;; 2. Root ts is 1688400000.0000 (Jul 3, 2023), which is < 1688445600.0 -> older than 14 days
    ;; Underneath, there are no replies, so it should NOT be recent
    (setf (gethash "1688400000.0000" taut-threads) nil)
    (should-not (taut-sidebar--thread-recent-p "1688400000.0000" current-time))
    
    ;; 3. Root ts is 1688400000.0000, but there's a reply at 1688450000.0001 (Jul 4, 2023) -> recent!
    (setf (gethash "1688400000.0000" taut-threads)
          (list (make-taut-message :id "r1" :channel-id "C_DEV" :user-id "U_ALICE"
                                   :text "Recent reply" :ts "1688450000.0001")))
    (should (taut-sidebar--thread-recent-p "1688400000.0000" current-time))))

(ert-deftest taut-sidebar-connection-status-indicator-test ()
  "Test rendering of connection status indicator in the sidebar."
  (taut-initialize-mock-data)
  (let ((taut-socket-ws nil))
    ;; 1. Check Disconnected rendering
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (taut-use-icons nil))
        (taut-sidebar--render-connection-status)
        (let ((hlf header-line-format))
          (should hlf)
          (should (string-match-p "○" hlf))
          (should (string-match-p "Offline" hlf))
          (should-not (string-match-p "Connected" hlf))))))

  ;; 2. Check Connected rendering
  (let ((taut-socket-ws t))
    (cl-letf (((symbol-function 'websocket-openp) (lambda (_ws) t)))
      (with-temp-buffer
        (let ((inhibit-read-only t)
              (taut-use-icons nil))
          (taut-sidebar--render-connection-status)
          (let ((hlf header-line-format))
            (should hlf)
            (should (string-match-p "●" hlf))
            (should (string-match-p "Connected" hlf))
            (should-not (string-match-p "Offline" hlf))))))))

(provide 'test-taut-sidebar)
;;; test-taut-sidebar.el ends here

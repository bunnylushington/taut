;;; test-taut-inbox.el --- Unit tests for taut-inbox.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; Unit tests for Taut Slack Activity Feed / Inbox (taut-inbox.el).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'taut-inbox)
(require 'taut-model)
(require 'taut-test-fixtures)

(ert-deftest taut-inbox-get-icon-badge-test ()
  "Test `taut-inbox--get-icon-badge` formatting."
  (let ((taut-use-icons nil))
    (should (string-match-p "👤 DM" (taut-inbox--get-icon-badge 'dm)))
    (should (string-match-p "@ MENTION" (taut-inbox--get-icon-badge 'mention)))
    (should (string-match-p "💬 THREAD" (taut-inbox--get-icon-badge 'thread-update)))
    (should (string-match-p "♯ CHANNEL" (taut-inbox--get-icon-badge 'channel)))
    (should (string-match-p "💬 CHAT" (taut-inbox--get-icon-badge 'other)))))

(ert-deftest taut-inbox-format-date-test ()
  "Test relative date formatting and grouping in `taut-inbox`."
  (cl-letf (((symbol-function 'float-time) (lambda () 1688500000.0)))
    ;; 1. Today (diff = 1000)
    (let* ((ts-today "1688499000.0000")
           (rel (taut-inbox--format-relative-date ts-today))
           (grp (taut-inbox--format-date-group ts-today)))
      (should (string-match-p "^Today " rel))
      (should (equal grp "Today")))
    
    ;; 2. Yesterday (diff = 100000)
    (let* ((ts-yesterday "1688400000.0000")
           (rel (taut-inbox--format-relative-date ts-yesterday))
           (grp (taut-inbox--format-date-group ts-yesterday)))
      (should (string-match-p "^Yesterday " rel))
      (should (equal grp "Yesterday")))

    ;; 3. Weekday (diff = 300000)
    (let* ((ts-weekday "1688200000.0000")
           (rel (taut-inbox--format-relative-date ts-weekday))
           (grp (taut-inbox--format-date-group ts-weekday)))
      (should (string-match-p "^[A-Za-z]+ " rel))
      (should (string-match-p "^[A-Za-z]+$" grp)))

    ;; 4. Nil or invalid
    (should (equal (taut-inbox--format-relative-date nil) "--:--"))
    (should (equal (taut-inbox--format-date-group nil) "Older Activity"))))

(ert-deftest taut-inbox-clean-snippet-test ()
  "Test snippet cleaning, truncation, and mention translations."
  (taut-model-clear-all)
  (should (equal (taut-inbox--clean-snippet nil) ""))
  (should (equal (taut-inbox--clean-snippet "  hello \n world \t ") "hello world"))
  (let ((long-str (make-string 100 ?a)))
    (should (equal (taut-inbox--clean-snippet long-str) (concat (make-string 80 ?a) "..."))))
  
  ;; Setup mock database state
  (let ((u-alice (make-taut-user :id "U_ALICE" :username "alice" :real-name "Alice Smith"))
        (c-general (make-taut-channel :id "C_GENERAL" :name "general" :type 'public)))
    (taut-model-add-user u-alice)
    (taut-model-add-channel c-general)
    
    ;; 1. User mentions with and without labels
    (should (equal (taut-inbox--clean-snippet "Please ask <@U_ALICE>") "Please ask @alice"))
    (should (equal (taut-inbox--clean-snippet "Ping <@U_ALICE|alice-smith>") "Ping @alice-smith"))
    (should (equal (taut-inbox--clean-snippet "Ask <@U_NONEXISTENT>") "Ask @U_NONEXISTENT"))
    
    ;; 2. Channel mentions with and without labels
    (should (equal (taut-inbox--clean-snippet "Go to <#C_GENERAL>") "Go to #general"))
    (should (equal (taut-inbox--clean-snippet "Join <#C_GENERAL|general-announcements>") "Join #general-announcements"))
    (should (equal (taut-inbox--clean-snippet "Go to <#C_NONEXISTENT>") "Go to #C_NONEXISTENT"))))

(ert-deftest taut-inbox-render-test ()
  "Test rendering the inbox buffer with correct items and layouts."
  (taut-initialize-mock-data)
  (setq taut-current-user-id "U_ME")
  
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (taut-use-icons nil))
      ;; Call the rendering function of the inbox feed
      (taut-inbox--render-feed)
      
      (let ((content (buffer-string)))
        ;; The mock data from taut-initialize-mock-data should generate some unread items:
        ;; - Bob Jones DM has is-unread t (C_BOB_DM, user-id U_BOB, "Did you get that script command...")
        ;; So it should render.
        (should (string-match-p "Bob Jones" content))
        (should (string-match-p "Did you get that script command" content))
        ;; Verify the bullet point/unread star is present:
        (should (string-match-p "●" content))
        ;; Verify DM badge:
        (should (string-match-p "DM" content))))))

(provide 'test-taut-inbox)
;;; test-taut-inbox.el ends here

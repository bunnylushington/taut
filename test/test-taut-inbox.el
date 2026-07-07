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
    (should (string-match-p "✉ DM" (taut-inbox--get-icon-badge 'dm)))
    (should (string-match-p "🔔 MENTION" (taut-inbox--get-icon-badge 'mention)))
    (should (string-match-p "🧵 CH-THREAD" (taut-inbox--get-icon-badge 'thread-update)))
    (should (string-match-p "♯ CHANNEL" (taut-inbox--get-icon-badge 'channel)))
    (should (string-match-p "💬 CHAT" (taut-inbox--get-icon-badge 'other)))

    ;; Test with item and channel mock for DM-THREAD and CH-THREAD
    (taut-model-clear-all)
    (taut-model-add-channel (make-taut-channel :id "C_DM" :type 'dm :name "alice"))
    (taut-model-add-channel (make-taut-channel :id "C_PUB" :type 'public :name "general"))
    
    (let ((dm-item (make-taut-inbox-item :channel-id "C_DM"))
          (pub-item (make-taut-inbox-item :channel-id "C_PUB")))
      (should (string-match-p "🧵 DM-THREAD" (taut-inbox--get-icon-badge 'thread-update dm-item)))
      (should (string-match-p "🧵 CH-THREAD" (taut-inbox--get-icon-badge 'thread-update pub-item))))))

(ert-deftest taut-inbox-format-date-test ()
  "Test relative date formatting and grouping in `taut-inbox`."
  (cl-letf (((symbol-function 'float-time) (lambda () 1688500000.0)))
    ;; 1. Today (diff = 1000)
    (let* ((ts-today "1688499000.0000")
           (rel (taut-inbox--format-relative-date ts-today))
           (grp (taut-inbox--format-date-group ts-today)))
      (should (string-match-p "^[0-9]\\{2\\}:[0-9]\\{2\\}$" rel))
      (should (equal grp "Today")))
    
    ;; 2. Yesterday (diff = 100000)
    (let* ((ts-yesterday "1688400000.0000")
           (rel (taut-inbox--format-relative-date ts-yesterday))
           (grp (taut-inbox--format-date-group ts-yesterday)))
      (should (string-match-p "^[0-9]\\{2\\}:[0-9]\\{2\\}$" rel))
      (should (equal grp "Yesterday")))

    ;; 3. Weekday (diff = 300000)
    (let* ((ts-weekday "1688200000.0000")
           (rel (taut-inbox--format-relative-date ts-weekday))
           (grp (taut-inbox--format-date-group ts-weekday)))
      (should (string-match-p "^[0-9]\\{2\\}:[0-9]\\{2\\}$" rel))
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
  
  (cl-letf (((symbol-function 'float-time) (lambda () 1688500000.0)))
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
          (should (string-match-p "DM" content)))))))

(ert-deftest taut-inbox-date-filtering-test ()
  "Test that date filtering works as expected for today, last 7 days, last 30 days, and all time."
  (cl-letf (((symbol-function 'float-time) (lambda () 1688500000.0)))
    ;; 1688500000.0 is Tuesday, July 4, 2023.
    (let* ((item-today (make-taut-inbox-item :ts "1688500000.0000")) ; day-diff = 0
           (item-yesterday (make-taut-inbox-item :ts "1688413600.0000")) ; day-diff = 1
           (item-6-days-ago (make-taut-inbox-item :ts "1687981600.0000")) ; day-diff = 6
           (item-8-days-ago (make-taut-inbox-item :ts "1687808800.0000")) ; day-diff = 8
           (item-29-days-ago (make-taut-inbox-item :ts "1685994400.0000")) ; day-diff = 29
           (item-35-days-ago (make-taut-inbox-item :ts "1685476000.0000"))) ; day-diff = 35
      
      ;; Test 'today filter
      (let ((taut-inbox-date-filter 'today))
        (should (taut-inbox--item-matches-date-filter-p item-today))
        (should-not (taut-inbox--item-matches-date-filter-p item-yesterday))
        (should-not (taut-inbox--item-matches-date-filter-p item-6-days-ago))
        (should-not (taut-inbox--item-matches-date-filter-p item-8-days-ago)))

      ;; Test 'last-7 filter
      (let ((taut-inbox-date-filter 'last-7))
        (should (taut-inbox--item-matches-date-filter-p item-today))
        (should (taut-inbox--item-matches-date-filter-p item-yesterday))
        (should (taut-inbox--item-matches-date-filter-p item-6-days-ago))
        (should-not (taut-inbox--item-matches-date-filter-p item-8-days-ago)))

      ;; Test 'last-30 filter
      (let ((taut-inbox-date-filter 'last-30))
        (should (taut-inbox--item-matches-date-filter-p item-today))
        (should (taut-inbox--item-matches-date-filter-p item-yesterday))
        (should (taut-inbox--item-matches-date-filter-p item-6-days-ago))
        (should (taut-inbox--item-matches-date-filter-p item-8-days-ago))
        (should (taut-inbox--item-matches-date-filter-p item-29-days-ago))
        (should-not (taut-inbox--item-matches-date-filter-p item-35-days-ago)))

      ;; Test 'all filter
      (let ((taut-inbox-date-filter 'all))
        (should (taut-inbox--item-matches-date-filter-p item-today))
        (should (taut-inbox--item-matches-date-filter-p item-yesterday))
        (should (taut-inbox--item-matches-date-filter-p item-6-days-ago))
        (should (taut-inbox--item-matches-date-filter-p item-8-days-ago))
        (should (taut-inbox--item-matches-date-filter-p item-29-days-ago))
        (should (taut-inbox--item-matches-date-filter-p item-35-days-ago))))))

(ert-deftest taut-inbox-code-filtering-test ()
  "Test that the code filter accurately isolates items with code blocks."
  (taut-model-clear-all)
  (let* ((item-no-code (make-taut-inbox-item :id "msg-1" :snippet "Hello, how are you?"))
         (item-with-code-snippet (make-taut-inbox-item :id "msg-2" :snippet "Here is the code:\n```elisp\n(defun foo ())\n```"))
         (item-with-code-model (make-taut-inbox-item :id "msg-3" :snippet "See below"))
         ;; Also create a message object in the model for msg-3
         (msg-3 (make-taut-message :ts "msg-3" :text "Here is code in the text:\n```python\nprint(123)\n```")))
    
    (taut-model-add-message msg-3)
    
    ;; Test predicate directly
    (should-not (taut-inbox-item-has-code-p item-no-code))
    (should (taut-inbox-item-has-code-p item-with-code-snippet))
    (should (taut-inbox-item-has-code-p item-with-code-model))
    
    ;; Test the commands and rendering filtering flow
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (taut-inbox-filter 'code))
        ;; Mock model retrieval to return our test list
        (cl-letf (((symbol-function 'taut-model-get-activity-items)
                   (lambda () (list item-no-code item-with-code-snippet item-with-code-model)))
                  ((symbol-function 'taut-inbox--item-matches-date-filter-p)
                   (lambda (_) t)))
          (taut-inbox--render-feed)
          (let ((content (buffer-string)))
            ;; Should render items with code blocks
            (should (string-match-p "Here is the code" content))
            (should (string-match-p "See below" content))
            ;; Should NOT render item without code blocks
            (should-not (string-match-p "Hello, how are you" content))))))))

(ert-deftest taut-inbox-navigation-helpers-test ()
  "Test helpers like `taut-inbox--find-item-point`, `taut-inbox--move-to-next-item`, and `taut-inbox--move-to-prev-item`."
  (with-temp-buffer
    (let ((item1 (make-taut-inbox-item :id "item-1"))
          (item2 (make-taut-inbox-item :id "item-2")))
      ;; Render mock rows manually
      (let ((start1 (point)))
        (insert "  Row 1")
        (add-text-properties start1 (point) (list 'taut-inbox-item item1))
        (insert "\n"))
      (insert "  Header non-item line\n")
      (let ((start2 (point)))
        (insert "  Row 2")
        (add-text-properties start2 (point) (list 'taut-inbox-item item2))
        (insert "\n"))
      
      ;; 1. Test find item point
      (should (equal (taut-inbox--find-item-point "item-1") 1))
      (should (integerp (taut-inbox--find-item-point "item-2")))
      (should-not (taut-inbox--find-item-point "nonexistent"))
      
      ;; 2. Test move-to-next-item and move-to-prev-item
      (goto-char (point-min))
      ;; From line 1 (item 1), next item should be item 2 (skipping header)
      (let ((next (taut-inbox--move-to-next-item)))
        (should (equal (taut-inbox-item-id next) "item-2"))
        (should (equal (get-text-property (point) 'taut-inbox-item) item2)))
      
      ;; Moving next from item 2 should return nil
      (should-not (taut-inbox--move-to-next-item))
      
      ;; From end, move prev should return item 2
      (goto-char (point-max))
      (let ((prev (taut-inbox--move-to-prev-item)))
        (should (equal (taut-inbox-item-id prev) "item-2"))
        (should (equal (get-text-property (point) 'taut-inbox-item) item2)))
      
      ;; Move prev from item 2 should return item 1 (skipping the non-item header!)
      (let ((prev (taut-inbox--move-to-prev-item)))
        (should (equal (taut-inbox-item-id prev) "item-1"))
        (should (equal (get-text-property (point) 'taut-inbox-item) item1)))
      
      ;; Move prev from item 1 should return nil
      (should-not (taut-inbox--move-to-prev-item)))))

(provide 'test-taut-inbox)
;;; test-taut-inbox.el ends here

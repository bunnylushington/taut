;;; test-taut-search.el --- ERT unit tests for taut-search.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; Unit tests for the context-aware search engine, rendering flow, merging/deduplication,
;; vague relative time calculation, and jump-to-message mechanics.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'taut-search)
(require 'taut-test-fixtures)

(ert-deftest taut-search-vague-time-test ()
  "Test that `taut-search--vague-time' segments times into correct human relative slots."
  (let* ((now (floor (float-time)))
         (today-ts (number-to-string now))
         (yesterday-ts (number-to-string (- now 90000)))
         (this-week-ts (number-to-string (- now (* 3 86400))))
         (last-week-ts (number-to-string (- now (* 10 86400))))
         (earlier-this-year-ts (number-to-string (- now (* 60 86400))))
         (earlier-years-ts (number-to-string (- now (* 400 86400)))))
    (should (equal (taut-search--vague-time today-ts) "Today"))
    (should (equal (taut-search--vague-time yesterday-ts) "Yesterday"))
    (should (equal (taut-search--vague-time this-week-ts) "This Week"))
    (should (equal (taut-search--vague-time last-week-ts) "Last Week"))
    ;; Check prefix for other bounds
    (should (string-prefix-p "Earlier this Year" (taut-search--vague-time earlier-this-year-ts)))
    (should (string-prefix-p "in " (taut-search--vague-time earlier-years-ts)))))

(ert-deftest taut-search-quick-scoping-test ()
  "Test that `taut-search-quick' sets scope correctly based on buffer context."
  (let ((taut-current-channel-id "C_TEST_CHAN"))
    (cl-letf (((symbol-function 'taut-search--execute)
               (lambda (query chan-id user-id)
                 (should (equal query "hello"))
                 (should (equal chan-id "C_TEST_CHAN"))
                 (should (null user-id))
                 t)))
      (taut-search-quick "hello"))))

(ert-deftest taut-search-execute-and-render-test ()
  "Test that `taut-search--execute' fetches, merges, dedups, and renders results."
  (taut-initialize-mock-data)
  (let ((mock-local-msgs (list (make-taut-message
                                :id "msg_local_1"
                                :channel-id "C_GENERAL"
                                :user-id "U_ALICE"
                                :text "Local copy of query match"
                                :ts "1688500000.0000")
                               (make-taut-message
                                :id "msg_local_dup"
                                :channel-id "C_GENERAL"
                                :user-id "U_BOB"
                                :text "Duplicate query match"
                                :ts "1688600000.0000")))
        (mock-api-results '((ok . t)
                            (messages . ((matches . (((ts . "1688600000.0000")
                                                      (channel . ((id . "C_GENERAL")))
                                                      (user . "U_BOB")
                                                      (text . "Duplicate query match"))
                                                     ((ts . "1688700000.0000")
                                                      (channel . ((id . "C_GENERAL")))
                                                      (user . "U_CAROL")
                                                      (text . "API only match")))))))))
    (cl-letf (((symbol-function 'taut-cache-search-messages)
               (lambda (query channel-id user-id)
                 (should (equal query "match"))
                 mock-local-msgs))
              ((symbol-function 'taut-api-search-messages)
               (lambda (query sort sort-dir)
                 (should (equal query "match"))
                 mock-api-results)))
      (taut-search--execute "match" nil nil)
      (let ((buf (get-buffer "*Taut Search*")))
        (should buf)
        (with-current-buffer buf
          (should (eq major-mode 'taut-search-mode))
          (should (equal taut-search-current-query "match"))
          (should (null taut-search-current-channel-id))
          (should (null taut-search-current-user-id))
          (let ((content (buffer-string)))
            ;; Should render header
            (should (string-match-p "TAUT SEARCH RESULTS" content))
            (should (string-match-p "Query: match" content))
            ;; Should merge and dedup (total 3 messages: msg_local_1, dup, API-only)
            (should (string-match-p "Found 3 matches:" content))
            ;; Check that we rendered the local copy and API match
            (should (string-match-p "Local copy of query match" content))
            (should (string-match-p "Duplicate query match" content))
            (should (string-match-p "API only match" content))
            ;; Check text properties of entries
            (goto-char (point-min))
            (should (search-forward "Local copy of query match"))
            (should (equal (get-text-property (point) 'taut-message-ts) "1688500000.0000"))
            (should (equal (get-text-property (point) 'taut-channel-id) "C_GENERAL")))))))
  ;; Clean up
  (when (get-buffer "*Taut Search*")
    (kill-buffer "*Taut Search*")))

(ert-deftest taut-search-activate-jump-test ()
  "Test that `taut-search-activate' opens the right channel and highlights."
  (taut-initialize-mock-data)
  (let ((test-ts "1688500000.0000")
        (test-chan "C_GENERAL")
        opened-chan-id
        goto-ts-called)
    (cl-letf (((symbol-function 'taut-message-open)
               (lambda (chan-id)
                 (setq opened-chan-id chan-id)))
              ((symbol-function 'taut-message-goto-ts)
               (lambda (ts)
                 (should (equal ts test-ts))
                 (setq goto-ts-called t)
                 t))
              ((symbol-function 'taut-search--flash-message)
               (lambda (pos)
                 (should pos)
                 t)))
      (let* ((buf-name "*Taut - #general*")
             (chan-buf (generate-new-buffer buf-name)))
        (unwind-protect
            (cl-letf (((symbol-function 'get-buffer)
                       (lambda (name)
                         (if (equal name buf-name)
                             chan-buf
                           nil))))
              ;; Execute from within the mock search results buffer
              (with-temp-buffer
                (insert "Match body here")
                (add-text-properties (point-min) (point-max)
                                     (list 'taut-message-ts test-ts
                                           'taut-channel-id test-chan))
                (goto-char (point-min))
                (taut-search-activate)
                (should (equal opened-chan-id test-chan))
                (should goto-ts-called)))
          (when (buffer-live-p chan-buf)
            (kill-buffer chan-buf)))))))

(provide 'test-taut-search)
;;; test-taut-search.el ends here

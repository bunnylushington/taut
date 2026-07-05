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

(provide 'test-taut-model)
;;; test-taut-model.el ends here

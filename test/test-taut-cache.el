;;; test-taut-cache.el --- Unit tests for taut-cache.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; Unit tests for Taut SQLite caching persistent engine (taut-cache.el).

;;; Code:

(require 'ert)
(require 'taut-model)
(require 'taut-cache)

(ert-deftest taut-cache-persistence-test ()
  "Test saving and loading users, channels, and messages from SQLite."
  (skip-unless (taut-cache--available-p))
  
  (let* ((temp-db-file (make-temp-file "taut-cache-test-db-"))
         (taut-cache-db-path temp-db-file)
         ;; Reset database connection variable to force new initialization
         (taut-cache--db nil))
    
    (unwind-protect
        (progn
          ;; 1. Clear model state and populate with test structures
          (taut-model-clear-all)
          (let ((user (make-taut-user :id "U_ALICE" :username "alice" :real-name "Alice Smith" :presence 'online :is-me t))
                (chan (make-taut-channel :id "C_GENERAL" :name "general" :type 'public :unread-count 2 :is-starred t))
                (msg (make-taut-message :id "msg1" :channel-id "C_GENERAL" :user-id "U_ALICE" :text "Persistence test" :ts "1688460000.0001" :is-unread t)))
            
            ;; 2. Save directly to the cache
            (taut-cache-save-user user)
            (taut-cache-save-channel chan)
            (taut-cache-save-message msg)
            (taut-cache-save-watched-thread "1688460000.0001")
            
            ;; 3. Clear memory entirely
            (taut-model-clear-all)
            (should (= (hash-table-count taut-users) 0))
            (should (= (hash-table-count taut-channels) 0))
            (should (= (hash-table-count taut-messages) 0))
            
            ;; 4. Load all back from SQLite
            (taut-cache-load-all)
            
            ;; 5. Assert database values are exactly restored
            (let ((loaded-user (taut-model-get-user "U_ALICE")))
              (should loaded-user)
              (should (equal (taut-user-username loaded-user) "alice"))
              (should (taut-user-is-me loaded-user)))
            
            (let ((loaded-chan (taut-model-get-channel "C_GENERAL")))
              (should loaded-chan)
              (should (equal (taut-channel-name loaded-chan) "general"))
              (should (= (taut-channel-unread-count loaded-chan) 2))
              (should (taut-channel-is-starred loaded-chan)))
            
            (let ((loaded-msgs (taut-model-get-messages "C_GENERAL")))
              (should (= (length loaded-msgs) 1))
              (should (equal (taut-message-text (car loaded-msgs)) "Persistence test"))
              (should (taut-message-is-unread (car loaded-msgs))))
            
            (should (member "1688460000.0001" taut-watched-threads))))
      
      ;; Cleanup connection and temporary file
      (when (and taut-cache--db (sqlitep taut-cache--db))
        (sqlite-close taut-cache--db))
      (when (file-exists-p temp-db-file)
        (delete-file temp-db-file)))))

(provide 'test-taut-cache)
;;; test-taut-cache.el ends here

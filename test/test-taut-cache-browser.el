;;; test-taut-cache-browser.el --- Unit tests for taut-cache-browser.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; Unit tests for Taut's premium media cache browser.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'taut-cache-browser)
(require 'taut-model)
(require 'taut-test-fixtures)

(ert-deftest taut-cache-browser-build-metadata-index-test ()
  "Test that `taut-cache-browser--build-metadata-index' correctly maps hashes to slack metadata."
  (taut-initialize-mock-data)
  
  ;; Setup a mock channel message with a file attachment
  (setq taut-messages (make-hash-table :test 'equal))
  (let* ((msg (make-taut-message
               :id "msg-123"
               :user-id "U_ALICE"
               :text "Here is a document"
               :ts "1688500000.0001"
               :files (list
                       (list (cons 'name "annual_report.pdf")
                             (cons 'url_private_download "https://files.slack.com/files/annual_report.pdf"))))))
    (puthash "C_DEV" (list msg) taut-messages))

  ;; Setup a mock thread reply message with a file attachment
  (setq taut-threads (make-hash-table :test 'equal))
  (let* ((reply (make-taut-message
                 :id "msg-456"
                 :user-id "U_BOB"
                 :channel-id "C_DEV"
                 :text "Here is the thread reply photo"
                 :ts "1688500005.0001"
                 :files (list
                         (list (cons 'name "beach_photo.png")
                               (cons 'url_private "https://files.slack.com/files/beach_photo.png"))))))
    (puthash "1688500000.0001" (list reply) taut-threads))

  (let ((index (taut-cache-browser--build-metadata-index)))
    (should (hash-table-p index))
    
    ;; Verify mapping for Alice's attachment
    (let* ((alice-url "https://files.slack.com/files/annual_report.pdf")
           (alice-hash (md5 alice-url))
           (alice-meta (gethash alice-hash index)))
      (should alice-meta)
      (should (equal (plist-get alice-meta :original-name) "annual_report.pdf"))
      (should (equal (plist-get alice-meta :sender-name) "Alice Smith")) ; uses real name from mock fixtures
      (should (equal (plist-get alice-meta :channel-name) "#development")))

    ;; Verify mapping for Bob's thread attachment
    (let* ((bob-url "https://files.slack.com/files/beach_photo.png")
           (bob-hash (md5 bob-url))
           (bob-meta (gethash bob-hash index)))
      (should bob-meta)
      (should (equal (plist-get bob-meta :original-name) "beach_photo.png"))
      (should (equal (plist-get bob-meta :sender-name) "Bob Jones")) ; uses real name
      (should (equal (plist-get bob-meta :channel-name) "#development")))))

(ert-deftest taut-cache-browser-build-metadata-index-sqlite-test ()
  "Test that `taut-cache-browser--build-metadata-index' loads metadata from SQLite offline."
  (skip-unless (taut-cache--available-p))
  
  (let* ((temp-db-file (make-temp-file "taut-cache-test-db-"))
         (taut-cache-db-path temp-db-file)
         (taut-cache--db nil))
    (unwind-protect
        (progn
          ;; Populate the DB with test structures
          (let ((user (make-taut-user :id "U_ALICE" :username "alice" :real-name "Alice Smith" :presence 'online :is-me t))
                (chan (make-taut-channel :id "C_GENERAL" :name "general" :type 'public :unread-count 0))
                (msg (make-taut-message
                      :id "msg-db"
                      :channel-id "C_GENERAL"
                      :user-id "U_ALICE"
                      :text "DB msg text"
                      :ts "1688500010.0001"
                      :files (list
                              (list (cons 'name "db_file.png")
                                    (cons 'url_private_download "https://files.slack.com/files/db_file.png"))))))
            (taut-cache-save-user user)
            (taut-cache-save-channel chan)
            (taut-cache-save-message msg))
          
          ;; Ensure in-memory structures are completely clear (simulating fresh startup)
          (setq taut-messages (make-hash-table :test 'equal))
          (setq taut-threads (make-hash-table :test 'equal))
          
          ;; Build index
          (let* ((index (taut-cache-browser--build-metadata-index))
                 (url "https://files.slack.com/files/db_file.png")
                 (hash (md5 url))
                 (meta (gethash hash index)))
            (should (hash-table-p index))
            (should meta)
            (should (equal (plist-get meta :original-name) "db_file.png"))
            (should (equal (plist-get meta :sender-name) "Alice Smith"))
            (should (equal (plist-get meta :channel-name) "#general"))
            (should (equal (plist-get meta :message-ts) "1688500010.0001"))
            (should (equal (plist-get meta :message-text) "DB msg text"))))
      
      ;; Cleanup connection and temporary file
      (when (and taut-cache--db (sqlitep taut-cache--db))
        (sqlite-close taut-cache--db))
      (when (file-exists-p temp-db-file)
        (delete-file temp-db-file)))))

(ert-deftest taut-cache-browser-refresh-and-sort-test ()
  "Test the directory scanning, display names formatting, and custom sorting logic."
  (taut-initialize-mock-data)
  
  ;; Create a temporary directory for local cache
  (let ((temp-cache-dir (make-temp-file "taut-test-media-cache-" t)))
    (unwind-protect
        (let ((taut-media-cache-dir temp-cache-dir)
              ;; Mock metadata index
              (taut-cache-browser--metadata-index (make-hash-table :test 'equal))
              (alice-hash "alice_hash_123")
              (bob-hash "bob_hash_456"))
          
          ;; Seed our mock index
          (puthash alice-hash
                   (list :original-name "proposal.docx"
                         :sender-name "Alice Smith"
                         :channel-name "#ideas")
                   taut-cache-browser--metadata-index)
          (puthash bob-hash
                   (list :original-name "invoice.xls"
                         :sender-name "Bob Jones"
                         :channel-name "#billing")
                   taut-cache-browser--metadata-index)

          ;; Create dummy files in temporary cache directory
          (let ((file-alice (expand-file-name (concat alice-hash ".docx") temp-cache-dir))
                (file-bob (expand-file-name (concat bob-hash ".xls") temp-cache-dir))
                (file-avatar (expand-file-name "avatar-U_ALICE.png" temp-cache-dir))
                (file-unknown (expand-file-name "xyz_random.txt" temp-cache-dir)))
            
            ;; Write files with different contents to test size and modified-time sorting
            (write-region "small" nil file-alice nil 'silent)
            (write-region "much larger contents for testing size sorting" nil file-bob nil 'silent)
            (write-region "avatar img" nil file-avatar nil 'silent)
            (write-region "unknown" nil file-unknown nil 'silent)

            ;; Set custom modification time on bob's file to ensure chronological differences
            ;; Alice file is modified recently, Bob file modified in the past
            (set-file-times file-bob '(22000 0)) ; long ago

            ;; Run inside browser buffer
            (with-temp-buffer
              (taut-cache-browser-mode)
              
              ;; Bind the internal index builder so we can use our seeded one
              (cl-letf (((symbol-function 'taut-cache-browser--build-metadata-index)
                         (lambda () taut-cache-browser--metadata-index)))
                (taut-cache-browser-refresh))
              
              ;; Verify correct number of items in tabulated list (4 files)
              (should (= (length tabulated-list-entries) 4))

              ;; Locate Alice's entry
              (let ((alice-entry (cl-find file-alice tabulated-list-entries :key #'car :test #'equal)))
                (should alice-entry)
                (let ((vec (cadr alice-entry)))
                  (should (equal (aref vec 0) "proposal.docx"))
                  (should (equal (aref vec 1) "Alice Smith"))
                  (should (equal (aref vec 2) "#ideas"))))

              ;; Locate Avatar entry
              (let ((avatar-entry (cl-find file-avatar tabulated-list-entries :key #'car :test #'equal)))
                (should avatar-entry)
                (let ((vec (cadr avatar-entry)))
                  (should (equal (aref vec 0) "avatar-U_ALICE.png"))
                  (should (equal (aref vec 1) "[User Avatar]"))
                  (should (equal (aref vec 2) "-"))))

              ;; Locate Unknown entry
              (let ((unknown-entry (cl-find file-unknown tabulated-list-entries :key #'car :test #'equal)))
                (should unknown-entry)
                (let ((vec (cadr unknown-entry)))
                  (should (equal (aref vec 0) "xyz_random.txt"))
                  (should (equal (aref vec 1) "[System / Asset]"))
                  (should (equal (aref vec 2) "-"))))

              ;; Test Custom Size Sorting
              ;; Small file A vs Big file B
              (let ((entry-alice (cl-find file-alice tabulated-list-entries :key #'car :test #'equal))
                    (entry-bob (cl-find file-bob tabulated-list-entries :key #'car :test #'equal)))
                (should (taut-cache-browser--sort-by-size entry-alice entry-bob))
                (should-not (taut-cache-browser--sort-by-size entry-bob entry-alice)))

              ;; Test Custom Modification Date Sorting
              ;; Bob file modified long ago, Alice modified now
              (let ((entry-alice (cl-find file-alice tabulated-list-entries :key #'car :test #'equal))
                    (entry-bob (cl-find file-bob tabulated-list-entries :key #'car :test #'equal)))
                (should (taut-cache-browser--sort-by-date entry-bob entry-alice))
                (should-not (taut-cache-browser--sort-by-date entry-alice entry-bob))))))
      
      ;; Cleanup temp cache directory
      (delete-directory temp-cache-dir t))))

(ert-deftest taut-cache-browser-interactive-actions-test ()
  "Test opening, deletion, and clear-all actions in `taut-cache-browser-mode`."
  (taut-initialize-mock-data)
  (let ((temp-cache-dir (make-temp-file "taut-test-media-cache-" t)))
    (unwind-protect
        (let ((taut-media-cache-dir temp-cache-dir)
              (file-dummy (expand-file-name "dummy.txt" temp-cache-dir)))
          (write-region "sample content" nil file-dummy nil 'silent)
          
          (with-temp-buffer
            (taut-cache-browser-mode)
            (setq tabulated-list-entries (list (list file-dummy (vector "dummy.txt" "Me" "-" "14 B" "mtime"))))
            (tabulated-list-print)

            ;; Test Open command (find-file-noselect)
            (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf) (should (bufferp buf)))))
              (goto-char (point-min))
              (taut-cache-browser-open-at-point))

            ;; Test Delete command with mock answer yes
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
              (should (file-exists-p file-dummy))
              (goto-char (point-min))
              (taut-cache-browser-delete-at-point)
              (should-not (file-exists-p file-dummy))))

          ;; Recreate a file for clear-all test
          (write-region "sample content" nil file-dummy nil 'silent)
          (should (file-exists-p file-dummy))

          ;; Test Clear-All command with mock answer yes
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
            (taut-cache-browser-clear-all)
            (should-not (file-exists-p file-dummy))))
      
      (when (file-directory-p temp-cache-dir)
        (delete-directory temp-cache-dir t)))))

(provide 'test-taut-cache-browser)
;;; test-taut-cache-browser.el ends here

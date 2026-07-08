;;; taut-cache-browser.el --- Media Cache Introspection for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us
;; Keywords: comm, slack, files

;; This file implements a premium, tabulated media cache browser for Taut,
;; providing full user visibility and maintenance tools into cached assets
;; (avatars, custom emojis, downloaded text attachments, and image previews).

;;; Code:

(require 'tabulated-list)
(require 'taut-model)
(require 'taut-message)
(require 'taut-cache)
(require 'json)

(defvar taut-cache-browser--metadata-index nil
  "Internal reverse-lookup mapping file hashes to message metadata.")

(defgroup taut-cache-browser nil
  "Media cache browser customization options."
  :group 'taut)

(defun taut-cache-browser--build-metadata-index ()
  "Build a reverse lookup hash table from file hash to message metadata.
Keys are MD5 file hashes (without extension) and values are property lists with:
  (:original-name name
   :sender-name real-or-username
   :channel-name name
   :message-ts ts
   :message-text text)"
  (let ((index (make-hash-table :test 'equal)))
    ;; 0. Query persistent SQLite cache if available
    (let ((db (and (fboundp 'taut-cache--get-db) (taut-cache--get-db))))
      (when db
        (ignore-errors
          (let ((user-names (make-hash-table :test 'equal))
                (chan-names (make-hash-table :test 'equal)))
            ;; Cache user ID to name mappings
            (dolist (row (sqlite-select db "SELECT id, username, real_name FROM users"))
              (let* ((id (nth 0 row))
                     (uname (nth 1 row))
                     (rname (nth 2 row))
                     (display (or rname uname id)))
                (puthash id display user-names)))
            ;; Cache channel ID to name mappings
            (dolist (row (sqlite-select db "SELECT id, name FROM channels"))
              (let ((id (nth 0 row))
                    (name (nth 1 row)))
                (puthash id name chan-names)))
            ;; Retrieve messages with files
            (dolist (row (sqlite-select db "SELECT channel_id, user_id, text, ts, files_json FROM messages WHERE files_json IS NOT NULL AND files_json != ''"))
              (let* ((chan-id (nth 0 row))
                     (user-id (nth 1 row))
                     (text (nth 2 row))
                     (ts (nth 3 row))
                     (files-json (nth 4 row))
                     (chan-name (or (gethash chan-id chan-names) chan-id))
                     (sender-name (or (gethash user-id user-names) user-id))
                     (files (ignore-errors
                              (let ((json-array-type 'list)
                                    (json-object-type 'alist)
                                    (json-key-type 'symbol))
                                (json-read-from-string files-json)))))
                (dolist (file files)
                  (let* ((url (or (cdr (assoc 'url_private_download file))
                                  (cdr (assoc 'url_private file))))
                         (filename (cdr (assoc 'name file))))
                    (when url
                      (let ((hash (md5 url)))
                        (puthash hash
                                 (list :original-name filename
                                       :sender-name sender-name
                                       :channel-name (format "#%s" chan-name)
                                       :message-ts ts
                                       :message-text text)
                                 index)))))))))))

    ;; 1. Scan in-memory channel messages
    (when (boundp 'taut-messages)
      (maphash
       (lambda (chan-id msgs)
         (let* ((chan (taut-model-get-channel chan-id))
                (chan-name (if chan (taut-channel-name chan) chan-id)))
           (dolist (msg msgs)
             (let* ((user-id (taut-message-user-id msg))
                    (user (taut-model-get-user user-id))
                    (sender-name (if user
                                     (or (taut-user-real-name user) (taut-user-username user) user-id)
                                   user-id))
                    (files (taut-message-files msg)))
               (dolist (file files)
                 (let* ((url (or (cdr (assoc 'url_private_download file))
                                 (cdr (assoc 'url_private file))))
                        (filename (cdr (assoc 'name file))))
                   (when url
                     (let ((hash (md5 url)))
                       (puthash hash
                                (list :original-name filename
                                      :sender-name sender-name
                                      :channel-name (format "#%s" chan-name)
                                      :message-ts (taut-message-ts msg)
                                      :message-text (taut-message-text msg))
                                index)))))))))
       taut-messages))
    
    ;; 2. Scan in-memory thread replies
    (when (boundp 'taut-threads)
      (maphash
       (lambda (_thread-ts replies)
         (dolist (reply replies)
           (let* ((chan-id (taut-message-channel-id reply))
                  (chan (and chan-id (taut-model-get-channel chan-id)))
                  (chan-name (if chan (taut-channel-name chan) (or chan-id "thread")))
                  (user-id (taut-message-user-id reply))
                  (user (taut-model-get-user user-id))
                  (sender-name (if user
                                   (or (taut-user-real-name user) (taut-user-username user) user-id)
                                 user-id))
                  (files (taut-message-files reply)))
             (dolist (file files)
               (let* ((url (or (cdr (assoc 'url_private_download file))
                               (cdr (assoc 'url_private file))))
                      (filename (cdr (assoc 'name file))))
                 (when url
                   (let ((hash (md5 url)))
                     (puthash hash
                              (list :original-name filename
                                    :sender-name sender-name
                                    :channel-name (format "#%s" chan-name)
                                    :message-ts (taut-message-ts reply)
                                    :message-text (taut-message-text reply))
                              index))))))))
       taut-threads))
    index))

(defun taut-cache-browser--sort-by-size (a b)
  "Sort cache entries A and B by file size."
  (let* ((attrs-a (file-attributes (car a)))
         (attrs-b (file-attributes (car b)))
         (size-a (if attrs-a (file-attribute-size attrs-a) 0))
         (size-b (if attrs-b (file-attribute-size attrs-b) 0)))
    (< size-a size-b)))

(defun taut-cache-browser--sort-by-date (a b)
  "Sort cache entries A and B by modification date."
  (let* ((attrs-a (file-attributes (car a)))
         (attrs-b (file-attributes (car b)))
         (time-a (if attrs-a (file-attribute-modification-time attrs-a) nil))
         (time-b (if attrs-b (file-attribute-modification-time attrs-b) nil)))
    (cond
     ((and time-a time-b) (time-less-p time-a time-b))
     (time-a t)
     (t nil))))

(defvar taut-cache-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'taut-cache-browser-open-at-point)
    (define-key map (kbd "f") #'taut-cache-browser-open-at-point)
    (define-key map (kbd "d") #'taut-cache-browser-delete-at-point)
    (define-key map (kbd "g") #'taut-cache-browser-refresh)
    (define-key map (kbd "C") #'taut-cache-browser-clear-all)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `taut-cache-browser-mode'.")

(define-derived-mode taut-cache-browser-mode tabulated-list-mode "Taut Cache"
  "Major mode for browsing and managing Taut's local media cache."
  (setq tabulated-list-format
        [("File Name / Attachment" 40 t)
         ("Sender" 22 t)
         ("Channel" 15 t)
         ("Size" 12 taut-cache-browser--sort-by-size)
         ("Last Modified" 20 taut-cache-browser--sort-by-date)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun taut-cache-browser-refresh ()
  "Refresh the list of files in the media cache."
  (interactive)
  (setq taut-cache-browser--metadata-index (taut-cache-browser--build-metadata-index))
  (setq tabulated-list-entries nil)
  (when (and taut-media-cache-dir (file-directory-p taut-media-cache-dir))
    (let ((files (directory-files taut-media-cache-dir t "^[^.]" t)))
      (dolist (file-path files)
        (let* ((attrs (file-attributes file-path))
               (size (if attrs (file-attribute-size attrs) 0))
               (mtime (if attrs (file-attribute-modification-time attrs) nil))
               (size-str (file-size-human-readable size))
               (mtime-str (if mtime (format-time-string "%Y-%m-%d %H:%M:%S" mtime) "-"))
               (base-name (file-name-base file-path))
               (meta (gethash base-name taut-cache-browser--metadata-index))
               (display-name (if meta (plist-get meta :original-name) (file-name-nondirectory file-path)))
               (sender (cond
                        (meta (plist-get meta :sender-name))
                        ((string-match-p "^avatar-" base-name) "[User Avatar]")
                        ((string-match-p "^emoji-" base-name) "[Emoji]")
                        (t "[System / Asset]")))
               (channel (if meta (plist-get meta :channel-name) "-")))
          (push (list file-path
                      (vector display-name sender channel size-str mtime-str))
                tabulated-list-entries)))))
  (tabulated-list-print t))

(defun taut-cache-browser-open-at-point ()
  "Open the cached file under point inside Emacs."
  (interactive)
  (let ((file-path (tabulated-list-get-id)))
    (if (and file-path (file-exists-p file-path))
        (let ((buf (find-file-noselect file-path)))
          (pop-to-buffer buf))
      (message "Taut: Selected file no longer exists."))))

(defun taut-cache-browser-delete-at-point ()
  "Delete the cached file under point."
  (interactive)
  (let ((file-path (tabulated-list-get-id)))
    (if (and file-path (file-exists-p file-path))
        (if (yes-or-no-p (format "Delete cached file %s? " (file-name-nondirectory file-path)))
            (progn
              (delete-file file-path)
              (tabulated-list-delete-entry)
              (message "Taut: Deleted file %s" (file-name-nondirectory file-path)))
          (message "Taut: Deletion cancelled."))
      (message "Taut: File no longer exists."))))

(defun taut-cache-browser-clear-all ()
  "Wipe the entire Taut media cache clean."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to clear the Taut media cache? ")
    (delete-directory taut-media-cache-dir t t)
    (taut-cache-browser-refresh)
    (message "Taut: Media cache cleared successfully.")))

;;;###autoload
(defun taut-cache-browser ()
  "Open the interactive Taut media cache browser buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Taut Media Cache*")))
    (with-current-buffer buf
      (taut-cache-browser-mode)
      (taut-cache-browser-refresh))
    (pop-to-buffer buf)))

(provide 'taut-cache-browser)
;;; taut-cache-browser.el ends here

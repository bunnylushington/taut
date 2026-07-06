;;; taut-search.el --- Robust Search Feature for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us
;; Keywords: comm, slack

;;; Commentary:
;; This file implements a robust, context-aware search feature for the Taut
;; Slack client. It supports hybrid local/remote search, interactive result
;; buffers, relative "vague" timestamps, and visual message flashing on jump.

;;; Code:

(require 'cl-lib)
(require 'taut-model)
(require 'taut-cache)
(require 'taut-api)
(require 'taut-message)

(declare-function taut-dispatch "taut-transient")

;;;; Faces

(defface taut-search-header-query
  '((t :weight bold :foreground "#36c5f0"))
  "Face for highlighting the query string in search headers."
  :group 'taut-faces)

(defface taut-search-header-scope
  '((t :weight bold :foreground "#e01e5a"))
  "Face for highlighting the search scope."
  :group 'taut-faces)

(defface taut-search-time
  '((t :foreground "gray50" :slant italic))
  "Face for search results relative timestamps."
  :group 'taut-faces)

;;;; Buffer-Local Variables

(defvar-local taut-search-current-query nil
  "The current search query.")

(defvar-local taut-search-current-channel-id nil
  "The channel-id filter (if any) used for the search.")

(defvar-local taut-search-current-user-id nil
  "The user-id filter (if any) used for the search.")

;;;; Major Mode Definition

(defvar taut-search-mode-map (make-sparse-keymap)
  "Keymap for `taut-search-mode`.")

(define-key taut-search-mode-map (kbd "RET") #'taut-search-activate)
(define-key taut-search-mode-map (kbd "<mouse-1>") #'taut-search-mouse-activate)
(define-key taut-search-mode-map (kbd "g") #'taut-search-refresh)
(define-key taut-search-mode-map (kbd "/") #'taut-search-quick)
(define-key taut-search-mode-map (kbd "p") #'taut-search-advanced)
(define-key taut-search-mode-map (kbd "q") #'quit-window)
(define-key taut-search-mode-map (kbd "?") #'taut-dispatch)

(define-derived-mode taut-search-mode special-mode "Taut Search"
  "Major mode for Taut search results.

\\{taut-search-mode-map}"
  (setq buffer-read-only t
        truncate-lines t)
  (hl-line-mode 1))

;;;; Vague Relative Time Segmenter

(defun taut-search--vague-time (ts-str)
  "Return a vague human-readable time string for TS-STR."
  (let* ((msg-time (floor (string-to-number ts-str)))
         (now-time (floor (float-time)))
         (diff-seconds (- now-time msg-time))
         (decoded-msg (decode-time (seconds-to-time msg-time)))
         (decoded-now (decode-time (seconds-to-time now-time)))
         (msg-day (decoded-time-day decoded-msg))
         (msg-month (decoded-time-month decoded-msg))
         (msg-year (decoded-time-year decoded-msg))
         (now-day (decoded-time-day decoded-now))
         (now-month (decoded-time-month decoded-now))
         (now-year (decoded-time-year decoded-now)))
    (cond
     ((< diff-seconds 0) "Future")
     ;; Today
     ((and (= msg-year now-year)
           (= msg-month now-month)
           (= msg-day now-day))
      "Today")
     ;; Yesterday
     ((and (= msg-year now-year)
           (or (and (= msg-month now-month) (= msg-day (- now-day 1)))
               ;; Handle month wrap or simplify with time diff
               (and (< diff-seconds 172800) (not (= msg-day now-day)))))
      "Yesterday")
     ;; This Week (within 7 days)
     ((< diff-seconds 604800) "This Week")
     ;; Last Week (within 14 days)
     ((< diff-seconds 1209600) "Last Week")
     ;; Earlier this Year
     ((= msg-year now-year)
      (format "Earlier this Year, %s %d"
              (nth (1- msg-month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
              msg-day))
     ;; Earlier Years
     (t (format "in %d" msg-year)))))

;;;; Interactive Entrypoints

;;;###autoload
(defun taut-search-quick (query)
  "Perform a quick search for QUERY.
If in a channel/DM buffer, scope the search to that channel.
Otherwise, perform a global workspace-wide search."
  (interactive
   (let* ((active-channel-id (cond
                              ((and (boundp 'taut-current-channel-id) taut-current-channel-id)
                               taut-current-channel-id)
                              ((and (derived-mode-p 'taut-search-mode) (boundp 'taut-search-current-channel-id))
                               taut-search-current-channel-id)
                              (t nil)))
          (prompt (if active-channel-id
                      (let* ((chan (taut-model-get-channel active-channel-id))
                             (name (if chan (taut-channel-name chan) active-channel-id)))
                        (format "Search Taut (#%s): " name))
                    "Search Taut (workspace): ")))
     (list (read-string prompt))))
  (let ((chan-id (cond
                  ((and (boundp 'taut-current-channel-id) taut-current-channel-id)
                   taut-current-channel-id)
                  ((and (derived-mode-p 'taut-search-mode) (boundp 'taut-search-current-channel-id))
                   taut-search-current-channel-id)
                  (t nil))))
    (taut-search--execute query chan-id nil)))

;;;###autoload
(defun taut-search-advanced ()
  "Prompt for detailed search parameters and perform search."
  (interactive)
  (let* ((query (read-string "Search query: "))
         (channels (taut-model-get-channels-list))
         (channel-names (mapcar #'taut-channel-name channels))
         (chan-choice (completing-read "Filter by Channel (optional, TAB to autocomplete): " (cons "" channel-names) nil t))
         (chan-id (when (not (string= chan-choice ""))
                    (let ((c (cl-find-if (lambda (ch) (string= (taut-channel-name ch) chan-choice)) channels)))
                      (and c (taut-channel-id c)))))
         (users (taut-model-get-users-list))
         (user-names (mapcar #'taut-user-username users))
         (user-choice (completing-read "Filter by User (optional, TAB to autocomplete): " (cons "" user-names) nil t))
         (usr-id (when (not (string= user-choice ""))
                   (let ((u (cl-find-if (lambda (usr) (string= (taut-user-username usr) user-choice)) users)))
                     (and u (taut-user-id u))))))
    (taut-search--execute query chan-id usr-id)))

;;;; Rendering & Merging Engine

(defun taut-search-refresh ()
  "Re-run the current search query with existing scopes."
  (interactive)
  (unless taut-search-current-query
    (error "Taut Search: No active query to refresh"))
  (taut-search--execute taut-search-current-query
                        taut-search-current-channel-id
                        taut-search-current-user-id))

(defun taut-search--execute (query &optional channel-id user-id)
  "Run the search and render results in *Taut Search* buffer."
  (let* ((buf (get-buffer-create "*Taut Search*"))
         (inhibit-read-only t))
    (with-current-buffer buf
      (taut-search-mode)
      (setq taut-search-current-query query
            taut-search-current-channel-id channel-id
            taut-search-current-user-id user-id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Render Header
        (insert (propertize "================================================================================\n" 'face 'font-lock-comment-face))
        (insert (propertize "  🔍 TAUT SEARCH RESULTS\n" 'face '(:weight bold :height 1.2 :foreground "#e01e5a")))
        (insert (propertize "================================================================================\n" 'face 'font-lock-comment-face))
        (insert "  Query: " (propertize query 'face 'taut-search-header-query) "\n")
        (insert "  Scope: " 
                (propertize (if channel-id
                                (let ((c (taut-model-get-channel channel-id)))
                                  (format "#%s" (if c (taut-channel-name c) channel-id)))
                              "Workspace (Global)")
                            'face 'taut-search-header-scope) "\n")
        (insert (propertize "--------------------------------------------------------------------------------\n\n" 'face 'font-lock-comment-face))

        ;; Fetch and Merge Results
        (let* ((local-results (taut-cache-search-messages query channel-id user-id))
               (api-results (condition-case _err
                                (taut-api-search-messages query "timestamp" "desc")
                              (error nil)))
               remote-msgs)
          (when api-results
            (let* ((messages-obj (cdr (assoc 'messages api-results)))
                   (matches (cdr (assoc 'matches messages-obj))))
              (dolist (m matches)
                (let* ((channel-obj (cdr (assoc 'channel m)))
                       (c-id (or (cdr (assoc 'id channel-obj)) (cdr (assoc 'iid m))))
                       (m-user-id (cdr (assoc 'user m)))
                       (m-text (cdr (assoc 'text m)))
                       (m-ts (cdr (assoc 'ts m)))
                       (m-id (format "%s-%s" c-id m-ts)))
                  (when (or (not channel-id) (equal c-id channel-id))
                    (push (make-taut-message
                           :id m-id
                           :channel-id c-id
                           :user-id m-user-id
                           :text m-text
                           :ts m-ts)
                          remote-msgs))))))
          (setq remote-msgs (nreverse remote-msgs))

          ;; Merge and deduplicate
          (let* ((all-msgs (append local-results remote-msgs))
                 (seen (make-hash-table :test 'equal))
                 deduped-msgs)
            (dolist (msg all-msgs)
              (let ((key (cons (taut-message-channel-id msg) (taut-message-ts msg))))
                (unless (gethash key seen)
                  (puthash key t seen)
                  (push msg deduped-msgs))))
            (setq deduped-msgs (nreverse deduped-msgs))

            ;; Render matches
            (if (null deduped-msgs)
                (insert "  No matching messages found.\n\n")
              (insert (format "  Found %d matches:\n\n" (length deduped-msgs)))
              (dolist (msg deduped-msgs)
                (let* ((start-pos (point))
                       (c-id (taut-message-channel-id msg))
                       (chan (taut-model-get-channel c-id))
                       (chan-name (if chan (taut-channel-name chan) c-id))
                       (user (taut-model-get-user (taut-message-user-id msg)))
                       (username (if user (or (taut-user-username user) "unknown") "unknown"))
                       (time-segment (taut-search--vague-time (taut-message-ts msg))))
                  ;; Render entry header: 🌑 #channel-name | @username  [Time-segment]
                  (insert "  🌑 " 
                          (propertize (format "#%s" chan-name) 'face 'taut-message-username)
                          " | "
                          (propertize (format "@%s" username) 'face 'taut-message-me)
                          "  "
                          (propertize (format "[%s]" time-segment) 'face 'taut-search-time)
                          "\n")
                  ;; Render body line with indentation
                  (insert "  \"" (taut-message-text msg) "\"\n")
                  (insert "  " (propertize "[RET: Jump to Message]" 'face 'link) "\n\n")
                  ;; Add properties for clicking/ret jump
                  (add-text-properties start-pos (point)
                                       (list 'taut-message-ts (taut-message-ts msg)
                                             'taut-channel-id c-id
                                             'mouse-face 'highlight
                                             'help-echo "Click or press RET to jump to this message")))))
            
            ;; Footer instructions
            (insert (propertize "================================================================================\n" 'face 'font-lock-comment-face))
            (insert "  [/] New Quick Search  •  [p] Advanced Filter Form  •  [q] Quit/Bury Window\n")))))
    (pop-to-buffer buf)))

;;;; Navigation & Jump Hooks

(defun taut-search--flash-message (pos)
  "Temporarily highlight the message starting at POS."
  (save-excursion
    (goto-char pos)
    (let* ((start (line-beginning-position))
           (ts (get-text-property pos 'taut-message-ts))
           (end (save-excursion
                  (let ((p pos))
                    (while (and p (< p (point-max)) (equal (get-text-property p 'taut-message-ts) ts))
                      (setq p (next-single-property-change p 'taut-message-ts)))
                    (or p (point-max)))))
           (overlay (make-overlay start end)))
      (overlay-put overlay 'face 'highlight)
      (run-with-timer 0.5 nil (lambda (ov) (delete-overlay ov)) overlay))))

(defun taut-search-activate ()
  "Jump to the message at point in the search results buffer."
  (interactive)
  (let ((ts (get-text-property (point) 'taut-message-ts))
        (chan-id (get-text-property (point) 'taut-channel-id)))
    (if (and ts chan-id)
        (progn
          ;; Open channel buffer
          (taut-message-open chan-id)
          ;; Go to message in the selected conversation buffer
          (let* ((chan (taut-model-get-channel chan-id))
                 (chan-name (if chan (taut-channel-name chan) chan-id))
                 (chan-type (if chan (taut-channel-type chan) 'public))
                 (buf-name (if (eq chan-type 'dm)
                               (format "*Taut - @%s*" chan-name)
                             (format "*Taut - #%s*" chan-name)))
                 (buf (get-buffer buf-name)))
            (if buf
                (with-current-buffer buf
                  (let ((found (taut-message-goto-ts ts)))
                    (if found
                        (taut-search--flash-message (point))
                      (message "Taut: Message found in search but not currently loaded in history."))))
              (message "Taut: Failed to locate conversation buffer %s" buf-name))))
      (message "Taut: No message at point."))))

(defun taut-search-mouse-activate (event)
  "Jump to the message clicked with mouse in the search results buffer."
  (interactive "e")
  (posn-set-point (event-end event))
  (taut-search-activate))

(provide 'taut-search)
;;; taut-search.el ends here

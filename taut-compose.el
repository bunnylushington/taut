;;; taut-compose.el --- Dedicated Message Composer for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us
;; Keywords: comm, slack

;;; Commentary:
;; This file implements a high-fidelity, buffer-based editor for composing
;; and replying to Slack messages/threads in Taut.

;;; Code:

(require 'cl-lib)
(require 'taut-model)
(require 'taut-api)
(require 'taut-message)

(declare-function taut-thread-refresh "taut-thread")
(declare-function taut-compose-dispatch "taut-transient")
(declare-function taut-emoticon-translate-string "taut-message")

(defvar taut-current-thread-ts)
(defvar taut-current-user-id)
(defvar taut-emoticon-alist)

;;;; Buffer-Local Variables

(defvar-local taut-compose-channel-id nil
  "The target channel ID for this draft.")

(defvar-local taut-compose-thread-ts nil
  "The target thread timestamp for this draft, or nil for main channel.")

(defvar-local taut-compose-edit-ts nil
  "The timestamp of the message being edited, or nil if posting new.")

;;;; Keymap & Major Mode

(defvar taut-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'taut-compose-send)
    (define-key map (kbd "C-c C-k") #'taut-compose-abort)
    (define-key map (kbd "C-c C-b") #'taut-compose-insert-code-block)
    (define-key map (kbd "C-c C-l") #'taut-compose-insert-link)
    (define-key map (kbd "C-c C-u") #'taut-compose-insert-user-mention)
    (define-key map (kbd "C-c @") #'taut-compose-insert-user-mention)
    (define-key map (kbd "?") #'taut-compose-dispatch)
    map)
  "Keymap for `taut-compose-mode`.")

(define-derived-mode taut-compose-mode text-mode "Taut-Compose"
  "Major mode for composing replies and messages in Taut.

\\{taut-compose-mode-map}"
  (setq-local header-line-format
              '(:eval (let* ((chan (taut-model-get-channel taut-compose-channel-id))
                             (name (if chan (or (taut-channel-name chan) "unknown") "unknown"))
                             (is-dm (and chan (eq (taut-channel-type chan) 'dm)))
                             (prefix (if is-dm "@" "#"))
                             (is-thread taut-compose-thread-ts)
                             (is-edit taut-compose-edit-ts))
                        (format " %s %s %s in %s%s  [C-c C-c to send, C-c C-k to abort, ? for helper]"
                                (if is-edit "✏️" (if is-thread "🧵" "💬"))
                                (if is-edit "Editing" "Composing")
                                (if is-edit "message" (if is-thread "thread reply" "message"))
                                prefix
                                name))))
  (setq word-wrap t)
  (visual-line-mode 1)
  (add-hook 'post-self-insert-hook #'taut-compose--post-self-insert nil t))

(defun taut-compose--post-self-insert ()
  "Translate emoticons to emojis as the user types in the compose buffer."
  (let ((pos (point)))
    (save-excursion
      (let ((found nil)
            (limit (max (point-min) (- pos 5))))
        (goto-char pos)
        ;; Check substrings of length 2 to 5 ending at point
        (cl-loop for len from 2 to 5
                 while (not found)
                 do (let ((start (- pos len)))
                      (when (>= start limit)
                        (let* ((substring (buffer-substring-no-properties start pos))
                               (match (assoc substring taut-emoticon-alist)))
                          (when match
                            ;; Check boundary before the emoticon
                            (goto-char start)
                            (when (or (bobp)
                                      (let ((char-before (char-before)))
                                        (or (member char-before '(?\s ?\t ?\n ?\r))
                                            (not (or (and (>= char-before ?a) (<= char-before ?z))
                                                     (and (>= char-before ?A) (<= char-before ?Z))
                                                     (and (>= char-before ?0) (<= char-before ?9)))))))
                              (setq found (cdr match))
                              (delete-region start pos)
                              (insert found)))))))))))

;;;; Core Composer Operations

;;;###autoload
(defun taut-compose-open (channel-id &optional thread-ts quote-msg edit-ts edit-text)
  "Open the `*Taut Compose*` buffer for writing a message.
CHANNEL-ID specifies the channel.  Optional THREAD-TS is for replies.
QUOTE-MSG can be a message struct to quote.
EDIT-TS and EDIT-TEXT are used for editing an existing message."
  (let* ((buf-name "*Taut Compose*")
         (buf (get-buffer-create buf-name))
         (functions (if (fboundp 'display-buffer-below-selected)
                        '(display-buffer-below-selected display-buffer-at-bottom)
                      '(display-buffer-at-bottom display-buffer-pop-up-window)))
         (action (list functions '((window-height . 8)))))
    
    (with-current-buffer buf
      (unless (eq major-mode 'taut-compose-mode)
        (taut-compose-mode))
      (setq taut-compose-channel-id channel-id
            taut-compose-thread-ts thread-ts
            taut-compose-edit-ts edit-ts)
      (erase-buffer)
      
      (cond
       (edit-ts
        (when edit-text
          (insert edit-text)))
       (quote-msg
        (let* ((user (taut-model-get-user (taut-message-user-id quote-msg)))
               (username (if user (or (taut-user-username user) "unknown") "unknown"))
               (text (or (taut-message-text quote-msg) ""))
               (quoted-lines (mapcar (lambda (line) (concat "> " line))
                                     (split-string text "\n"))))
          (insert (format "> *@%s wrote:*\n" username))
          (insert (mapconcat #'identity quoted-lines "\n") "\n\n")))))
    
    ;; Place the compose buffer in a window at the bottom of the frame
    (pop-to-buffer buf action)
    (goto-char (point-max))))

;;;###autoload
(defun taut-compose-send ()
  "Send the composed message to Slack or update an existing message."
  (interactive)
  ;; Translate any remaining emoticons (e.g. pasted or typed fast) before sending
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (translated-text (taut-emoticon-translate-string text))
         (chan-id taut-compose-channel-id)
         (thread-ts taut-compose-thread-ts)
         (edit-ts taut-compose-edit-ts))
    (if (string-blank-p translated-text)
        (message "Cannot send an empty message.")
      ;; Post or update the message!
      (if (and (boundp 'taut-bot-token) taut-bot-token)
          (if edit-ts
              (taut-api-update-message chan-id edit-ts translated-text)
            (taut-api-post-message chan-id translated-text thread-ts))
        ;; Fallback to offline/mock
        (if edit-ts
            (let ((m (taut-model-get-message-by-ts edit-ts)))
              (when m
                (setf (taut-message-text m) translated-text)
                (taut-model-trigger-update)))
          (let* ((ts (format "%d.0000" (time-convert nil 'integer)))
                 (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) translated-text)))
            (taut-model-add-message
             (make-taut-message
              :id (concat "msg_" ts)
              :channel-id chan-id
              :user-id taut-current-user-id
              :text translated-text
              :ts ts
              :thread-ts thread-ts
              :reply-count 0
              :is-unread nil
              :is-mention is-mention)))))
      ;; Refresh active buffers
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (or (eq major-mode 'taut-message-mode)
                    (eq major-mode 'taut-thread-mode))
            (if (eq major-mode 'taut-thread-mode)
                (taut-thread-refresh)
              (taut-message-refresh)))))
      ;; Close the compose window and buffer
      (taut-compose-abort))))

;;;###autoload
(defun taut-compose-abort ()
  "Abort composition, killing the window and buffer."
  (interactive)
  (let ((win (get-buffer-window "*Taut Compose*")))
    (when win
      (delete-window win))
    (kill-buffer "*Taut Compose*")))

;;;; Formatting Helpers

;;;###autoload
(defun taut-compose-insert-code-block (lang)
  "Insert a Slack code block for LANG."
  (interactive "sLanguage (e.g. python, elisp): ")
  (let ((start (point)))
    (insert "```" lang "\n\n```")
    (goto-char (+ start 3 (length lang) 1))))

;;;###autoload
(defun taut-compose-insert-link (url label)
  "Insert a Slack-formatted link with URL and LABEL."
  (interactive "sURL: \nsLabel: ")
  (insert (format "<%s|%s>" url label)))

;;;###autoload
(defun taut-compose-insert-user-mention ()
  "Insert a Slack user mention selected via completing-read.
Mentions are formatted as <@U_ID|username>."
  (interactive)
  (let ((candidates nil))
    (maphash (lambda (uid user)
               (let* ((username (taut-user-username user))
                      (real-name (taut-user-real-name user))
                      (display (if real-name
                                   (format "@%s (%s)" username real-name)
                                 (format "@%s" username))))
                 (push (cons display uid) candidates)))
             taut-users)
    (if (null candidates)
        (message "Taut: No users available to mention.")
      (let* ((sorted-candidates (sort candidates (lambda (a b) (string< (car a) (car b)))))
             (choice (completing-read "Mention User: " sorted-candidates nil t))
             (uid (cdr (assoc choice sorted-candidates)))
             (user (gethash uid taut-users))
             (username (and user (taut-user-username user))))
        (when uid
          (insert (format "<@%s|%s>" uid (or username uid))))))))

;;;; Interactive Dispatch Triggers (r / R)

(defun taut-message-under-point ()
  "Get the `taut-message` struct under the cursor, if any."
  (let ((msg-id (get-text-property (point) 'taut-message-id)))
    (when msg-id
      ;; Scan active channel messages
      (or (and taut-current-channel-id
               (cl-find msg-id (taut-model-get-messages taut-current-channel-id)
                        :key #'taut-message-id :test #'equal))
          ;; Scan active thread replies if we can determine the thread-ts
          (and (boundp 'taut-current-thread-ts) taut-current-thread-ts
               (cl-find msg-id (taut-model-get-thread-replies taut-current-thread-ts)
                        :key #'taut-message-id :test #'equal))
          ;; Scan all messages as a fallback
          (let (found)
            (maphash (lambda (_cid msgs)
                       (unless found
                         (setq found (cl-find msg-id msgs :key #'taut-message-id :test #'equal))))
                     taut-messages)
            found)
          ;; Scan all thread replies as a fallback
          (let (found)
            (maphash (lambda (_th-ts replies)
                       (unless found
                         (setq found (cl-find msg-id replies :key #'taut-message-id :test #'equal))))
                     taut-threads)
            found)))))

(defun taut-message-reply-normal ()
  "Start composing a reply to the current channel or message under point."
  (interactive)
  (taut-message-reply-impl nil))

(defun taut-message-reply-quote ()
  "Start composing a reply quoting the message under point."
  (interactive)
  (taut-message-reply-impl t))

(defun taut-message-reply-impl (quote-p)
  "Internal implementation of compose/reply.
If QUOTE-P is non-nil, quote the message under point."
  (let* ((msg (taut-message-under-point))
         (chan-id (or taut-current-channel-id
                      (and msg (taut-message-channel-id msg))
                      (and (eq major-mode 'taut-thread-mode)
                           ;; If in thread buffer, find the channel of the root message
                           (let (cid)
                             (maphash (lambda (c msgs)
                                        (when (cl-some (lambda (m) (equal (taut-message-ts m) taut-current-thread-ts)) msgs)
                                          (setq cid c)))
                                      taut-messages)
                             cid))))
         (thread-ts nil))
    (unless chan-id
      (error "Cannot determine active channel for composition"))
    
    (cond
     ;; 0. High priority: we are on an item with an explicit thread TS property (e.g. inline reply or thread button)
     ((get-text-property (point) 'taut-thread-ts)
      (setq thread-ts (get-text-property (point) 'taut-thread-ts)))
     
     ;; 1. We are in thread mode: reply goes to the active thread
     ((eq major-mode 'taut-thread-mode)
      (setq thread-ts taut-current-thread-ts))
     
     ;; 2. We are on a message that is already a thread reply
     ((and msg (taut-message-thread-ts msg) (not (equal (taut-message-thread-ts msg) (taut-message-ts msg))))
      (setq thread-ts (taut-message-thread-ts msg)))
     
     ;; 3. We are on a message that has thread replies (is a root of a thread)
     ((and msg (taut-message-reply-count msg) (> (taut-message-reply-count msg) 0))
      (setq thread-ts (taut-message-ts msg)))
     
     ;; 4. We are on a message with no thread replies: prompt the user!
     (msg
      (if (y-or-n-p "Start a new thread for this message? ")
          (setq thread-ts (taut-message-ts msg))
        (setq thread-ts nil)))
     
     ;; 5. No message under point (e.g. empty area): normal channel message
     (t
      (setq thread-ts nil)))

    ;; Open composer
    (taut-compose-open chan-id thread-ts (when quote-p msg))))

(provide 'taut-compose)
;;; taut-compose.el ends here

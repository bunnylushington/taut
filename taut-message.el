;;; taut-message.el --- Rich Conversation Buffer for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Google DeepMind

;; Author: Antigravity
;; Keywords: comm, slack

;;; Commentary:
;; This file implements the main conversation buffer for channels and DMs
;; in the Taut Slack client. It renders usernames, timestamps, thread-links,
;; reactions, and formats Slack markdown dynamically.

;;; Code:

(require 'taut-model)
(require 'taut-api)

(declare-function taut-dispatch "taut-transient")

;;;; Faces

(defface taut-message-username
  '((t :foreground "#4a154b" :weight bold))
  "Face for sender usernames."
  :group 'taut-faces)

(defface taut-message-me
  '((t :foreground "#611f69" :weight bold))
  "Face for the current user's username."
  :group 'taut-faces)

(defface taut-message-timestamp
  '((t :foreground "#8a8a8a" :height 0.8))
  "Face for message timestamps."
  :group 'taut-faces)

(defface taut-message-text
  '((t :inherit font-lock-variable-name-face :weight normal))
  "Face for standard message body text."
  :group 'taut-faces)

(defface taut-message-mention
  '((t :background "#fff3cd" :foreground "#856404" :weight bold :box (:line-width (1 . -1) :style flat-button)))
  "Face for @mentions in messages."
  :group 'taut-faces)

(defface taut-message-code
  '((t :inherit separator-line :background "#f4f4f4" :family "monospace" :height 0.9))
  "Face for inline markdown `code` blocks."
  :group 'taut-faces)

(defface taut-message-reaction
  '((t :background "#f8f9fa" :foreground "#495057" :box (:line-width (1 . -1) :color "#dee2e6" :style flat-button) :height 0.85))
  "Face for message reactions."
  :group 'taut-faces)

(defface taut-message-thread-link
  '((t :foreground "#1264a3" :weight bold :underline t :height 0.9))
  "Face for clickable thread reply markers."
  :group 'taut-faces)

(defface taut-message-active-thread
  '((t :background "#f1f3f4" :extend t))
  "Face for highlighting the parent message of the active thread."
  :group 'taut-faces)

;;;; Buffer-Local Variables

(defvar-local taut-current-channel-id nil
  "The channel-id represented by this conversation buffer.")

(defvar-local taut-expanded-threads nil
  "List of thread-ts currently expanded inline in this buffer.")

(defvar taut-message-thread-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'taut-message-button-open-thread)
    (define-key map (kbd "<mouse-1>") #'taut-message-button-open-thread-mouse)
    (define-key map (kbd "TAB") #'taut-message-toggle-thread-inline)
    map)
  "Keymap for thread links/buttons inside message buffers.")

(defun taut-message-button-open-thread ()
  "Open the thread at point."
  (interactive)
  (let ((ts (get-text-property (point) 'taut-thread-ts)))
    (if ts
        (if (fboundp 'taut-thread-open)
            (funcall 'taut-thread-open ts)
          (message "Thread view is not yet loaded."))
      (message "No thread metadata found at point."))))

(defun taut-message-button-open-thread-mouse (event)
  "Open the thread with mouse click EVENT."
  (interactive "e")
  (posn-set-point (event-end event))
  (taut-message-button-open-thread))

;;;; Major Mode Definition

(defvar taut-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'taut-message-send)
    (define-key map (kbd "t") #'taut-message-start-thread)
    (define-key map (kbd "RET") #'taut-message-start-thread)
    (define-key map (kbd "TAB") #'taut-message-toggle-thread-inline)
    (define-key map (kbd "a") #'taut-message-add-reaction)
    (define-key map (kbd "g") #'taut-message-refresh)
    (define-key map (kbd "q") #'taut-message-bury)
    (define-key map (kbd "?") #'taut-dispatch)
    map)
  "Keymap for `taut-message-mode`.")

(define-derived-mode taut-message-mode special-mode "Taut-Chat"
  "Major mode for a Taut Slack conversation buffer.

\\{taut-message-mode-map}"
  (setq buffer-read-only t
        word-wrap t
        wrap-prefix "           ") ; Align wrapped text under usernames nicely
  (visual-line-mode 1))

;;;; Rendering Engine

(defun taut-message-refresh ()
  "Redraw the current conversation buffer."
  (interactive)
  (when taut-current-channel-id
    (let ((inhibit-read-only t)
          (old-point (point))
          (at-end (eobp)))
      (erase-buffer)
      (taut-message--render-history taut-current-channel-id)
      (if at-end
          (goto-char (point-max))
        (goto-char (min old-point (point-max)))))))

(defun taut-message--render-history (chan-id)
  "Render message list for CHAN-ID."
  (let* ((chan (taut-model-get-channel chan-id))
         (msgs (taut-model-get-messages chan-id)))
    ;; Buffer title banner
    (insert (propertize (if (eq (taut-channel-type chan) 'dm)
                            (format "  👤 @%s" (taut-channel-name chan))
                          (format "  #  %s" (taut-channel-name chan)))
                        'face '(:weight bold :height 1.2))
            "\n"
            (propertize (or (taut-channel-topic chan) "(no topic set)")
                        'face 'font-lock-comment-face)
            "\n"
            (make-string (window-body-width) ?─)
            "\n\n")

    (if (null msgs)
        (insert "\n\n  No messages in this conversation yet. Send a message with `r`!\n")
      (dolist (msg msgs)
        (taut-message--render-message-line msg)))))

(defun taut-message--render-message-line (msg)
  "Render a single message line MSG."
  (let* ((msg-start (point))
         (user (taut-model-get-user (taut-message-user-id msg)))
         (is-me (equal (taut-user-id user) taut-current-user-id))
         (user-face (if is-me 'taut-message-me 'taut-message-username))
         (user-part (propertize (taut-user-username user) 'face user-face))
         (time-str (taut-message--format-ts (taut-message-ts msg)))
         (time-part (propertize time-str 'face 'taut-message-timestamp))
         (active-thread-ts (taut-active-thread-ts))
         (is-active-thread (and active-thread-ts (equal active-thread-ts (taut-message-ts msg)))))

    ;; Header line: Username  [12:34]
    (insert user-part "  " time-part "\n")
    
    ;; Body line: (formatted text body with left indentation)
    (insert "         ")
    (taut-message--insert-formatted-text (taut-message-text msg))
    (insert "\n")

    ;; Reactions display (if any)
    (when (taut-message-reactions msg)
      (insert "         ")
      (dolist (reaction (taut-message-reactions msg))
        (let ((emoji (car reaction))
              (reactors (cdr reaction)))
          (insert (propertize (format " %s %d " emoji (length reactors))
                              'face 'taut-message-reaction
                              'mouse-face 'highlight
                              'taut-reaction-emoji emoji
                              'taut-message-id (taut-message-id msg))
                  " ")))
      (insert "\n"))

    ;; Thread replies indicator
    (let ((reply-count (taut-message-reply-count msg))
          (ts (taut-message-ts msg)))
      (when (and reply-count (> reply-count 0))
        (let* ((expanded (member ts taut-expanded-threads))
               (icon (if expanded "▼" "▶"))
               (label (format "💬 %s %d %s " icon reply-count (if (= reply-count 1) "reply" "replies"))))
          (insert "         "
                  (propertize label
                              'face 'taut-message-thread-link
                              'mouse-face 'highlight
                              'keymap taut-message-thread-button-map
                              'taut-thread-ts ts)
                  "\n")
          ;; If expanded inline, render replies
          (when expanded
            (let ((replies (taut-model-get-thread-replies ts)))
              (if (null replies)
                  (insert "             " (propertize "Loading replies..." 'face 'font-lock-comment-face) "\n")
                (let ((count (length replies))
                      (idx 0))
                  (dolist (reply replies)
                    (setq idx (1+ idx))
                    (taut-message--render-inline-reply reply (= idx count))))))))))

    (insert "\n")
    
    ;; Save message properties onto the paragraph block for targeting keys
    (add-text-properties msg-start (point)
                         (list 'taut-message-id (taut-message-id msg)
                               'taut-message-ts (taut-message-ts msg)))
    (when is-active-thread
      (add-face-text-property msg-start (point) 'taut-message-active-thread))))

(defun taut-active-thread-ts ()
  "Get the ts of the currently active thread in the `*Taut Thread*` buffer."
  (let ((buf (get-buffer "*Taut Thread*")))
    (when (and buf (get-buffer-window buf))
      (buffer-local-value 'taut-current-thread-ts buf))))

(defun taut-message--render-inline-reply (reply is-last)
  "Render a single inline thread reply message REPLY.
If IS-LAST is non-nil, use terminal branch markers."
  (let* ((user (taut-model-get-user (taut-message-user-id reply)))
         (is-me (equal (taut-user-id user) taut-current-user-id))
         (user-face (if is-me 'taut-message-me 'taut-message-username))
         (user-part (propertize (taut-user-username user) 'face user-face))
         (time-str (taut-message--format-ts (taut-message-ts reply)))
         (time-part (propertize time-str 'face 'taut-message-timestamp))
         (marker-branch (if is-last "             └─ " "             ├─ "))
         (marker-indent (if is-last "                " "             │  ")))
    (insert marker-branch user-part "  " time-part "\n")
    (insert marker-indent)
    (taut-message--insert-formatted-text (taut-message-text reply))
    (insert "\n")
    ;; Reactions in reply
    (when (taut-message-reactions reply)
      (insert marker-indent)
      (dolist (reaction (taut-message-reactions reply))
        (let ((emoji (car reaction))
              (reactors (cdr reaction)))
          (insert (propertize (format " %s %d " emoji (length reactors))
                              'face 'taut-message-reaction
                              'mouse-face 'highlight
                              'taut-reaction-emoji emoji
                              'taut-message-id (taut-message-id reply))
                  " ")))
      (insert "\n"))
    ;; Small vertical continuation spacer line
    (unless is-last
      (insert "             │\n"))))

(defun taut-message-toggle-thread-inline ()
  "Toggle inline expansion of the thread at point."
  (interactive)
  (let ((ts (get-text-property (point) 'taut-thread-ts)))
    (unless ts
      ;; Fallback: try to find any thread-ts or message-ts in the line/paragraph
      (setq ts (get-text-property (point) 'taut-message-ts)))
    (if ts
        (progn
          ;; Fetch replies asynchronously if not cached
          (let ((replies (taut-model-get-thread-replies ts)))
            (when (and (null replies) (boundp 'taut-bot-token) taut-bot-token)
              (ignore-errors (taut-api-fetch-replies taut-current-channel-id ts))))
          ;; Toggle in list
          (if (member ts taut-expanded-threads)
              (setq taut-expanded-threads (delete ts taut-expanded-threads))
            (push ts taut-expanded-threads))
          (taut-message-refresh))
      (message "No thread found at point to toggle."))))

(defun taut-message--format-ts (ts-str)
  "Format Slack timestamp TS-STR into human HH:MM:SS format."
  (if (and ts-str (string-match "^\\([0-9]+\\)" ts-str))
      (let* ((epoch (string-to-number (match-string 1 ts-str)))
             (time-val (seconds-to-time epoch)))
        (format-time-string "%H:%M:%S" time-val))
    "--:--:--"))

;;;; Rich Markdown Formatting Parser

(defun taut-message--insert-formatted-text (text)
  "Parse basic Slack formatting in TEXT and insert at point with nice faces."
  (let ((start 0))
    (while (string-match "\\(\\*\\([^*]+\\)\\*\\)\\|\\(_\\([^_]+\\)_\\)\\|\\(`\\([^`]+\\)`\\)\\|\\(<@\\([^>]+\\)>\\)" text start)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0)))
        ;; Insert preceding plain text
        (insert (substring text start match-start))
        
        ;; Apply match formatting
        (cond
         ;; *bold*
         ((match-string 2 text)
          (insert (propertize (match-string 2 text) 'face 'bold)))
         ;; _italic_
         ((match-string 4 text)
          (insert (propertize (match-string 4 text) 'face 'italic)))
         ;; `code`
         ((match-string 6 text)
          (insert (propertize (match-string 6 text) 'face 'taut-message-code)))
         ;; <@U_ID> mention
         ((match-string 8 text)
          (let* ((uid (match-string 8 text))
                 (user (taut-model-get-user uid)))
            (insert (propertize (format "@%s" (taut-user-username user))
                                'face 'taut-message-mention)))))
        
        (setq start match-end)))
    ;; Insert trailing plain text
    (insert (substring text start))))

;;;; Interactive Actions

(defun taut-message-open (chan-id)
  "Switch to the conversation buffer for CHAN-ID in the active main window."
  (let* ((chan (taut-model-get-channel chan-id))
         (buf-name (if (eq (taut-channel-type chan) 'dm)
                       (format "*Taut - @%s*" (taut-channel-name chan))
                     (format "*Taut - #%s*" (taut-channel-name chan))))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'taut-message-mode)
        (taut-message-mode))
      (setq taut-current-channel-id chan-id)
      (when (and (boundp 'taut-bot-token) taut-bot-token)
        (condition-case err
            (taut-api-fetch-history chan-id)
          (error
           (message "Taut: Failed to fetch history for %s: %s"
                    (taut-channel-name chan)
                    (error-message-string err)))))
      (taut-message-refresh))
    
    ;; Make sure we don't open inside the Sidebar window
    (let ((sidebar-win (get-buffer-window "*Taut Sidebar*")))
      (if (and sidebar-win (eq (selected-window) sidebar-win))
          (progn
            (select-window (next-window sidebar-win))
            (switch-to-buffer buf))
        (switch-to-buffer buf)))
    
    (goto-char (point-max))
    buf))

(defun taut-message-send ()
  "Prompt for a new message and append it to the current conversation."
  (interactive)
  (unless taut-current-channel-id
    (error "Not in an active conversation buffer"))
  (let* ((text (read-string "Send Message: ")))
    (unless (string-blank-p text)
      (if (and (boundp 'taut-bot-token) taut-bot-token)
          (taut-api-post-message taut-current-channel-id text)
        ;; Fallback to offline/mock
        (let* ((ts (format "%d.0000" (time-convert nil 'integer)))
               (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) text)))
          (taut-model-add-message
           (make-taut-message
            :id (concat "msg_" ts)
            :channel-id taut-current-channel-id
            :user-id taut-current-user-id
            :text text
            :ts ts
            :thread-ts nil
            :reply-count 0
            :is-unread nil
            :is-mention is-mention))))
      (taut-message-refresh)
      (goto-char (point-max)))))

(defun taut-message-start-thread ()
  "Start or open thread replies for the message under the cursor."
  (interactive)
  (let ((ts (get-text-property (point) 'taut-message-ts)))
    (if (null ts)
        (message "No message under point to thread.")
      (if (fboundp 'taut-thread-open)
          (funcall 'taut-thread-open ts)
        (message "Thread view is not yet loaded.")))))

(defun taut-message-add-reaction ()
  "Add an emoji reaction to the message under the cursor."
  (interactive)
  (let ((msg-id (get-text-property (point) 'taut-message-id))
        (ts (get-text-property (point) 'taut-message-ts)))
    (if (or (null msg-id) (null ts))
        (message "No message under point to react to.")
      (let* ((emoji (read-string "Reaction Emoji (e.g. 👍, 🎉, 😄): ")))
        (unless (string-blank-p emoji)
          (if (and (boundp 'taut-bot-token) taut-bot-token)
              (progn
                (taut-api-add-reaction taut-current-channel-id ts emoji)
                (ignore-errors (taut-api-fetch-history taut-current-channel-id)))
            ;; Fallback to offline/mock
            (let* ((chan-msgs (taut-model-get-messages taut-current-channel-id))
                   (msg (cl-find msg-id chan-msgs :key #'taut-message-id :test #'equal)))
              (when msg
                (let* ((reactions (taut-message-reactions msg))
                       (existing (assoc emoji reactions)))
                  (if existing
                      ;; Toggle user in list
                      (if (member taut-current-user-id (cdr existing))
                          (setcdr existing (delete taut-current-user-id (cdr existing)))
                        (setcdr existing (append (cdr existing) (list taut-current-user-id))))
                    ;; Append new reaction
                    (setf (taut-message-reactions msg)
                          (append reactions (list (cons emoji (list taut-current-user-id))))))))))
          (taut-message-refresh))))))

(defun taut-message-bury ()
  "Bury the current conversation buffer."
  (interactive)
  (bury-buffer))

;; Hook auto-updates
(add-hook 'taut-model-updated-hook #'taut-message-refresh)

(provide 'taut-message)
;;; taut-message.el ends here

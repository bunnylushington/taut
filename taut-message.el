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
(declare-function taut-message-reply-normal "taut-compose")
(declare-function taut-message-reply-quote "taut-compose")
(declare-function taut-thread-refresh "taut-thread")
(declare-function taut-compose-open "taut-compose" (channel-id &optional thread-ts quote-msg edit-ts edit-text))

(defvar taut-current-thread-ts)

(declare-function taut-code-block-dispatch "taut-transient")

(declare-function taut-dispatch "taut-transient")

;;;; Faces

(defface taut-message-username
  '((((background dark))  :foreground "#d3a4ff" :weight bold)
    (((background light)) :foreground "#4a154b" :weight bold)
    (t                    :foreground "#4a154b" :weight bold))
  "Face for sender usernames."
  :group 'taut-faces)

(defface taut-message-me
  '((((background dark))  :foreground "#f78af2" :weight bold)
    (((background light)) :foreground "#611f69" :weight bold)
    (t                    :foreground "#611f69" :weight bold))
  "Face for the current user's username."
  :group 'taut-faces)

(defface taut-message-timestamp
  '((((background dark))  :foreground "#718096" :height 0.8)
    (((background light)) :foreground "#8a8a8a" :height 0.8)
    (t                    :foreground "#8a8a8a" :height 0.8))
  "Face for message timestamps."
  :group 'taut-faces)

(defface taut-message-star
  '((((background dark))  :foreground "#f1c40f")
    (((background light)) :foreground "#f39c12")
    (t                    :foreground "#f1c40f"))
  "Face for the message star/bookmark indicator."
  :group 'taut-faces)

(defface taut-message-text
  '((t :inherit font-lock-variable-name-face :weight normal))
  "Face for standard message body text."
  :group 'taut-faces)

(defface taut-message-mention
  '((((background dark))  :background "#4a3e1d" :foreground "#ffeb3b" :weight bold :box (:line-width (1 . -1) :style flat-button))
    (((background light)) :background "#fff3cd" :foreground "#856404" :weight bold :box (:line-width (1 . -1) :style flat-button))
    (t                    :background "#fff3cd" :foreground "#856404" :weight bold :box (:line-width (1 . -1) :style flat-button)))
  "Face for @mentions in messages."
  :group 'taut-faces)

(defface taut-message-code
  '((t :inherit fixed-pitch :height 0.9))
  "Face for inline and multi-line markdown `code` blocks."
  :group 'taut-faces)

(defface taut-message-reaction
  '((((background dark))  :background "#2d3748" :foreground "#cbd5e0" :box (:line-width (1 . -1) :color "#4a5568" :style flat-button) :height 0.85)
    (((background light)) :background "#f8f9fa" :foreground "#495057" :box (:line-width (1 . -1) :color "#dee2e6" :style flat-button) :height 0.85)
    (t                    :background "#f8f9fa" :foreground "#495057" :box (:line-width (1 . -1) :color "#dee2e6" :style flat-button) :height 0.85))
  "Face for message reactions."
  :group 'taut-faces)

(defface taut-message-thread-link
  '((((background dark))  :foreground "#63b3ed" :weight bold :underline t :height 0.9)
    (((background light)) :foreground "#1264a3" :weight bold :underline t :height 0.9)
    (t                    :foreground "#1264a3" :weight bold :underline t :height 0.9))
  "Face for clickable thread reply markers."
  :group 'taut-faces)

(defface taut-message-active-thread
  '((((background dark))  :background "#2d3748" :extend t)
    (((background light)) :background "#f1f3f4" :extend t)
    (t                    :background "#f1f3f4" :extend t))
  "Face for highlighting the parent message of the active thread."
  :group 'taut-faces)

(defface taut-message-link
  '((((background dark))  :foreground "#63b3ed" :underline t)
    (((background light)) :foreground "#1264a3" :underline t)
    (t                    :foreground "#1264a3" :underline t))
  "Face for clickable general links/URLs."
  :group 'taut-faces)

;;;; Buffer-Local Variables

(defvar-local taut-current-channel-id nil
  "The channel-id represented by this conversation buffer.")

(defvar-local taut-expanded-threads nil
  "List of thread-ts currently expanded inline in this buffer.")

(defvar taut-code-block-map (make-sparse-keymap)
  "Keymap active inside code blocks.")

(define-key taut-code-block-map (kbd "c") #'taut-code-block-copy)
(define-key taut-code-block-map (kbd "v") #'taut-code-block-view)
(define-key taut-code-block-map (kbd "s") #'taut-code-block-save)
(define-key taut-code-block-map (kbd "C-c C-y") #'taut-code-block-copy)
(define-key taut-code-block-map (kbd "C-c C-v") #'taut-code-block-view)
(define-key taut-code-block-map (kbd "C-c C-s") #'taut-code-block-save)
(define-key taut-code-block-map (kbd "?") #'taut-code-block-dispatch)

(defun taut-code-block-copy ()
  "Copy the raw contents of the code block at point to the kill ring."
  (interactive)
  (let ((code (get-text-property (point) 'taut-code-block-content)))
    (if code
        (progn
          (kill-new code)
          (message "Copied code block contents to clipboard."))
      (message "No code block found at point."))))

(defun taut-code-block-view ()
  "Pop open a temporary buffer with the code in its native major-mode."
  (interactive)
  (let ((code (get-text-property (point) 'taut-code-block-content))
        (lang (get-text-property (point) 'taut-code-block-lang)))
    (if (not code)
        (message "No code block found at point.")
      (let* ((buf-name (format "*Taut Code - %s*" (if (string-blank-p (or lang "")) "text" lang)))
             (buf (get-buffer-create buf-name))
             (mode-sym (intern (concat (or (cdr (assoc lang '(("elisp" . "emacs-lisp")
                                                             ("python" . "python")
                                                             ("js" . "javascript")
                                                             ("javascript" . "javascript")
                                                             ("ts" . "typescript")
                                                             ("html" . "html")
                                                             ("css" . "css")
                                                             ("bash" . "sh")
                                                             ("sh" . "sh")
                                                             ("ruby" . "ruby")
                                                             ("go" . "go")
                                                             ("rust" . "rust"))))
                                           lang)
                                       "-mode"))))
        (with-current-buffer buf
          (erase-buffer)
          (insert code)
          (let ((buffer-file-name lang))
            (if (fboundp mode-sym)
                (funcall mode-sym)
              (normal-mode)))
          (setq-local header-line-format "📝 View Code Block  [q to close]"))
        (pop-to-buffer buf)
        (local-set-key (kbd "q") #'quit-window)))))

(defun taut-code-block-save (filename)
  "Save the raw contents of the code block at point to FILENAME."
  (interactive "FSave code block as: ")
  (let ((code (get-text-property (point) 'taut-code-block-content)))
    (if (not code)
        (message "No code block found at point.")
      (with-temp-file filename
        (insert code))
      (message "Code block saved to %s" filename))))

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

(defun taut-message-view-at-point ()
  "View the code block at point, if any."
  (interactive)
  (if (get-text-property (point) 'taut-code-block-content)
      (call-interactively #'taut-code-block-view)
    (message "No code block under cursor.")))

(defun taut-message-copy-at-point ()
  "Copy the code block at point, if any."
  (interactive)
  (if (get-text-property (point) 'taut-code-block-content)
      (call-interactively #'taut-code-block-copy)
    (message "No code block under cursor.")))

(defun taut-message-download-file (url name)
  "Prompt the user for a path and download file from URL named NAME."
  (let* ((default-path (expand-file-name (or name "downloaded_file")))
         (dest-path (read-file-name "Save file to: " nil default-path)))
    (when dest-path
      (taut-api-download-file url dest-path)
      (when (y-or-n-p (format "Open %s in Emacs? " (file-name-nondirectory dest-path)))
        (find-file dest-path)))))

(defun taut-message-handle-file-link ()
  "Handle interactive selection on clicking a file link."
  (interactive)
  (let* ((url (get-text-property (point) 'taut-file-url))
         (name (get-text-property (point) 'taut-file-name))
         (browser-url (or (get-text-property (point) 'taut-file-browser-url) url)))
    (if (not url)
        (message "No file link under point.")
      (let* ((choices '("Download file locally" "Open in Browser"))
             (choice (completing-read (format "Action for %s: " (or name "file"))
                                      choices nil t)))
        (cond
         ((string= choice "Download file locally")
          (taut-message-download-file url name))
         ((string= choice "Open in Browser")
          (browse-url browser-url)))))))

(defun taut-message-save-at-point ()
  "Save the code block or download the file at point, if any."
  (interactive)
  (cond
   ((get-text-property (point) 'taut-code-block-content)
    (call-interactively #'taut-code-block-save))
   ((get-text-property (point) 'taut-file-url)
    (let ((url (get-text-property (point) 'taut-file-url))
          (name (get-text-property (point) 'taut-file-name)))
      (taut-message-download-file url name)))
   (t
    (message "No code block or file link under cursor."))))

;;;; Major Mode Definition

(defvar taut-message-mode-map (make-sparse-keymap)
  "Keymap for `taut-message-mode`.")

(define-key taut-message-mode-map (kbd "r") #'taut-message-reply-normal)
(define-key taut-message-mode-map (kbd "R") #'taut-message-reply-quote)
(define-key taut-message-mode-map (kbd "t") #'taut-message-start-thread)
(define-key taut-message-mode-map (kbd "RET") #'taut-message-start-thread)
(define-key taut-message-mode-map (kbd "TAB") #'taut-message-toggle-thread-inline)
(define-key taut-message-mode-map (kbd "a") #'taut-message-add-reaction)
(define-key taut-message-mode-map (kbd "b") #'taut-message-toggle-star)
(define-key taut-message-mode-map (kbd "*") #'taut-message-toggle-star)
(define-key taut-message-mode-map (kbd "n") #'taut-message-next)
(define-key taut-message-mode-map (kbd "p") #'taut-message-previous)
(define-key taut-message-mode-map (kbd "g") #'taut-message-refresh)
(define-key taut-message-mode-map (kbd "q") #'taut-message-bury)
(define-key taut-message-mode-map (kbd "v") #'taut-message-view-at-point)
(define-key taut-message-mode-map (kbd "e") #'taut-message-edit)
(define-key taut-message-mode-map (kbd "s") #'taut-message-save-at-point)
(define-key taut-message-mode-map (kbd "c") #'taut-message-copy-at-point)
(define-key taut-message-mode-map (kbd "u") #'taut-message-upload-file)
(define-key taut-message-mode-map (kbd "d") #'taut-message-delete)
(define-key taut-message-mode-map (kbd "?") #'taut-dispatch)

(define-derived-mode taut-message-mode special-mode "Taut-Chat"
  "Major mode for a Taut Slack conversation buffer.

\\{taut-message-mode-map}"
  (setq buffer-read-only t
        word-wrap t
        wrap-prefix "         ") ; Align wrapped text under usernames nicely (9 spaces)
  (setq-local view-read-only nil)
  (when (and (boundp 'view-mode) view-mode)
    (view-mode -1))
  (visual-line-mode 1))

;;;; Rendering Engine

(defun taut-message--resolve-point-pos (old-point old-ts old-thread-ts old-reaction-emoji at-end)
  "Find the best point position in the rebuilt buffer."
  (cond
   (at-end
    (point-max))
   (old-ts
    ;; Search the rebuilt buffer for the same message
    (let ((found nil)
          (resolved (point-min))
          (pos (point-min)))
      (while (and (not found) (< pos (point-max)))
        (let ((next-pos (next-single-property-change pos 'taut-message-ts)))
          (if (equal (get-text-property pos 'taut-message-ts) old-ts)
              (progn
                (setq resolved pos)
                ;; Restore exact sub-element button focus if possible
                (cond
                 (old-thread-ts
                  (let ((th-pos pos)
                        (end-bound (or next-pos (point-max))))
                    (while (and (< th-pos end-bound)
                                (not (equal (get-text-property th-pos 'taut-thread-ts) old-thread-ts)))
                      (setq th-pos (1+ th-pos)))
                    (when (< th-pos end-bound)
                      (setq resolved th-pos))))
                 (old-reaction-emoji
                  (let ((re-pos pos)
                        (end-bound (or next-pos (point-max))))
                    (while (and (< re-pos end-bound)
                                (not (equal (get-text-property re-pos 'taut-reaction-emoji) old-reaction-emoji)))
                      (setq re-pos (1+ re-pos)))
                    (when (< re-pos end-bound)
                      (setq resolved re-pos)))))
                (setq found t))
            (setq pos (or next-pos (point-max))))))
      (if found resolved (min old-point (point-max)))))
   (t
    (min old-point (point-max)))))

(defun taut-message-refresh (&optional fetch-p)
  "Redraw the current conversation buffer.
If FETCH-P is non-nil (or when called interactively), fetch latest
history from API first."
  (interactive "P")
  (when (and (or fetch-p (called-interactively-p 'any))
             taut-current-channel-id
             (boundp 'taut-bot-token)
             taut-bot-token)
    (with-local-quit
      (ignore-errors (taut-api-fetch-history taut-current-channel-id))))
  (when taut-current-channel-id
    (let* ((inhibit-read-only t)
           ;; Save information for each window displaying this buffer
           (windows-info (mapcar (lambda (win)
                                   (with-selected-window win
                                     (list win
                                           (point)
                                           (get-text-property (point) 'taut-message-ts)
                                           (get-text-property (point) 'taut-thread-ts)
                                           (get-text-property (point) 'taut-reaction-emoji)
                                           (ignore-errors (count-screen-lines (window-start) (point)))
                                           (eobp))))
                                 (get-buffer-window-list (current-buffer) nil t)))
           ;; Also save for the current buffer itself (in case it is not visible in any window)
           (buf-old-point (point))
           (buf-old-ts (get-text-property (point) 'taut-message-ts))
           (buf-old-thread-ts (get-text-property (point) 'taut-thread-ts))
           (buf-old-reaction-emoji (get-text-property (point) 'taut-reaction-emoji))
           (buf-at-end (eobp)))
      
      (erase-buffer)
      (taut-message--render-history taut-current-channel-id)
      
      ;; 1. Restore the buffer-local point (for the buffer itself/selected window/fallback)
      (let ((resolved-point (taut-message--resolve-point-pos buf-old-point buf-old-ts buf-old-thread-ts buf-old-reaction-emoji buf-at-end)))
        (goto-char resolved-point))
      
      ;; 2. Restore point and scroll position for each window showing this buffer
      (dolist (info windows-info)
        (let ((win (nth 0 info))
              (w-point (nth 1 info))
              (w-ts (nth 2 info))
              (w-thread-ts (nth 3 info))
              (w-reaction-emoji (nth 4 info))
              (w-screen-line (nth 5 info))
              (w-at-end (nth 6 info)))
          (when (window-live-p win)
            (with-selected-window win
              (let ((new-pos (taut-message--resolve-point-pos w-point w-ts w-thread-ts w-reaction-emoji w-at-end)))
                (goto-char new-pos)
                (when w-screen-line
                  (ignore-errors (recenter w-screen-line)))))))))))

(defun taut-message--render-history (chan-id)
  "Render message list for CHAN-ID."
  (let* ((chan (taut-model-get-channel chan-id))
         (chan-type (if chan (taut-channel-type chan) 'public))
         (chan-name (if chan (taut-channel-name chan) chan-id))
         (chan-topic (if chan (taut-channel-topic chan) "(no topic set)"))
         (msgs (taut-model-get-messages chan-id)))
    ;; Buffer title banner
    (insert (propertize (if (eq chan-type 'dm)
                            (format "  👤 @%s" chan-name)
                          (format "  #  %s" chan-name))
                        'face '(:weight bold :height 1.2))
            "\n"
            (propertize (or chan-topic "(no topic set)")
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
         (is-me (and user (equal (taut-user-id user) taut-current-user-id)))
         (user-face (if is-me 'taut-message-me 'taut-message-username))
         (username (if user (or (taut-user-username user) "unknown") "unknown"))
         (user-part (propertize username 'face user-face))
         (time-str (taut-message--format-ts (taut-message-ts msg)))
         (time-part (propertize time-str 'face 'taut-message-timestamp))
         (active-thread-ts (taut-active-thread-ts))
         (is-active-thread (and active-thread-ts (equal active-thread-ts (taut-message-ts msg)))))

    ;; Header line: Username  [12:34]
    (insert user-part "  " time-part)
    (when (taut-message-is-starred msg)
      (insert " " (propertize "⭐" 'face 'taut-message-star)))
    (insert "\n")
    
    ;; Body line: (formatted text body with left indentation)
    (insert "         ")
    (taut-message--insert-formatted-text (taut-message-text msg) "         ")
    (insert "\n")

    ;; Reactions display (if any)
    (when (taut-message-reactions msg)
      (insert "         ")
      (dolist (reaction (taut-message-reactions msg))
        (let* ((emoji (car reaction))
               (reactors (cdr reaction))
               (display-emoji (taut-emoji-translate emoji)))
          (insert (propertize (format " %s %d " display-emoji (length reactors))
                              'face 'taut-message-reaction
                              'mouse-face 'highlight
                              'taut-reaction-emoji emoji
                              'taut-message-id (taut-message-id msg))
                  " ")))
      (insert "\n"))

    ;; Save root message properties onto the root part before inline replies render
    (add-text-properties msg-start (point)
                         (list 'taut-message-id (taut-message-id msg)
                               'taut-message-ts (taut-message-ts msg)))
    (when is-active-thread
      (add-face-text-property msg-start (point) 'taut-message-active-thread))

    ;; Thread replies indicator
    (let ((reply-count (taut-message-reply-count msg))
          (ts (taut-message-ts msg)))
      (when (and reply-count (> reply-count 0) (not (eq major-mode 'taut-thread-mode)))
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
                    (taut-message--render-inline-reply reply (= idx count) ts)))))))))

    (insert "\n")))

(defun taut-active-thread-ts ()
  "Get the ts of the currently active thread in the `*Taut Thread*` buffer."
  (let ((buf (get-buffer "*Taut Thread*")))
    (when (and buf (get-buffer-window buf))
      (buffer-local-value 'taut-current-thread-ts buf))))

(defun taut-message--render-inline-reply (reply is-last root-ts)
  "Render a single inline thread reply message REPLY.
If IS-LAST is non-nil, use terminal branch markers.
ROOT-TS is the timestamp of the parent message."
  (let* ((reply-start (point))
         (user (taut-model-get-user (taut-message-user-id reply)))
         (is-me (and user (equal (taut-user-id user) taut-current-user-id)))
         (user-face (if is-me 'taut-message-me 'taut-message-username))
         (username (if user (or (taut-user-username user) "unknown") "unknown"))
         (user-part (propertize username 'face user-face))
         (time-str (taut-message--format-ts (taut-message-ts reply)))
         (time-part (propertize time-str 'face 'taut-message-timestamp))
         (marker-branch (if is-last "             └─ " "             ├─ "))
         (marker-indent (if is-last "                " "             │  ")))
    (insert marker-branch user-part "  " time-part)
    (when (taut-message-is-starred reply)
      (insert " " (propertize "⭐" 'face 'taut-message-star)))
    (insert "\n")
    (insert marker-indent)
    (taut-message--insert-formatted-text (taut-message-text reply) marker-indent)
    (insert "\n")
    ;; Reactions in reply
    (when (taut-message-reactions reply)
      (insert marker-indent)
      (dolist (reaction (taut-message-reactions reply))
        (let* ((emoji (car reaction))
               (reactors (cdr reaction))
               (display-emoji (taut-emoji-translate emoji)))
          (insert (propertize (format " %s %d " display-emoji (length reactors))
                              'face 'taut-message-reaction
                              'mouse-face 'highlight
                              'taut-reaction-emoji emoji
                              'taut-message-id (taut-message-id reply))
                  " ")))
      (insert "\n"))
    ;; Small vertical continuation spacer line
    (unless is-last
      (insert "             │\n"))
    ;; Save message properties onto the reply block for targeting keys
    (add-text-properties reply-start (point)
                         (list 'taut-message-id (taut-message-id reply)
                               'taut-message-ts (taut-message-ts reply)
                               'taut-thread-ts root-ts))))

(defun taut-message-toggle-thread-inline ()
  "Toggle inline expansion of the thread at point."
  (interactive)
  (let ((ts (get-text-property (point) 'taut-thread-ts)))
    (unless ts
      ;; Fallback: try to find any thread-ts or message-ts in the line/paragraph
      (setq ts (get-text-property (point) 'taut-message-ts)))
    (if ts
        (progn
          ;; Fetch replies asynchronously if not cached or incomplete
          (let* ((replies (taut-model-get-thread-replies ts))
                 (chan-id taut-current-channel-id)
                 ;; If chan-id is nil, find it from the message database
                 (root-msg
                  (if chan-id
                      (cl-find ts (taut-model-get-messages chan-id) :key #'taut-message-ts :test #'equal)
                    (let (found-msg)
                      (maphash (lambda (cid msgs)
                                 (let ((found (cl-find ts msgs :key #'taut-message-ts :test #'equal)))
                                   (when found
                                     (setq chan-id cid
                                           found-msg found))))
                               taut-messages)
                      found-msg)))
                 (expected-replies (if root-msg (or (taut-message-reply-count root-msg) 0) 0)))
            (when (and (or (null replies)
                           (< (length replies) expected-replies))
                       chan-id
                       (boundp 'taut-bot-token)
                       taut-bot-token)
              (ignore-errors (taut-api-fetch-replies chan-id ts))))
          ;; Toggle in list
          (if (member ts taut-expanded-threads)
              (setq taut-expanded-threads (delete ts taut-expanded-threads))
            (push ts taut-expanded-threads))
          (taut-message-refresh))
      (message "No thread found at point to toggle."))))

(defun taut-message--format-ts (ts-str)
  "Format Slack timestamp TS-STR into human readable format.
Returns a string of \\=`Weekday Month Day, Year, HH:MM:SS\\='."
  (if (and ts-str (string-match "^\\([0-9]+\\)" ts-str))
      (let* ((epoch (string-to-number (match-string 1 ts-str)))
             (time-val (seconds-to-time epoch)))
        (replace-regexp-in-string "  " " " (format-time-string "%A %B %e, %Y, %H:%M:%S" time-val)))
    "--:--:--"))

;;;; Emoji Translation Support

(defvar taut-emoji-alist
  '(("thumbsup" . "👍")
    ("+1" . "👍")
    ("thumbsdown" . "👎")
    ("-1" . "👎")
    ("raised_hands" . "🙌")
    ("tada" . "🎉")
    ("party" . "🎉")
    ("smile" . "😄")
    ("heart" . "❤️")
    ("fire" . "🔥")
    ("eyes" . "👀")
    ("heavy_check_mark" . "✅")
    ("white_check_mark" . "✅")
    ("check" . "✅")
    ("rocket" . "🚀")
    ("thinking" . "🤔")
    ("thinking_face" . "🤔")
    ("clap" . "👏")
    ("cry" . "😢")
    ("joy" . "😂")
    ("sob" . "😭")
    ("pray" . "🙏")
    ("pensive" . "😔")
    ("star" . "⭐")
    ("grin" . "😁")
    ("wink" . "😉")
    ("sunglasses" . "😎")
    ("disappointed" . "😞")
    ("rage" . "😡")
    ("ok_hand" . "👌"))
  "Alist mapping Slack emoji names/shortcodes to unicode characters.")

(defvar taut-emoticon-alist
  '((":-)"  . "🙂")
    (":)"   . "🙂")
    (":-D"  . "😃")
    (":D"   . "😃")
    (";-)"  . "😉")
    (";)"   . "😉")
    (":-P"  . "😛")
    (":P"   . "😛")
    (":-p"  . "😛")
    (":p"   . "😛")
    (":-("  . "🙁")
    (":("   . "🙁")
    (":-O"  . "😮")
    (":O"   . "😮")
    (":-o"  . "😮")
    (":o"   . "😮")
    ("B-)"  . "😎")
    ("B)"   . "😎")
    (">:-)" . "😈")
    (">:)"  . "😈")
    (":-/"  . "😕")
    (":/"   . "😕")
    ("<3"   . "❤️"))
  "Alist mapping standard emoticons to Unicode emojis.")

(defun taut-emoticon--boundary-p (char)
  "Return non-nil if CHAR is a valid emoticon boundary (non-alphanumeric or nil)."
  (or (null char)
      (not (or (and (>= char ?a) (<= char ?z))
               (and (>= char ?A) (<= char ?Z))
               (and (>= char ?0) (<= char ?9))))))

(defun taut-emoticon-translate-string (text)
  "Translate all emoticons in TEXT to Unicode emoji equivalents.
Emoticons are only translated if they are preceded by a non-alphanumeric
character or beginning of string, and followed by a non-alphanumeric
character or end of string."
  (if (string-blank-p text)
      text
    (with-temp-buffer
      (insert text)
      (let ((case-fold-search nil))
        (dolist (pair taut-emoticon-alist)
          (let* ((emoticon (car pair))
                 (emoji (cdr pair))
                 (escaped (regexp-quote emoticon)))
            (goto-char (point-min))
            (while (re-search-forward escaped nil t)
              (let ((start (match-beginning 0))
                    (end (match-end 0)))
                (when (and (taut-emoticon--boundary-p (char-before start))
                           (taut-emoticon--boundary-p (char-after end)))
                  (replace-match emoji t t)))))))
      (buffer-string))))

(defun taut-emoji-translate (name)
  "Translate Slack emoji shortcode NAME to unicode.
Allows both raw shortcode names and bracketed format like \":raised_hands:\"."
  (let* ((name (or name ""))
         (clean-name (if (and (string-prefix-p ":" name) (string-suffix-p ":" name))
                        (substring name 1 -1)
                      name))
         (match (assoc clean-name taut-emoji-alist)))
    (if match
        (cdr match)
      ;; Fallback: return the original shortcode with colons intact
      (if (and (string-prefix-p ":" name) (string-suffix-p ":" name))
          name
        (concat ":" name ":")))))

;;;; Rich Markdown Formatting Parser

(defun taut-message--insert-formatted-line (text)
  "Parse advanced Slack formatting in a single line TEXT.
Insert at point with premium faces and interactive links."
  (let* ((text (taut-emoticon-translate-string (or text "")))
         (start 0))
    (while (string-match "\\(\\*\\([^*]+\\)\\*\\)\\|\\(_\\([^_]+\\)_\\)\\|\\(~\\([^~]+\\)~\\)\\|\\(`\\([^`]+\\)`\\)\\|\\(<@\\([^>|]+\\)\\(|\\([^>]+\\)\\)?>\\)\\|\\(<#\\([^>|]+\\)\\(|\\([^>]+\\)\\)?>\\)\\|\\(<\\(\\(?:https?\\|taut-file\\)://[^>|]+\\)\\(|\\([^>]+\\)\\)?>\\)\\|\\(:\\([a-zA-Z0-9_+-]+\\):\\)" text start)
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
         ;; ~strike-through~
         ((match-string 6 text)
          (insert (propertize (match-string 6 text) 'face '(:strike-through t :foreground "#8a8a8a"))))
         ;; `code`
         ((match-string 8 text)
          (insert (propertize (match-string 8 text) 'face 'taut-message-code)))
         ;; <@U_ID|label> mention (interactive to open DM)
         ((match-string 10 text)
          (let* ((uid (match-string 10 text))
                 (label (match-string 12 text))
                 (user (taut-model-get-user uid))
                 (username (or label (if user (taut-user-username user) uid) uid)))
            (insert (propertize (format "@%s" username)
                                'face 'taut-message-mention
                                'mouse-face 'highlight
                                'help-echo (format "Click/RET to DM @%s" username)
                                'keymap (let ((map (make-sparse-keymap)))
                                          (define-key map (kbd "RET")
                                            (lambda ()
                                              (interactive)
                                              (let ((chan-id
                                                     (if (and (boundp 'taut-bot-token) taut-bot-token)
                                                         (taut-api-open-dm uid)
                                                       (let* ((mock-id (concat "C_" (upcase username) "_DM"))
                                                              (existing (taut-model-get-channel mock-id)))
                                                         (unless existing
                                                           (taut-model-add-channel
                                                            (make-taut-channel
                                                             :id mock-id
                                                             :name username
                                                             :type 'dm
                                                             :unread-count 0
                                                             :mention-count 0)))
                                                         mock-id))))
                                                (taut-message-open chan-id))))
                                          (define-key map (kbd "<mouse-1>")
                                            (lambda (event)
                                              (interactive "e")
                                              (posn-set-point (event-end event))
                                              (let ((chan-id
                                                     (if (and (boundp 'taut-bot-token) taut-bot-token)
                                                         (taut-api-open-dm uid)
                                                       (let* ((mock-id (concat "C_" (upcase username) "_DM"))
                                                              (existing (taut-model-get-channel mock-id)))
                                                         (unless existing
                                                           (taut-model-add-channel
                                                            (make-taut-channel
                                                             :id mock-id
                                                             :name username
                                                             :type 'dm
                                                             :unread-count 0
                                                             :mention-count 0)))
                                                         mock-id))))
                                                (taut-message-open chan-id))))
                                          map)))))
         ;; <#C_ID|label> channel link (interactive to open channel)
         ((match-string 14 text)
          (let* ((cid (match-string 14 text))
                 (label (match-string 16 text))
                 (chan (taut-model-get-channel cid))
                 (chan-name (or label (if chan (taut-channel-name chan) cid) cid)))
            (insert (propertize (format "#%s" chan-name)
                                'face 'taut-message-mention
                                'mouse-face 'highlight
                                'help-echo (format "Click/RET to jump to channel #%s" chan-name)
                                'taut-channel-id cid
                                'keymap (let ((map (make-sparse-keymap)))
                                          (define-key map (kbd "RET") (lambda () (interactive) (taut-message-open cid)))
                                          (define-key map (kbd "<mouse-1>") (lambda (event) (interactive "e") (posn-set-point (event-end event)) (taut-message-open cid)))
                                          map)))))
          ;; <https://...|label> general link (interactive to open URL)
          ((match-string 18 text)
           (let* ((url (match-string 18 text))
                  (label (or (match-string 20 text) url)))
             (if (string-prefix-p "taut-file://" url)
                 (let* ((orig-url (replace-regexp-in-string "^taut-file://" "https://" url))
                        (name (when (string-match "[?&]taut_name=\\([^&]+\\)" orig-url)
                                (url-unhex-string (match-string 1 orig-url))))
                        (browser-url (when (string-match "[?&]browser_url=\\([^&]+\\)" orig-url)
                                       (url-unhex-string (match-string 1 orig-url))))
                        (clean-url (let ((u orig-url))
                                     (setq u (replace-regexp-in-string "[?&]taut_name=[^&]+" "" u))
                                     (setq u (replace-regexp-in-string "[?&]browser_url=[^&]+" "" u))
                                     u)))
                   (insert (propertize label
                                       'face 'taut-message-link
                                       'mouse-face 'highlight
                                       'help-echo (format "Click/RET to download/open: %s" (or name "file"))
                                       'taut-file-url clean-url
                                       'taut-file-name name
                                       'taut-file-browser-url browser-url
                                       'keymap (let ((map (make-sparse-keymap)))
                                                 (define-key map (kbd "RET") #'taut-message-handle-file-link)
                                                 (define-key map (kbd "<mouse-1>")
                                                   (lambda (event)
                                                     (interactive "e")
                                                     (posn-set-point (event-end event))
                                                     (taut-message-handle-file-link)))
                                                 map))))
               (insert (propertize label
                                   'face 'taut-message-link
                                   'mouse-face 'highlight
                                   'help-echo (format "Click/RET to open link: %s" url)
                                   'keymap (let ((map (make-sparse-keymap)))
                                             (define-key map (kbd "RET") (lambda () (interactive) (browse-url url)))
                                             (define-key map (kbd "<mouse-1>") (lambda (event) (interactive "e") (posn-set-point (event-end event)) (browse-url url)))
                                             map))))))
         ;; :emoji:
         ((match-string 22 text)
          (let ((emoji-name (match-string 22 text)))
            (insert (taut-emoji-translate emoji-name)))))
         
         (setq start match-end)))
    ;; Insert trailing plain text
    (insert (substring text start))))

(defun taut-message--insert-formatted-text-normal (text &optional prefix)
  "Parse basic Slack formatting in normal TEXT (no multi-line code blocks)."
  (let ((lines (split-string (or text "") "\n"))
        (first t))
    (dolist (line lines)
      (if first
          (setq first nil)
        (insert "\n")
        (when prefix
          (insert prefix)))
      (taut-message--insert-formatted-line line))))

(defun taut-message--insert-code-block-rendered (lang code prefix)
  "Render a multi-line code block in LANG with content CODE."
  (let* ((lang (or lang "text"))
         (code (or code ""))
         (start-pos (point))
         (border-char ?─)
         (width 40)
         (border-line (make-string width border-char))
         (code-face 'taut-message-code)
         (margin-prefix (concat prefix "│  ")))
    
    ;; Render top border with language label
    (insert "\n" prefix "┌" border-line "\n")
    (insert prefix "│  " (propertize (format "💻 CODE (%s) - [c:copy, v:view, s:save]" (if (string-blank-p lang) "text" (upcase lang))) 'face '(:weight bold :foreground "#8a8a8a")) "\n")
    (insert prefix "├" border-line "\n")
    
    ;; Insert code content with prefix on each line, limited to 10 lines
    (let* ((lines (split-string code "\n"))
           (total-count (length lines))
           (max-lines 10)
           (show-lines (if (> total-count max-lines)
                           (butlast lines (- total-count max-lines))
                         lines))
           (hidden-count (- total-count max-lines)))
      (dolist (line show-lines)
        (insert margin-prefix (propertize (concat line "\n") 'face code-face)))
      (when (> hidden-count 0)
        (insert margin-prefix
                (propertize (format "... (+%d lines hidden, press v to view) ...\n" hidden-count)
                            'face (list '(:slant italic :foreground "#8a8a8a") code-face)))))
    
    ;; Render bottom border
    (insert prefix "└" border-line "\n")
    
    ;; Save text properties and interactive keymap on the whole rendered block
    (add-text-properties start-pos (point)
                         (list 'taut-code-block-content code
                                'taut-code-block-lang lang
                                'keymap taut-code-block-map
                                'rear-nonsticky t))))

(defun taut-message--insert-formatted-text (text &optional prefix)
  "Parse Slack formatting, including multi-line code blocks and inline formatting."
  (let* ((text (or text ""))
         (start-pos (point))
         (start 0)
         (len (length text)))
    (while (and (< start len)
                (string-match "```\\([^\n\r]*\\)\r?\n" text start))
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (lang (string-trim (match-string 1 text)))
             (code nil)
             (block-end nil))
        
        ;; Insert preceding normal text
        (let ((pre-text (substring text start match-start)))
          (unless (string-blank-p pre-text)
            (taut-message--insert-formatted-text-normal pre-text prefix)))
        
        ;; Check if this is a file snippet fallback block: ```lang\n```\n<content>
        (if (and (not (string-blank-p lang))
                 (string-match "\\````\r?\n" (substring text match-end)))
            (let ((content-start (+ match-end (match-end 0))))
              (if (string-match "\r?\n[ \t\r]*```" text content-start)
                  (setq code (substring text content-start (match-beginning 0))
                        block-end (match-end 0))
                (setq code (substring text content-start)
                      block-end len)))
          
          ;; Normal code block: ```lang\n<code>\n```
          (if (string-match "\r?\n[ \t\r]*```" text match-end)
              (setq code (substring text match-end (match-beginning 0))
                    block-end (match-end 0))
            (setq code (substring text match-end)
                  block-end len)))
        
        ;; Render the code block
        (taut-message--insert-code-block-rendered lang code (or prefix "         "))
        (setq start block-end)))
    
    ;; Insert trailing normal text
    (let ((post-text (substring text start)))
      (unless (string-blank-p post-text)
        (taut-message--insert-formatted-text-normal post-text prefix)))
    
    (when prefix
      (add-text-properties start-pos (point) (list 'wrap-prefix prefix)))))

;;;; Interactive Actions

(defun taut-message-open (chan-id)
  "Switch to the conversation buffer for CHAN-ID in the active main window."
  (let* ((chan (taut-model-get-channel chan-id))
         (chan-type (if chan (taut-channel-type chan) 'public))
         (chan-name (if chan (taut-channel-name chan) chan-id))
         (buf-name (if (eq chan-type 'dm)
                       (format "*Taut - @%s*" chan-name)
                     (format "*Taut - #%s*" chan-name)))
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
                    chan-name
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
  "Start composing a new message in the current conversation buffer.
Uses the dedicated compose buffer."
  (interactive)
  (unless taut-current-channel-id
    (error "Not in an active conversation buffer"))
  (if (fboundp 'taut-compose-open)
      (taut-compose-open taut-current-channel-id)
    (error "Composer is not loaded")))

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
  "Add an emoji reaction to the message under the cursor.
Uses a premium autocomplete picker mapping emojis and shortcodes."
  (interactive)
  (let* ((msg-id (get-text-property (point) 'taut-message-id))
         (ts (get-text-property (point) 'taut-message-ts))
         (msg (and ts (taut-model-get-message-by-ts ts)))
         (chan-id (and msg (taut-message-channel-id msg))))
    (if (or (null msg-id) (null ts) (null chan-id))
        (message "No message under point to react to.")
      (let* ((candidates nil))
        (dolist (item taut-emoji-alist)
          (let* ((shortcode (car item))
                 (unicode (cdr item))
                 (display-str (format "%s  :%s:" unicode shortcode)))
            (unless (assoc display-str candidates)
              (push (cons display-str shortcode) candidates))))
        (setq candidates (nreverse candidates))
        (let* ((choice (completing-read "Add reaction (emoji/shortcode): " candidates nil nil))
               (emoji (or (cdr (assoc choice candidates)) choice)))
          (unless (string-blank-p emoji)
            (let ((is-online (and (boundp 'taut-bot-token) taut-bot-token)))
              (if is-online
                  (taut-api-add-reaction chan-id ts emoji)
                ;; Fallback to offline/mock
                (let* ((chan-msgs (taut-model-get-messages chan-id))
                       (target-msg (cl-find msg-id chan-msgs :key #'taut-message-id :test #'equal)))
                  (unless target-msg
                    ;; Check thread replies
                    (maphash (lambda (_thread-ts replies)
                               (unless target-msg
                                 (setq target-msg (cl-find msg-id replies :key #'taut-message-id :test #'equal))))
                             taut-threads))
                  (when target-msg
                    (let* ((reactions (taut-message-reactions target-msg))
                           (existing (assoc emoji reactions)))
                      (if existing
                          ;; Toggle user in list
                          (if (member taut-current-user-id (cdr existing))
                              (setcdr existing (delete taut-current-user-id (cdr existing)))
                            (setcdr existing (append (cdr existing) (list taut-current-user-id))))
                        ;; Append new reaction
                        (setf (taut-message-reactions target-msg)
                              (append reactions (list (cons emoji (list taut-current-user-id))))))))))
              ;; If online, re-fetch history or replies to sync with server state
              (when is-online
                (let ((thread-ts (taut-message-thread-ts msg)))
                  (if (and thread-ts (not (equal thread-ts ts)))
                      (ignore-errors (taut-api-fetch-replies chan-id thread-ts))
                    (ignore-errors (taut-api-fetch-history chan-id)))))
              (taut-model-trigger-update)
              (taut-message-refresh))))))))

(defun taut-message-toggle-star ()
  "Star or unstar (bookmark) the message under the cursor."
  (interactive)
  (let ((ts (get-text-property (point) 'taut-message-ts)))
    (if (null ts)
        (message "No message under point to bookmark.")
      (let ((msg (taut-model-get-message-by-ts ts)))
        (if (null msg)
            (message "Could not locate message metadata for bookmarking.")
          (let* ((chan-id (taut-message-channel-id msg))
                 (currently-starred (taut-message-is-starred msg))
                 (new-state (not currently-starred)))
            (if (and (boundp 'taut-bot-token) taut-bot-token)
                ;; Online logic
                (condition-case err
                    (progn
                      (if new-state
                          (taut-api-star-add chan-id ts)
                        (taut-api-star-remove chan-id ts))
                      (setf (taut-message-is-starred msg) new-state)
                      (taut-model-trigger-update)
                      (message "Taut: %s message." (if new-state "Bookmarked" "Unbookmarked")))
                  (error
                   (message "Taut: Bookmark action failed: %s" (error-message-string err))))
              ;; Offline / Fallback logic
              (setf (taut-message-is-starred msg) new-state)
              (taut-model-trigger-update)
              (message "Taut (offline): %s message." (if new-state "Bookmarked" "Unbookmarked")))))))))

(defun taut-message-delete ()
  "Delete the message under the cursor after confirmation."
  (interactive)
  (let* ((ts (get-text-property (point) 'taut-message-ts))
         (msg (and ts (taut-model-get-message-by-ts ts)))
         (chan-id (and msg (taut-message-channel-id msg))))
    (if (or (null ts) (null chan-id))
        (message "No message under point to delete.")
      (let ((is-online (and (boundp 'taut-bot-token) taut-bot-token)))
        (if is-online
            (when (y-or-n-p "Delete this message? ")
              (taut-api-delete-message chan-id ts)
              (message "Taut: Message deletion requested.")
              (taut-message-refresh))
          ;; Mock delete for offline testing
          (when (y-or-n-p "Delete this message (Mock)? ")
            (taut-model-delete-message ts)
            (message "Taut: Message deleted (offline mock).")
            (taut-message-refresh)))))))

(defun taut-message--start-of-message (pos)
  "Find the start position of the message block containing POS."
  (let ((ts (get-text-property pos 'taut-message-ts)))
    (if (not ts)
        pos
      (let ((change (previous-single-property-change pos 'taut-message-ts)))
        (if change (1+ change) (point-min))))))

(defun taut-message-next ()
  "Move point to the start of the next message."
  (interactive)
  (let ((current-ts (get-text-property (point) 'taut-message-ts))
        (pos (point))
        (found nil))
    (if current-ts
        (let ((change (next-single-property-change pos 'taut-message-ts)))
          (if change
              (setq pos change)
            (setq pos (point-max)))) )
    (while (and (< pos (point-max)) (not found))
      (if (get-text-property pos 'taut-message-ts)
          (setq found t)
        (let ((next-change (next-single-property-change pos 'taut-message-ts)))
          (if next-change
              (setq pos next-change)
            (setq pos (point-max))))))
    (if found
        (goto-char pos)
      (goto-char (point-max))
      (message "End of messages."))))

(defun taut-message-previous ()
  "Move point to the start of the previous message."
  (interactive)
  (let* ((pos (point))
         (current-start (taut-message--start-of-message pos)))
    (if (< current-start pos)
        (goto-char current-start)
      (let ((search-pos (1- current-start))
            (found nil))
        (while (and (>= search-pos (point-min)) (not found))
          (if (get-text-property search-pos 'taut-message-ts)
              (setq found t)
            (let ((prev-change (previous-single-property-change search-pos 'taut-message-ts)))
              (if prev-change
                  (setq search-pos prev-change)
                (setq search-pos (1- (point-min)))))))
        (if found
            (goto-char (taut-message--start-of-message search-pos))
          (goto-char (point-min))
          (message "Beginning of messages."))))))

(defun taut-message-bury ()
  "Bury the current conversation buffer."
  (interactive)
  (bury-buffer))

(defun taut-message-refresh-all ()
  "Refresh all active `taut-message-mode` buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'taut-message-mode)
          (taut-message-refresh))))))

;;;###autoload
(defun taut-message-upload-file (file-path)
  "Upload a file selected by FILE-PATH to the current channel/thread."
  (interactive "fUpload File: ")
  (unless taut-current-channel-id
    (error "Not in an active conversation buffer"))
  (let ((chan-id taut-current-channel-id)
        (thread-ts (and (boundp 'taut-current-thread-ts) taut-current-thread-ts))
        (is-thread (eq major-mode 'taut-thread-mode)))
    (if (and (boundp 'taut-bot-token) taut-bot-token)
        (progn
          (taut-api-upload-file chan-id file-path thread-ts)
          ;; Refresh chat or thread after API upload
          (if is-thread
              (taut-thread-refresh t)
            (taut-message-refresh t)))
      ;; Offline/Mock fallback
      (let* ((filename (file-name-nondirectory file-path))
             (size (file-attribute-size (file-attributes file-path)))
             (ts (format "%d.0000" (time-convert nil 'integer)))
             (mock-text (format "📎 *Uploaded file*: _%s_ (%d bytes)" filename size)))
        (taut-model-add-message
         (make-taut-message
          :id (concat "msg_" ts)
          :channel-id chan-id
          :user-id taut-current-user-id
          :text mock-text
          :ts ts
          :thread-ts thread-ts
          :reply-count 0
          :is-unread nil
          :is-mention nil))
        (if is-thread
            (taut-thread-refresh)
          (taut-message-refresh))
        (message "Taut: Simulated upload of %s (%d bytes)" filename size)))))

;;;###autoload
(defun taut-message-edit ()
  "Edit the message under the cursor if it was sent by the current user."
  (interactive)
  (let* ((ts (get-text-property (point) 'taut-message-ts))
         (msg (and ts (taut-model-get-message-by-ts ts)))
         (chan-id (and msg (taut-message-channel-id msg))))
    (cond
     ((null ts)
      (message "No message under point to edit."))
     ((null msg)
      (message "Could not retrieve message details."))
     ((not (equal (taut-message-user-id msg) taut-current-user-id))
      (user-error "You can only edit your own messages."))
     (t
      (let ((text (taut-message-text msg))
            (thread-ts (taut-message-thread-ts msg)))
        (taut-compose-open chan-id thread-ts nil ts text))))))


;; Hook auto-updates
(add-hook 'taut-model-updated-hook #'taut-message-refresh-all)

(provide 'taut-message)
;;; taut-message.el ends here

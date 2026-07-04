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
  '((((background dark))  :inherit fixed-pitch :background "#2d3748" :height 0.9)
    (((background light)) :inherit fixed-pitch :background "#f4f4f4" :height 0.9)
    (t                    :inherit fixed-pitch :background "#f4f4f4" :height 0.9))
  "Face for inline markdown `code` blocks."
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

;;;; Buffer-Local Variables

(defvar-local taut-current-channel-id nil
  "The channel-id represented by this conversation buffer.")

(defvar-local taut-expanded-threads nil
  "List of thread-ts currently expanded inline in this buffer.")

(defvar taut-code-block-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'taut-code-block-copy)
    (define-key map (kbd "e") #'taut-code-block-edit)
    (define-key map (kbd "s") #'taut-code-block-save)
    (define-key map (kbd "C-c C-y") #'taut-code-block-copy)
    (define-key map (kbd "C-c C-e") #'taut-code-block-edit)
    (define-key map (kbd "C-c C-s") #'taut-code-block-save)
    (define-key map (kbd "?") #'taut-code-block-dispatch)
    map)
  "Keymap active inside code blocks.")

(defun taut-code-block-copy ()
  "Copy the raw contents of the code block at point to the kill ring."
  (interactive)
  (let ((code (get-text-property (point) 'taut-code-block-content)))
    (if code
        (progn
          (kill-new code)
          (message "Copied code block contents to clipboard."))
      (message "No code block found at point."))))

(defun taut-code-block-edit ()
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

;;;; Major Mode Definition

(defvar taut-message-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'taut-message-reply-normal)
    (define-key map (kbd "R") #'taut-message-reply-quote)
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
        wrap-prefix "         ") ; Align wrapped text under usernames nicely (9 spaces)
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
If FETCH-P is non-nil (or when called interactively), fetch latest history from API first."
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
    (insert user-part "  " time-part "\n")
    
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
    (insert marker-branch user-part "  " time-part "\n")
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
  "Format Slack timestamp TS-STR into human 'Weekday Month Day, Year, HH:MM:SS' format."
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

(defun taut-emoji-translate (name)
  "Translate Slack emoji shortcode NAME (e.g. \"raised_hands\" or \":raised_hands:\") to unicode."
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
  "Parse basic Slack formatting in a single line TEXT and insert at point with nice faces."
  (let* ((text (or text ""))
         (start 0))
    (while (string-match "\\(\\*\\([^*]+\\)\\*\\)\\|\\(_\\([^_]+\\)_\\)\\|\\(`\\([^`]+\\)`\\)\\|\\(<@\\([^>]+\\)>\\)\\|\\(:\\([a-zA-Z0-9_+-]+\\):\\)" text start)
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
                 (user (taut-model-get-user uid))
                 (username (if user (or (taut-user-username user) uid) uid)))
            (insert (propertize (format "@%s" username)
                                'face 'taut-message-mention))))
         ;; :emoji:
         ((match-string 10 text)
          (let ((emoji-name (match-string 10 text)))
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
         (width (or (and (window-live-p (selected-window)) (- (window-body-width) 14)) 60))
         (border-line (make-string width border-char))
         (code-face 'taut-message-code)
         (margin-prefix (concat prefix "│  ")))
    
    ;; Render top border with language label
    (insert "\n" prefix "┌" border-line "\n")
    (insert prefix "│  " (propertize (format "💻 CODE (%s) - [c:copy, e:edit, s:save]" (if (string-blank-p lang) "text" (upcase lang))) 'face '(:weight bold :foreground "#8a8a8a")) "\n")
    (insert prefix "├" border-line "\n")
    
    ;; Insert code content with prefix on each line
    (let ((lines (split-string code "\n")))
      (dolist (line lines)
        (insert margin-prefix (propertize line 'face code-face) "\n")))
    
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
         (trimmed-text (string-trim text)))
    ;; Check if the text contains a Slack file snippet fallback pattern anywhere
    ;; format: ```<filename>\n```\n<content>
    (if (string-match "```\\([^\n\r]+\\)\r?\n```\r?\n\\([^\000]*?\\)\\(?:\r?\n[ \t\r]*```\\)?\\'" trimmed-text)
        (let ((pre-text (substring trimmed-text 0 (match-beginning 0)))
              (filename (match-string 1 trimmed-text))
              (content (match-string 2 trimmed-text)))
          (unless (string-blank-p pre-text)
            (taut-message--insert-formatted-text-normal pre-text prefix))
          (taut-message--insert-code-block-rendered filename content (or prefix "         ")))
      
      ;; Normal multi-line code blocks matching loop
      (while (string-match "```\\([^\n\r]*\\)\r?\n\\([^\000]*?\\)\r?\n[ \t\r]*```" text start)
        (let ((match-start (match-beginning 0))
              (match-end (match-end 0))
              (lang (string-trim (or (match-string 1 text) "")))
              (code (match-string 2 text)))
          ;; Insert normal text preceding the code block
          (let ((pre-text (substring text start match-start)))
            (unless (string-blank-p pre-text)
              (taut-message--insert-formatted-text-normal pre-text prefix)))
          
          ;; Insert the rendered code block
          (taut-message--insert-code-block-rendered lang code (or prefix "         "))
          
          (setq start match-end)))
      
      ;; Insert trailing normal text
      (let ((post-text (substring text start)))
        (unless (string-blank-p post-text)
          (taut-message--insert-formatted-text-normal post-text prefix))))
    
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
  "Start composing a new message in the current conversation buffer using the compose buffer."
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

(defun taut-message-refresh-all ()
  "Refresh all active `taut-message-mode` buffers."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (eq major-mode 'taut-message-mode)
          (taut-message-refresh))))))

;; Hook auto-updates
(add-hook 'taut-model-updated-hook #'taut-message-refresh-all)

(provide 'taut-message)
;;; taut-message.el ends here

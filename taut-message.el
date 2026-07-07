;;; taut-message.el --- Rich Conversation Buffer for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us
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
(declare-function taut-get-chat-window "taut")

(defvar taut-strict-windows)
(defvar taut-current-thread-ts)

(declare-function taut-code-block-dispatch "taut-transient")

(declare-function taut-dispatch "taut-transient")
(declare-function taut-search-quick "taut-search")


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

(defface taut-message-huddle-box
  '((((background dark))  :background "#1a365d" :foreground "#90cdf4" :box (:line-width 1 :color "#2b6cb0" :style flat-button))
    (((background light)) :background "#ebf8ff" :foreground "#2b6cb0" :box (:line-width 1 :color "#90cdf4" :style flat-button))
    (t                    :background "#ebf8ff" :foreground "#2b6cb0" :box (:line-width 1 :color "#90cdf4" :style flat-button)))
  "Face for beautiful Slack Huddle inline summary boxes."
  :group 'taut-faces)

(defface taut-message-huddle-border
  '((((background dark))  :foreground "#2b6cb0")
    (((background light)) :foreground "#90cdf4")
    (t                    :foreground "#90cdf4"))
  "Face for the border characters of Slack Huddle inline summary boxes."
  :group 'taut-faces)

;;;; Buffer-Local Variables

(defvar-local taut-current-channel-id nil
  "The channel-id represented by this conversation buffer.")

(defvar-local taut-expanded-threads nil
  "List of thread-ts currently expanded inline in this buffer.")

(defvar-local taut-message-fetching-p nil
  "Non-nil if currently fetching older history for infinite scroll.")

(defvar-local taut-message-no-more-history-p nil
  "Non-nil if there is no more older history to fetch for this channel.")

(defcustom taut-code-block-language-alist
  '(("elisp" . emacs-lisp)
    ("emacs-lisp" . emacs-lisp)
    ("python" . python)
    ("js" . javascript)
    ("javascript" . javascript)
    ("ts" . typescript)
    ("typescript" . typescript)
    ("html" . html)
    ("css" . css)
    ("bash" . sh)
    ("sh" . sh)
    ("shell" . sh)
    ("ruby" . ruby)
    ("go" . go)
    ("rust" . rust)
    ("elixir" . elixir)
    ("ex" . elixir)
    ("clojure" . clojure)
    ("clj" . clojure)
    ("sql" . sql)
    ("yaml" . yaml)
    ("yml" . yaml)
    ("json" . js)
    ("xml" . xml)
    ("markdown" . markdown)
    ("md" . markdown))
  "Alist mapping code block language specifiers to their Emacs major mode base symbols."
  :type '(repeat (cons (string :tag "Language Specifier")
                       (symbol :tag "Major Mode Base Symbol")))
  :group 'taut)
 
(defcustom taut-code-block-max-lines 10
  "Maximum number of lines to display in a code block before truncating."
  :type 'integer
  :group 'taut)

(defun taut-message--valid-lang-p (str)
  "Return t if STR is a valid programming language identifier."
  (and str
       (not (string-blank-p str))
       (or (assoc-string str taut-code-block-language-alist t)
           (and (not (string-match-p "[ \t]" str))
                (<= (length str) 15)
                (string-match-p "^[a-zA-Z0-9+#_.-]+$" str)))))

(defvar taut-code-block-map (make-sparse-keymap)
  "Keymap active inside code blocks.")

(define-key taut-code-block-map (kbd "c") #'taut-code-block-copy)
(define-key taut-code-block-map (kbd "v") #'taut-code-block-view)
(define-key taut-code-block-map (kbd "s") #'taut-code-block-save)
(define-key taut-code-block-map (kbd "n") #'taut-code-block-toggle-line-numbers)
(define-key taut-code-block-map (kbd "e") #'taut-code-block-evaluate)
(define-key taut-code-block-map (kbd "E") #'taut-code-block-edit)
(define-key taut-code-block-map (kbd "l") #'taut-code-block-set-language)
(define-key taut-code-block-map (kbd "C-c C-y") #'taut-code-block-copy)
(define-key taut-code-block-map (kbd "C-c C-v") #'taut-code-block-view)
(define-key taut-code-block-map (kbd "C-c C-s") #'taut-code-block-save)
(define-key taut-code-block-map (kbd "C-c C-l") #'taut-code-block-set-language)
(define-key taut-code-block-map (kbd "?") #'taut-code-block-dispatch)

;;;###autoload
(defun taut-code-block-copy ()
  "Copy the raw contents of the code block at point to the kill ring."
  (interactive)
  (let ((code (get-text-property (point) 'taut-code-block-content)))
    (if code
        (progn
          (kill-new code)
          (message "Copied code block contents to clipboard."))
      (message "No code block found at point."))))

;;;###autoload
(defun taut-code-block-scratch-save ()
  "Commit/save the current scratchpad buffer content back to the origin chat message."
  (interactive)
  (let ((new-content (buffer-substring-no-properties (point-min) (point-max)))
        (origin-buf taut-scratch-origin-buffer)
        (ts taut-scratch-message-ts)
        (orig-content taut-scratch-original-content))
    (cond
     ((or (null origin-buf) (null ts))
      (user-error "Scratchpad context is missing. Cannot save back to chat."))
     ((string= new-content orig-content)
      (message "No changes to save."))
     (t
      (let ((msg (taut-model-get-message-by-ts ts)))
        (if (not msg)
            (user-error "Could not find the original message to update.")
          (let* ((old-text (taut-message-text msg))
                 ;; Find first exact match of original content to replace
                 (new-text (if (string-match (regexp-quote orig-content) old-text)
                               (replace-match new-content t t old-text)
                             ;; Fallback if exact matching fails, replace whole text
                             new-content)))
            (setf (taut-message-text msg) new-text)
            ;; Save to SQLite cache if available
            (when (fboundp 'taut-cache-save-message)
              (taut-cache-save-message msg))
            ;; Save to local edits override tables
            (setf (gethash ts taut-local-edits) new-text)
            (when (fboundp 'taut-cache-save-local-edit)
              (taut-cache-save-local-edit ts new-text))
            ;; If it's our own message, update on Slack
            (when (equal (taut-message-user-id msg) taut-current-user-id)
              (taut-api-update-message (taut-message-channel-id msg) ts new-text))
            ;; Update original content local variable to prevent duplicate updates
            (setq-local taut-scratch-original-content new-content)
            ;; Refresh the origin buffer
            (with-current-buffer origin-buf
              (if (eq major-mode 'taut-thread-mode)
                  (taut-thread-refresh)
                (taut-message-refresh)))
            (message "Code block saved back to chat!"))))))))

;;;###autoload
(defun taut-code-block-set-language (new-lang)
  "Assign/change the language of the code block under point to NEW-LANG.
This modification is saved locally and persists across sessions."
  (interactive
   (list (completing-read "Assign language: "
                          (mapcar #'car taut-code-block-language-alist)
                          nil t)))
  (let ((code (get-text-property (point) 'taut-code-block-content))
        (old-lang (get-text-property (point) 'taut-code-block-lang))
        (ts (get-text-property (point) 'taut-message-ts))
        (origin-buf (current-buffer)))
    (if (not code)
        (message "No code block found at point.")
      (let ((msg (taut-model-get-message-by-ts ts)))
        (if (not msg)
            (user-error "Could not find the original message to update.")
          (let* ((old-text (taut-message-text msg))
                 ;; Find the code block within the text.
                 (old-lang-pattern (if (or (null old-lang) (string= old-lang "text") (string-blank-p old-lang))
                                       ""
                                     (regexp-quote old-lang)))
                 (search-regex (concat "```" old-lang-pattern "\r?\n"
                                       (regexp-quote code)
                                       "\r?\n[ \t\r]*```"))
                 (replacement (concat "```" new-lang "\n"
                                      code
                                      "\n```"))
                 (new-text (if (string-match search-regex old-text)
                               (replace-match replacement t t old-text)
                             ;; Fallback: if exact match fails, look for the code itself
                             (if (string-match (regexp-quote code) old-text)
                                 (let* ((code-match-start (match-beginning 0))
                                        (pre-text (substring old-text 0 code-match-start))
                                        (post-text (substring old-text (match-end 0))))
                                   (if (string-match "```[^\n]*\r?\n\\'" pre-text)
                                       (concat (replace-match (concat "```" new-lang "\n") t t pre-text)
                                               code
                                               post-text)
                                     old-text))
                               old-text))))
            (if (string= new-text old-text)
                (message "Could not modify the code block in message text.")
              (setf (taut-message-text msg) new-text)
              ;; Save to SQLite message cache
              (when (fboundp 'taut-cache-save-message)
                (taut-cache-save-message msg))
              ;; Save to local edits cache
              (setf (gethash ts taut-local-edits) new-text)
              (when (fboundp 'taut-cache-save-local-edit)
                (taut-cache-save-local-edit ts new-text))
              ;; If it's our own message, update on Slack
              (when (equal (taut-message-user-id msg) taut-current-user-id)
                (taut-api-update-message (taut-message-channel-id msg) ts new-text))
              ;; Refresh the buffer
              (with-current-buffer origin-buf
                (if (eq major-mode 'taut-thread-mode)
                    (taut-thread-refresh)
                  (taut-message-refresh)))
              (message "Code block language set to '%s'!" new-lang))))))))

;;;###autoload
(defun taut-code-block-view ()
  "Pop open a temporary buffer with the code in its native major-mode.
Edits made in this buffer can be committed back to the chat using \\[taut-code-block-scratch-save] (C-c C-c)."
  (interactive)
  (let ((code (get-text-property (point) 'taut-code-block-content))
        (lang (get-text-property (point) 'taut-code-block-lang))
        (ts (get-text-property (point) 'taut-message-ts))
        (prefix (get-text-property (point) 'wrap-prefix))
        (origin-buf (current-buffer)))
    (if (not code)
        (message "No code block found at point.")
      (let* ((buf-name (format "*Taut Scratch - %s*" (if (string-blank-p (or lang "")) "text" lang)))
             (buf (get-buffer-create buf-name))
             (mode-base (or (and lang (cdr (assoc-string lang taut-code-block-language-alist t)))
                            lang))
             (mode-sym (intern (format "%s-mode" mode-base))))
        (with-current-buffer buf
          (erase-buffer)
          (insert code)
          (let ((buffer-file-name lang))
            (if (fboundp mode-sym)
                (funcall mode-sym)
              (normal-mode)))
          ;; Set buffer-local variables for saving back
          (setq-local taut-scratch-origin-buffer origin-buf)
          (setq-local taut-scratch-message-ts ts)
          (setq-local taut-scratch-original-content code)
          (setq-local taut-scratch-lang lang)
          (setq-local taut-scratch-prefix prefix)
          
          (setq-local header-line-format "📝 Scratchpad Code Block  [C-c C-c to save back to chat, C-c C-k to abort]")
          ;; Bind local keys
          (local-set-key (kbd "C-c C-c") #'taut-code-block-scratch-save)
          (local-set-key (kbd "C-c C-s") #'taut-code-block-scratch-save)
          (local-set-key (kbd "C-c C-k") #'quit-window))
        (pop-to-buffer buf)))))

;;;###autoload
(defun taut-code-block-toggle-line-numbers ()
  "Toggle display of line numbers inside the code block at point."
  (interactive)
  (let* ((pos (point))
         (code (get-text-property pos 'taut-code-block-content))
         (lang (get-text-property pos 'taut-code-block-lang))
         (prefix (or (get-text-property pos 'wrap-prefix) "         "))
         (current-show (get-text-property pos 'taut-code-block-show-line-numbers))
         (new-show (not current-show)))
    (if (not code)
        (message "No code block found at point.")
      (let ((inhibit-read-only t)
            (start (previous-single-property-change (1+ pos) 'taut-code-block-content nil (point-min)))
            (end (next-single-property-change pos 'taut-code-block-content nil (point-max))))
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (taut-message--insert-code-block-rendered lang code prefix new-show)
          (message "Toggled line numbers %s." (if new-show "ON" "OFF")))))))

;;;###autoload
(defun taut-code-block-evaluate ()
  "Securely evaluate the code block under point."
  (interactive)
  (let ((code (get-text-property (point) 'taut-code-block-content))
        (lang (get-text-property (point) 'taut-code-block-lang)))
    (if (not code)
        (message "No code block found at point.")
      (when (y-or-n-p (format "Evaluate this %s code block?" (upcase lang)))
        (let ((mode-base (or (and lang (cdr (assoc-string lang taut-code-block-language-alist t)))
                             lang)))
          (cond
           ;; Emacs Lisp evaluation
           ((member mode-base '("elisp" "emacs-lisp" emacs-lisp))
            (condition-case err
                (let ((result (eval (car (read-from-string (concat "(progn " code "\n)"))))))
                  (message "Eval result: %S" result))
              (error (message "Evaluation error: %s" (error-message-string err)))))
           
           ;; Subprocess execution for shell, python, elixir, etc.
           (t
            (let* ((interpreter (cdr (assoc-string mode-base
                                                  '(("python" . "python3")
                                                    ("elixir" . "elixir")
                                                    ("ruby" . "ruby")
                                                    ("js" . "node")
                                                    ("javascript" . "node")
                                                    ("ts" . "ts-node")
                                                    ("sh" . "bash")
                                                    ("bash" . "bash")
                                                    ("shell" . "bash"))
                                                  t)))
                   (cmd (or interpreter (and (stringp mode-base) mode-base) "bash")))
              (message "Executing code block with %s..." cmd)
              (let ((buf (get-buffer-create "*Taut Code Output*")))
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (special-mode)
                    (local-set-key (kbd "q") nil)
                    (insert (format "=== Execution of %s block ===\n\n" (upcase lang)))))
                (pop-to-buffer buf)
                (let ((process-connection-type nil)) ; use pipe
                  (let ((proc (start-process "taut-eval" buf cmd)))
                    (process-send-string proc code)
                    (process-send-eof proc))))))))))))

;;;###autoload
(defalias 'taut-code-block-edit #'taut-code-block-view)

;;;###autoload
(defun taut-code-block-save (filename)
  "Save the raw contents of the code block at point to FILENAME."
  (interactive
   (let* ((lang (get-text-property (point) 'taut-code-block-lang))
          (ext (or (and lang (cdr (assoc-string lang
                                                '(("elisp" . "el")
                                                  ("emacs-lisp" . "el")
                                                  ("python" . "py")
                                                  ("elixir" . "ex")
                                                  ("ex" . "ex")
                                                  ("ruby" . "rb")
                                                  ("js" . "js")
                                                  ("javascript" . "js")
                                                  ("ts" . "ts")
                                                  ("typescript" . "ts")
                                                  ("sh" . "sh")
                                                  ("bash" . "sh")
                                                  ("html" . "html")
                                                  ("css" . "css")
                                                  ("rust" . "rs")
                                                  ("go" . "go"))
                                                t)))
                   "txt"))
          (default-name (format "snippet.%s" ext)))
     (list (read-file-name "Save code block as: " nil nil nil default-name))))
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

(defun taut-message-copy-reference ()
  "Copy a web reference URL for the message at point to the kill-ring and the Taut reference ring."
  (interactive)
  (let* ((ts (get-text-property (point) 'taut-message-ts))
         (msg (and ts (taut-model-get-message-by-ts ts))))
    (if (null msg)
        (message "Taut: No message found under cursor.")
      (let* ((chan-id (taut-message-channel-id msg))
             (chan (taut-model-get-channel chan-id))
             (chan-name (if chan (or (taut-channel-name chan) "unknown") "unknown"))
             (author-id (taut-message-user-id msg))
             (author-user (and author-id (taut-model-get-user author-id)))
             (author-name (if author-user (or (taut-user-username author-user) author-id) "unknown"))
             (root-ts (taut-message-thread-ts msg))
             (actual-thread-ts (and root-ts (not (equal root-ts ts)) root-ts))
             (url (taut-message-get-url chan-id ts actual-thread-ts))
             (snippet (let ((text (or (taut-message-text msg) "")))
                        (if (> (length text) 50)
                            (concat (substring text 0 47) "...")
                          text)))
             (ref (list :channel-id chan-id
                        :channel-name chan-name
                        :ts ts
                        :author author-name
                        :snippet snippet
                        :url url)))
        (setq taut-message-reference-ring (cons ref taut-message-reference-ring))
        (when (> (length taut-message-reference-ring) taut-message-reference-ring-max)
          (setq taut-message-reference-ring (cl-subseq taut-message-reference-ring 0 taut-message-reference-ring-max)))
        (kill-new url)
        (message "Copied reference to @%s's message in #%s!" author-name chan-name)))))

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
(define-key taut-message-mode-map (kbd "w") #'taut-message-copy-reference)
(define-key taut-message-mode-map (kbd "u") #'taut-message-upload-file)
(define-key taut-message-mode-map (kbd "d") #'taut-message-delete)
(define-key taut-message-mode-map (kbd "M") #'taut-message-mark-all-read)
(define-key taut-message-mode-map (kbd "H") #'taut-huddle-join)
(define-key taut-message-mode-map (kbd "?") #'taut-dispatch)
(define-key taut-message-mode-map (kbd "/") #'taut-search-quick)

(define-derived-mode taut-message-mode special-mode "Taut-Chat"
  "Major mode for a Taut Slack conversation buffer.

\\{taut-message-mode-map}"
  (setq buffer-read-only t
        word-wrap t
        wrap-prefix "         ") ; Align wrapped text under usernames nicely (9 spaces)
  (setq-local view-read-only nil)
  (when (and (boundp 'view-mode) view-mode)
    (view-mode -1))
  (visual-line-mode 1)
  (add-hook 'post-command-hook #'taut-message--scroll-handler nil t))

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

(defun taut-message-mark-all-read ()
  "Mark all messages in the current channel as read."
  (interactive)
  (if taut-current-channel-id
      (progn
        (taut-model-mark-channel-read taut-current-channel-id)
        (message "Marked all messages in channel as read."))
    (message "No active channel for this buffer.")))

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

(defun taut-message--scroll-handler ()
  "Check scroll position and trigger infinite scroll if near the top."
  (when (and (eq major-mode 'taut-message-mode)
             taut-current-channel-id
             (not taut-message-fetching-p)
             (not taut-message-no-more-history-p)
             (boundp 'taut-bot-token)
             taut-bot-token)
    ;; To avoid false triggers during initial buffer opening (where window-start
    ;; is temporarily 1 before redisplay scrolls to the bottom), we verify
    ;; that the window-start is actually visible and point is not at the very
    ;; end of a large buffer.
    (when (and (<= (window-start) 300)
               (or (< (point-max) 1000)
                   (< (point) (- (point-max) 100))))
      (taut-message--fetch-older-history))))

(defun taut-message--fetch-older-history ()
  "Fetch older messages from API for the current channel."
  (let ((msgs (taut-model-get-messages taut-current-channel-id)))
    (when msgs
      (let* ((oldest-msg (car msgs))
             (oldest-ts (taut-message-ts oldest-msg)))
        (setq taut-message-fetching-p t)
        (message "Taut: Loading older messages...")
        (unwind-protect
            (let* ((raw-msgs (condition-case err
                                 (taut-api-fetch-history
                                  taut-current-channel-id 40 oldest-ts)
                               (error
                                (message "Taut: Error loading older messages: %s"
                                         (error-message-string err))
                                nil)))
                   (new-count (length raw-msgs)))
              (if (or (not raw-msgs) (<= new-count 1))
                  (progn
                    (setq taut-message-no-more-history-p t)
                    (message "Taut: Reached start of conversation history."))
                (message "Taut: Loaded %d older messages." (1- new-count))
                (taut-message-refresh)))
          (setq taut-message-fetching-p nil))))))

(defun taut-message--render-history (chan-id)
  "Render message list for CHAN-ID."
  (let* ((chan (taut-model-get-channel chan-id))
         (chan-type (if chan (taut-channel-type chan) 'public))
         (chan-name (if chan (taut-channel-name chan) chan-id))
         (chan-topic (if chan (taut-channel-topic chan) "(no topic set)"))
         (msgs (taut-model-get-messages chan-id)))
    ;; Set buffer-local header-line-format for an anchored premium header
    (setq-local header-line-format
                (concat
                 (propertize (if (eq chan-type 'dm)
                                 (format " 👤 @%s" chan-name)
                               (format " ♯ %s" chan-name))
                             'face '(:weight bold :foreground "#36c5f0"))
                 (propertize (format "  |  %s" (or chan-topic "(no topic set)"))
                             'face 'font-lock-comment-face)))

    (insert "\n")

    (if (null msgs)
        (insert "\n\n  No messages in this conversation yet. Send a message with `r`!\n")
      (dolist (msg msgs)
        (taut-message--render-message-line msg)))
    (force-mode-line-update t)))

(defun taut-message--huddle-message-p (text)
  "Return non-nil if TEXT represents a Slack Huddle message."
  (and text (string-match-p "📞 Slack Huddle" text)))

(defun taut-message--insert-huddle-box (text prefix)
  "Insert a beautifully styled inline huddle box for TEXT with PREFIX indentation."
  (let* ((has-ended (string-match-p "Ended" text))
         (in-progress (string-match-p "in progress" text))
         (title (cond
                 (has-ended "Slack Huddle (Ended)")
                 (in-progress "Slack Huddle (Active)")
                 (t "Slack Huddle")))
         (details (let ((clean (replace-regexp-in-string "📞 Slack Huddle" "" text)))
                    (setq clean (replace-regexp-in-string " ?(Ended)" "" clean))
                    (setq clean (replace-regexp-in-string " ?(Active)" "" clean))
                    (setq clean (replace-regexp-in-string " ?in progress" "" clean))
                    (setq clean (string-trim clean))
                    (if (string-prefix-p ":" clean)
                        (string-trim (substring clean 1))
                      clean)))
         (icon (if in-progress "🎧" "📞"))
         (border-face 'taut-message-huddle-border)
         (width 60)
         (content-width (- width 4)))
    ;; Print top border
    (insert (propertize "╭" 'face border-face)
            (propertize (make-string (- width 2) ?─) 'face border-face)
            (propertize "╮\n" 'face border-face))
    ;; Title line
    (let* ((title-text (format "%s  %s" icon title))
           (padding (- content-width (string-width title-text))))
      (insert prefix
              (propertize "│ " 'face border-face)
              (propertize title-text 'face 'bold)
              (propertize (make-string (max 0 padding) ? ) 'face 'default)
              (propertize " │\n" 'face border-face)))
    ;; Details line
    (unless (string-blank-p details)
      (let* ((details-text (format "    %s" details))
             (truncated-details (truncate-string-to-width details-text content-width nil nil "..."))
             (padding (- content-width (string-width truncated-details))))
        (insert prefix
                (propertize "│ " 'face border-face)
                (propertize truncated-details 'face 'font-lock-comment-face)
                (propertize (make-string (max 0 padding) ? ) 'face 'default)
                (propertize " │\n" 'face border-face))))
    ;; Print bottom border
    (insert prefix
            (propertize "╰" 'face border-face)
            (propertize (make-string (- width 2) ?─) 'face border-face)
            (propertize "╯" 'face border-face))))

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
    (if (taut-message--huddle-message-p (taut-message-text msg))
        (taut-message--insert-huddle-box (taut-message-text msg) "         ")
      (taut-message--insert-formatted-text (taut-message-text msg) "         "))
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
                              'help-echo (taut-message--format-reaction-tooltip reactors)
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
               (label (propertize (format "💬 %s %d %s " icon reply-count (if (= reply-count 1) "reply" "replies"))
                                  'face 'taut-message-thread-link))
               (full-line (concat "         " label)))
          (insert (propertize full-line
                              'mouse-face 'highlight
                              'keymap taut-message-thread-button-map
                              'taut-thread-ts ts
                              'taut-message-ts ts
                              'taut-message-id (taut-message-id msg))
                  (propertize "\n"
                              'keymap taut-message-thread-button-map
                              'taut-thread-ts ts
                              'taut-message-ts ts
                              'taut-message-id (taut-message-id msg)))
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
                              'help-echo (taut-message--format-reaction-tooltip reactors)
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
Returns a string of `Weekday Month Day, Year, HH:MM:SS'."
  (if (and ts-str (string-match "^\\([0-9]+\\)" ts-str))
      (let* ((epoch (string-to-number (match-string 1 ts-str)))
             (time-val (seconds-to-time epoch)))
        (replace-regexp-in-string "  " " " (format-time-string "%A %B %e, %Y, %H:%M:%S" time-val)))
    "--:--:--"))

(defun taut-message--format-reaction-tooltip (user-ids)
  "Format a list of USER-IDS into a hover tooltip string.
This lists usernames of users who reacted."
  (let ((names (mapcar (lambda (uid)
                         (let ((user (taut-model-get-user uid)))
                           (concat "@" (or (taut-user-username user) uid "unknown"))))
                       user-ids)))
    (concat "Reacted by: " (mapconcat #'identity names ", "))))

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
Allows both raw shortcode names and bracketed format like \":raised_hands:\".
If the shortcode is not in `taut-emoji-alist`, attempts to dynamically resolve
it using Emacs's built-in `char-from-name` by substituting underscores with
spaces and converting to uppercase."
  (let* ((name (cond
                ((symbolp name) (symbol-name name))
                ((stringp name) name)
                (t "")))
         (clean-name (if (and (string-prefix-p ":" name) (string-suffix-p ":" name))
                         (substring name 1 -1)
                       name))
         (match (assoc clean-name taut-emoji-alist)))
    (cond
     (match (cdr match))
     ;; Dynamic fallback using char-from-name
     ((let* ((unicode-name (upcase (replace-regexp-in-string "_" " " clean-name)))
             (char (char-from-name unicode-name)))
        (and char (string char))))
     ;; Default fallback: return the original shortcode with colons intact
     (t
      (if (and (string-prefix-p ":" name) (string-suffix-p ":" name))
          name
        (concat ":" name ":"))))))

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

(defun taut-message--fontify-string (code lang)
  "Return CODE string fontified as LANG."
  (let* ((mode-base (or (and lang (cdr (assoc-string lang taut-code-block-language-alist t)))
                        lang))
         (mode-sym (and mode-base (intern (format "%s-mode" mode-base)))))
    (with-temp-buffer
      (insert code)
      (condition-case nil
          (if (and mode-sym (fboundp mode-sym))
              (funcall mode-sym)
            (normal-mode))
        (error (normal-mode)))
      (ignore-errors (font-lock-ensure))
      (buffer-string))))

(defun taut-message--insert-code-block-rendered (lang code prefix &optional show-line-numbers)
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
    (insert prefix "│  " (propertize (format "💻 CODE (%s) - [? for options]" (if (string-blank-p lang) "text" (upcase lang))) 'face '(:weight bold :foreground "#8a8a8a")) "\n")
    (insert prefix "├" border-line "\n")
    
    ;; Insert code content with prefix on each line, limited by taut-code-block-max-lines
    (let* ((fontified-code (taut-message--fontify-string code lang))
           (lines (split-string fontified-code "\n"))
           (total-count (length lines))
           (max-lines (if show-line-numbers total-count taut-code-block-max-lines))
           (show-lines (if (> total-count max-lines)
                           (butlast lines (- total-count max-lines))
                         lines))
           (hidden-count (- total-count max-lines))
           (idx 1))
      (dolist (line show-lines)
        (insert margin-prefix)
        (when show-line-numbers
          (let* ((digit-width (length (number-to-string total-count)))
                 (fmt-str (format "%%%dd │ " digit-width))
                 (num-str (format fmt-str idx)))
            (insert (propertize num-str 'face '(:foreground "#8a8a8a")))))
        (let ((start-line (point)))
          (insert line "\n")
          (add-face-text-property start-line (point) code-face t))
        (setq idx (1+ idx)))
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
                               'taut-code-block-show-line-numbers show-line-numbers
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
             (raw-lang (string-trim (match-string 1 text)))
             (is-valid-lang (taut-message--valid-lang-p raw-lang))
             (lang (if is-valid-lang raw-lang "text"))
             (code-start (if (or is-valid-lang (string-blank-p raw-lang))
                             match-end
                           (+ match-start 3)))
             (code nil)
             (block-end nil))
        
        ;; Insert preceding normal text
        (let ((pre-text (substring text start match-start)))
          (unless (string-blank-p pre-text)
            (taut-message--insert-formatted-text-normal pre-text prefix)))
        
        ;; Check if this is a file snippet fallback block: ```lang\n```\n<content>
        (if (and is-valid-lang
                 (string-match "\\````\r?\n" (substring text match-end)))
            (let ((content-start (+ match-end (match-end 0))))
              (if (string-match "\r?\n[ \t\r]*```" text content-start)
                  (setq code (substring text content-start (match-beginning 0))
                        block-end (match-end 0))
                (setq code (substring text content-start)
                      block-end len)))
          
          ;; Normal code block: ```lang\n<code>\n```
          (if (string-match "\r?\n[ \t\r]*```" text code-start)
              (setq code (substring text code-start (match-beginning 0))
                    block-end (match-end 0))
            (setq code (substring text code-start)
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

(defun taut-message-open (chan-id &optional other-window)
  "Switch to the conversation buffer for CHAN-ID.
If OTHER-WINDOW is non-nil, open the buffer in another window."
  (taut-ensure-consolidated-workspace)
  (let* ((chan (taut-api-get-or-fetch-channel chan-id))
         (chan-type (if chan (taut-channel-type chan) 'public))
         (chan-name (if chan (taut-channel-name chan) chan-id))
         (buf-name (if (eq chan-type 'dm)
                       (format "*Taut - @%s*" chan-name)
                     (format "*Taut - #%s*" chan-name)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'taut-message-mode)
        (taut-message-mode))
      (setq taut-current-channel-id chan-id
            taut-message-fetching-p nil
            taut-message-no-more-history-p nil)
      (when (and (boundp 'taut-bot-token) taut-bot-token)
        (condition-case err
            (taut-api-fetch-history chan-id)
          (error
           (message "Taut: Failed to fetch history for %s: %s"
                    chan-name
                    (error-message-string err)))))
      (taut-message-refresh))
    
    (if (and (boundp 'taut-strict-windows) taut-strict-windows)
        (let ((chat-win (taut-get-chat-window)))
          (set-window-buffer chat-win buf)
          (select-window chat-win))
      ;; Make sure we don't open inside the Sidebar window
      (let ((sidebar-win (get-buffer-window "*Taut Sidebar*")))
        (cond
         ((and sidebar-win (eq (selected-window) sidebar-win))
          (select-window (next-window sidebar-win))
          (if other-window
              (switch-to-buffer-other-window buf)
            (switch-to-buffer buf)))
         (other-window
          (switch-to-buffer-other-window buf))
         (t
          (switch-to-buffer buf)))))
    
    (goto-char (point-max))
    (redisplay t)
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


;;;###autoload
(defun taut-message-goto-ts (ts)
  "Move point to the message with timestamp TS in the current buffer.
Returns non-nil if found."
  (let ((pos (point-min))
        found)
    (while (and pos (< pos (point-max)) (not found))
      (let ((next-change (next-single-property-change pos 'taut-message-ts)))
        (if (equal (get-text-property pos 'taut-message-ts) ts)
            (progn
              (goto-char pos)
              (setq found t))
          (setq pos next-change))))
    found))

;; Hook auto-updates
(add-hook 'taut-model-updated-hook #'taut-message-refresh-all)

(provide 'taut-message)
;;; taut-message.el ends here

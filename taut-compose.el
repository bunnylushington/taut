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
(declare-function taut-emoji-translate "taut-message")
(declare-function taut-message--insert-formatted-text "taut-message")

(defvar taut-current-thread-ts)
(defvar taut-current-user-id)
(defvar taut-emoticon-alist)

(defgroup taut-compose nil
  "Message composition options for Taut."
  :group 'taut)

(defcustom taut-compose-emoji-list
  '("smile" "thumbsup" "heart" "tada" "fire" "rocket" "eyes" "thinking_face"
    "checkmark" "rolling_on_the_floor_laughing" "heavy_check_mark" "x"
    "raised_hands" "pray" "clap" "bow" "muscle" "metal" "star" "gift" "party-parrot"
    "cry" "scream" "laughing" "sob" "cold_sweat" "sweat_smile" "wink" "shrug" "facepalm")
  "A list of common Slack emoji names (without colons) for completion."
  :type '(list string)
  :group 'taut-compose)

(defcustom taut-compose-markdown-p t
  "Toggle automatic translation of standard Markdown to Slack's custom `mrkdwn' syntax.
When non-nil, Markdown elements (like **bold**, _italics_, [links](url), bullet lists,
and headings) are parsed and translated before sending or rendering in live preview."
  :type 'boolean
  :group 'taut-compose)

(make-variable-buffer-local 'taut-compose-markdown-p)

;;;; Buffer-Local Variables

(defvar-local taut-compose-channel-id nil
  "The target channel ID for this draft.")

(defvar-local taut-compose-thread-ts nil
  "The target thread timestamp for this draft, or nil for main channel.")

(defvar-local taut-compose-edit-ts nil
  "The timestamp of the message being edited, or nil if posting new.")

(defvar-local taut-compose--preview-timer nil
  "Idle timer for updating the compose preview buffer.")

;;;; Keymap & Major Mode

(defvar taut-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'taut-compose-send)
    (define-key map (kbd "C-c C-k") #'taut-compose-abort)
    (define-key map (kbd "C-c C-b") #'taut-compose-insert-code-block)
    (define-key map (kbd "C-c C-s") #'taut-compose-insert-shell-steps-skeleton)
    (define-key map (kbd "C-c C-a") #'taut-compose-from-atuin-history)
    (define-key map (kbd "C-c C-l") #'taut-compose-insert-link)
    (define-key map (kbd "C-c C-y") #'taut-compose-insert-reference)
    (define-key map (kbd "C-c C-u") #'taut-compose-insert-user-mention)
    (define-key map (kbd "C-c @") #'taut-compose-insert-user-mention)
    (define-key map (kbd "C-c C-m") #'taut-compose-dispatch)
    (define-key map (kbd "C-c C-p") #'taut-compose-toggle-preview)
    (define-key map (kbd "C-c C-t") #'taut-compose-toggle-markdown)
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
                        (format " %s %s %s in %s%s  [C-c C-c to send, C-c C-k to abort, C-c C-m for helper]"
                                (if is-edit "✏️" (if is-thread "🧵" "💬"))
                                (if is-edit "Editing" "Composing")
                                (if is-edit "message" (if is-thread "thread reply" "message"))
                                prefix
                                name))))
  (setq word-wrap t)
  (visual-line-mode 1)
  (add-hook 'post-self-insert-hook #'taut-compose--post-self-insert nil t)
  ;; Trigger preview update asynchronously as the user moves point/types
  (add-hook 'post-command-hook #'taut-compose--post-command-preview-trigger nil t)
  ;; Ensure timer is cleanly cancelled when buffer dies
  (add-hook 'kill-buffer-hook #'taut-compose--cleanup-preview-timer nil t)
  ;; Register our custom Completion-At-Point Function (Capf)
  (add-hook 'completion-at-point-functions #'taut-compose-capf nil t)
  ;; Ensure Corfu manages completions if present
  (when (fboundp 'corfu-mode)
    (corfu-mode 1))
  ;; Enable spelling checking if flyspell-mode is available
  (when (fboundp 'flyspell-mode)
    (flyspell-mode 1)))

(defun taut-compose--post-self-insert ()
  "Translate emoticons to emojis.
Trigger completion for @, #, and : as the user types."
  (let ((pos (point))
        (last-char (char-before)))
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
                              (insert found)))))))))
    ;; Trigger autocomplete dynamically on prefix characters
    (when (memq last-char '(?@ ?# ?:))
      (completion-at-point))))

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

(defun taut-compose--get-text-with-markup ()
  "Retrieve the buffer contents.
Replace displayed mentions/channels with their underlying Slack markup stored
in the `taut-compose-markup' property."
  (let ((chunks nil)
        (pos (point-min))
        next-pos)
    (while (< pos (point-max))
      (setq next-pos (next-single-property-change pos 'taut-compose-markup nil (point-max)))
      (let ((markup (get-text-property pos 'taut-compose-markup)))
        (if markup
            (push markup chunks)
          (push (buffer-substring-no-properties pos next-pos) chunks)))
      (setq pos next-pos))
    (apply #'concat (nreverse chunks))))

;;;###autoload
(defun taut-compose-send ()
  "Send the composed message to Slack or update an existing message."
  (interactive)
  ;; Translate any remaining emoticons (e.g. pasted or typed fast) before sending
  (let* ((text (taut-compose--get-text-with-markup))
         (translated-text (taut-emoticon-translate-string text))
         (final-text (if taut-compose-markdown-p
                         (taut-compose-markdown-to-mrkdwn translated-text)
                       translated-text))
         (chan-id taut-compose-channel-id)
         (thread-ts taut-compose-thread-ts)
         (edit-ts taut-compose-edit-ts))
    (if (string-blank-p final-text)
        (message "Cannot send an empty message.")
      ;; Post or update the message!
      (if (and (boundp 'taut-bot-token) taut-bot-token)
          (if edit-ts
              (taut-api-update-message chan-id edit-ts final-text)
            (taut-api-post-message chan-id final-text thread-ts))
        ;; Fallback to offline/mock
        (if edit-ts
            (let ((m (taut-model-get-message-by-ts edit-ts)))
              (when m
                (setf (taut-message-text m) final-text)
                (taut-model-trigger-update)))
          (let* ((ts (format "%d.0000" (time-convert nil 'integer)))
                 (is-mention (string-match-p (regexp-quote (format "<@%s>" taut-current-user-id)) final-text)))
            (taut-model-add-message
             (make-taut-message
              :id (concat "msg_" ts)
              :channel-id chan-id
              :user-id taut-current-user-id
              :text final-text
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
  (let ((win (get-buffer-window "*Taut Compose*"))
        (p-win (get-buffer-window "*Taut Compose Preview*")))
    (when win
      (delete-window win))
    (when p-win
      (delete-window p-win))
    (kill-buffer "*Taut Compose*")
    (when (get-buffer "*Taut Compose Preview*")
      (kill-buffer "*Taut Compose Preview*"))))

;;;; Formatting Helpers

;;;###autoload
(defun taut-compose-insert-code-block (lang)
  "Insert a Slack code block for LANG."
  (interactive "sLanguage (e.g. python, elisp): ")
  (let ((start (point)))
    (insert "```" lang "\n\n```")
    (goto-char (+ start 3 (length lang) 1))))

;;;###autoload
(defun taut-compose-insert-shell-steps-skeleton ()
  "Insert the skeleton of a shell steps block into the compose buffer.
The point lands right after the `# @taut-runnable` decoration on a new line,
ready to write the first command."
  (interactive)
  (insert "```bash\n# @taut-runnable\n")
  (let ((cmd-pos (point)))
    (insert "\n```\n")
    (goto-char cmd-pos)))

;;;###autoload
(defun taut-compose-from-atuin-history ()
  "Interactively select shell commands from Atuin history.
Insert them as a runnable code block."
  (interactive)
  (let* ((atuin-bin (or (executable-find "atuin") "/opt/homebrew/bin/atuin"))
         (history-cmd (and (file-executable-p atuin-bin)
                           (format "%s search --limit 500 --format \"{command}\"" atuin-bin))))
    (if (not history-cmd)
        (user-error "Taut: Atuin binary not found")
      (let* ((session-id (or (getenv "ATUIN_SESSION")
                             (secure-hash 'md5 (format "%s%s%s" (random) (current-time) (user-uid)))))
             (process-environment (cons (format "ATUIN_SESSION=%s" session-id)
                                        process-environment))
             (history-str (shell-command-to-string history-cmd))
             (candidates (delete-dups (nreverse (split-string history-str "\n" t))))
             (start-pos (point))
             (insert-marker nil)
             (selected-count 0)
             (done nil))
        ;; Pre-insert code block skeleton and place marker inside
        (insert "```bash\n# @taut-runnable\n\n```\n")
        (setq insert-marker (copy-marker (+ start-pos (length "```bash\n# @taut-runnable\n"))))
        (unwind-protect
            ;; Loop prompting for commands
            (while (not done)
              (let* ((prompt (if (> selected-count 0)
                                 (format "Select command [%d selected] (RET to finish): " selected-count)
                               "Select command from Atuin (RET to cancel): "))
                     (choice (completing-read prompt
                                              (lambda (string pred action)
                                                (if (eq action 'metadata)
                                                    '(metadata (display-sort-function . identity)
                                                               (cycle-sort-function . identity))
                                                  (complete-with-action action candidates string pred)))
                                              nil nil)))
                (if (string-empty-p choice)
                    (setq done t)
                  (save-excursion
                    (goto-char insert-marker)
                    (insert choice "\n")
                    (set-marker insert-marker (point)))
                  (setq selected-count (1+ selected-count))
                  (redisplay t))))
          ;; Cleanup and finalization
          (if (or (not done) (= selected-count 0))
              (delete-region start-pos (point))
            (save-excursion
              (goto-char insert-marker)
              (when (eq (char-after) ?\n)
                (delete-char 1))))
          (set-marker insert-marker nil))))))

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
          (let ((disp (format "@%s" (or username uid))))
            (insert (propertize disp
                                'face 'taut-message-mention
                                'taut-compose-markup (format "<@%s|%s>" uid (or username uid))
                                'rear-nonsticky t))))))))

;;;###autoload
(defun taut-compose-insert-reference ()
  "Insert a Slack message reference from the Taut reference ring."
  (interactive)
  (if (null taut-message-reference-ring)
      (message "Taut: Reference ring is empty. Copy a reference with 'w' in a chat buffer first.")
    (let ((candidates nil))
      (dolist (ref taut-message-reference-ring)
        (let* ((channel (plist-get ref :channel-name))
               (author (plist-get ref :author))
               (snippet (plist-get ref :snippet))
               (url (plist-get ref :url))
               (display (format "[#%s] @%s: %s" channel author snippet)))
          (unless (assoc display candidates)
            (push (cons display url) candidates))))
      (setq candidates (nreverse candidates))
      (let* ((choice (completing-read "Insert Message Reference: " candidates nil t))
             (url (cdr (assoc choice candidates))))
        (when url
          (insert url))))))

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

;;;; Capf Core Engine

(defun taut-compose--capf-bounds ()
  "Scan backward from point to find a valid completion prefix.
Returns a plist with keys :type, :start, and :end if a valid prefix
is found, otherwise nil."
  (save-excursion
    (let ((limit (line-beginning-position))
          (pos (point))
          found)
      ;; Search backward for @, #, or :
      (while (and (> pos limit) (not found))
        (setq pos (1- pos))
        (let ((char (char-after pos)))
          (cond
           ;; Check if it's one of our triggers
           ((memq char '(?@ ?# ?:))
            ;; Ensure it is preceded by beginning-of-line, whitespace, or punctuation
            (let ((pre-char (if (= pos limit) nil (char-before pos))))
              (if (or (null pre-char)
                      (memq pre-char '(?\s ?\t ?\n ?\( ?\[ ?\{ ?\" ?\' ?< ?, ?. ?? ?! ?\; ?: ?-)))
                  ;; Ensure no whitespace exists between trigger and point
                  (let ((text-between (buffer-substring-no-properties (1+ pos) (point))))
                    (unless (string-match-p "[[:space:]]" text-between)
                      (setq found
                            (list :type (cond
                                         ((eq char ?@) 'user)
                                         ((eq char ?#) 'channel)
                                         ((eq char ?:) 'emoji))
                                  :start pos
                                  :end (point)))))))))))
      found)))

(defun taut-compose-capf ()
  "Completion at point function for Taut message composer."
  (when-let ((bounds (taut-compose--capf-bounds)))
    (let* ((type (plist-get bounds :type))
           (start (plist-get bounds :start))
           (end (plist-get bounds :end))
           (collection
            (cond
             ((eq type 'user)
              (let (candidates)
                (maphash (lambda (_id u)
                           (push (concat "@" (taut-user-username u)) candidates))
                         taut-users)
                candidates))
             ((eq type 'channel)
              (let (candidates)
                (maphash (lambda (_id c)
                           (unless (eq (taut-channel-type c) 'dm)
                             (push (concat "#" (taut-channel-name c)) candidates)))
                         taut-channels)
                candidates))
             ((eq type 'emoji)
              (let (custom-list)
                (maphash (lambda (name _)
                           (push name custom-list))
                         taut-custom-emojis)
                (mapcar (lambda (e) (format ":%s:" e))
                        (append taut-compose-emoji-list (sort custom-list #'string<))))))))
      
      (list start end collection
            :exit-function
            (lambda (str status)
              (when (memq status '(finished sole exact))
                (let ((end-pos nil))
                  (save-excursion
                    (goto-char start)
                    (when (looking-at (regexp-quote str))
                      (let ((inhibit-read-only t))
                        (delete-region start (match-end 0))
                        (cond
                         ((eq type 'user)
                          (let* ((username (substring str 1))
                                 (uid nil))
                            (maphash (lambda (id u)
                                       (when (equal (taut-user-username u) username)
                                         (setq uid id)))
                                     taut-users)
                            (if uid
                                (insert (propertize str
                                                    'face 'taut-message-mention
                                                    'taut-compose-markup (format "<@%s|%s>" uid username)
                                                    'rear-nonsticky t))
                              (insert str))))
                         ((eq type 'channel)
                          (let* ((chan-name (substring str 1))
                                 (cid nil))
                            (maphash (lambda (id c)
                                       (when (equal (taut-channel-name c) chan-name)
                                         (setq cid id)))
                                     taut-channels)
                            (if cid
                                (insert (propertize str
                                                    'face 'taut-message-mention
                                                    'taut-compose-markup (format "<#%s|%s>" cid chan-name)
                                                    'rear-nonsticky t))
                              (insert str))))
                         ((eq type 'emoji)
                          (insert str)))
                        (setq end-pos (point)))))
                  (when end-pos
                    (goto-char end-pos)))))
            :annotation-function
            (lambda (cand)
              (cond
               ((eq type 'user)
                (let* ((username (substring cand 1))
                       (user (cl-loop for u being hash-values of taut-users
                                      when (equal (taut-user-username u) username)
                                      return u)))
                  (if (and user (taut-user-real-name user))
                      (format "  (%s)" (taut-user-real-name user))
                    "")))
               ((eq type 'channel)
                (let* ((chan-name (substring cand 1))
                       (chan (cl-loop for c being hash-values of taut-channels
                                      when (equal (taut-channel-name c) chan-name)
                                      return c)))
                  (if (and chan (taut-channel-topic chan))
                      (format "  [%s]" (taut-channel-topic chan))
                    "")))
               ((eq type 'emoji)
                (let* ((emoji-name (substring cand 1 (1- (length cand))))
                       (custom-url (gethash emoji-name taut-custom-emojis))
                       (emoji-char (unless custom-url (taut-emoji-translate emoji-name))))
                  (cond
                   (custom-url "  [custom]")
                   (emoji-char (format "  %s" emoji-char))
                   (t ""))))))))))

;;;; =========================================================================
;;;; 📝 Markdown Translation & Live Preview Engine
;;;; =========================================================================

;;;###autoload
(defun taut-compose-markdown-to-mrkdwn (text)
  "Translate standard Markdown in TEXT to Slack's custom `mrkdwn' syntax."
  (if (string-blank-p text)
      ""
    (with-temp-buffer
      (insert text)
      
      ;; 1. Protect triple-backtick code blocks
      (goto-char (point-min))
      (while (re-search-forward "```[a-zA-Z0-9-]*\\(?:\n\\|.\\)*?```" nil t)
        (put-text-property (match-beginning 0) (match-end 0) 'taut-protected t))
        
      ;; 2. Protect inline backtick code blocks
      (goto-char (point-min))
      (while (re-search-forward "`[^`\n]+`" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0)))
          (unless (get-text-property start 'taut-protected)
            (put-text-property start end 'taut-protected t))))
            
      ;; 3. Handle Block-level elements line-by-line (headings, blockquotes, bullet lists)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (unless (get-text-property start 'taut-protected)
            (let ((line (buffer-substring-no-properties start end)))
              ;; A. Headings: "# heading" -> "*heading*"
              (cond
               ((string-match "^\\([ \t]*\\)\\(#+\\)[ \t]+\\([^\n]+\\)$" line)
                (let ((indent (match-string 1 line))
                      (heading (match-string 3 line)))
                  (delete-region start end)
                  (insert (format "%s*%s*" indent heading))
                  (put-text-property start (point) 'taut-protected t)))
                  
               ;; B. Blockquotes: "> text" -> "> text"
               ((string-match "^\\([ \t]*\\)>[ \t]*\\([^\n]+\\)$" line)
                (let ((indent (match-string 1 line))
                      (content (match-string 2 line)))
                  (delete-region start end)
                  (insert (format "%s> %s" indent content))
                  (put-text-property start (point) 'taut-protected t)))
                  
               ;; C. Bullet lists: "- item" or "* item" -> "• item"
               ((string-match "^\\([ \t]*\\)\\([-*]\\)[ \t]+\\([^\n]+\\)$" line)
                (let ((indent (match-string 1 line))
                      (content (match-string 3 line)))
                  (delete-region start end)
                  (insert (format "%s• %s" indent content))
                  (put-text-property start (point) 'taut-protected t)))))))
        (forward-line 1))
        
      ;; 4. Handle Inline elements (global search skipping protected text)
      
      ;; 4.1: Links [label](url) -> <url|label>
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([^]]+?\\)\\](\\([^)\n]+?\\))" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (label (match-string 1))
              (url (match-string 2)))
          (unless (get-text-property start 'taut-protected)
            (delete-region start end)
            (insert (format "<%s|%s>" url label))
            (put-text-property start (point) 'taut-protected t)
            (goto-char (point-min)))))
            
      ;; 4.2: Strikethrough ~~text~~ -> ~text~
      (goto-char (point-min))
      (while (re-search-forward "~~\\([^\n~]+?\\)~~" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (content (match-string 1)))
          (unless (get-text-property start 'taut-protected)
            (delete-region start end)
            (insert (format "~%s~" content))
            (put-text-property start (point) 'taut-protected t)
            (goto-char (point-min)))))
            
      ;; 4.3: Bold **text** -> *text*
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\([^\n*]+?\\)\\*\\*" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (content (match-string 1)))
          (unless (get-text-property start 'taut-protected)
            (delete-region start end)
            (insert (format "*%s*" content))
            (put-text-property start (point) 'taut-protected t)
            (goto-char (point-min)))))
            
      ;; 4.4: Bold __text__ -> *text*
      (goto-char (point-min))
      (while (re-search-forward "__\\([^\n_]+?\\)__" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (content (match-string 1)))
          (unless (get-text-property start 'taut-protected)
            (delete-region start end)
            (insert (format "*%s*" content))
            (put-text-property start (point) 'taut-protected t)
            (goto-char (point-min)))))
            
      ;; 4.5: Italics *text* -> _text_
      (goto-char (point-min))
      (while (re-search-forward "\\*\\([^\n*]+?\\)\\*" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (content (match-string 1)))
          (unless (get-text-property start 'taut-protected)
            (delete-region start end)
            (insert (format "_%s_" content))
            (put-text-property start (point) 'taut-protected t)
            (goto-char (point-min)))))
            
      ;; 4.6: Italics _text_ -> _text_
      (goto-char (point-min))
      (while (re-search-forward "_\\([^\n_]+?\\)_" nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (content (match-string 1)))
          (unless (get-text-property start 'taut-protected)
            (put-text-property start end 'taut-protected t)
            (goto-char (point-min)))))

      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun taut-compose-toggle-markdown ()
  "Toggle standard Markdown to Slack `mrkdwn' translation."
  (interactive)
  (setq taut-compose-markdown-p (not taut-compose-markdown-p))
  (message "Taut Markdown translation: %s" (if taut-compose-markdown-p "ENABLED" "DISABLED"))
  (when (get-buffer-window "*Taut Compose Preview*")
    (taut-compose-preview-update)))

;;;###autoload
(defun taut-compose-preview-update ()
  "Force-update the active Taut compose preview buffer."
  (interactive)
  (taut-compose-preview-update-from-composer (current-buffer)))

(defun taut-compose-preview-update-from-composer (composer-buf)
  "Update the preview buffer using text from COMPOSER-BUF."
  (when (buffer-live-p composer-buf)
    (let ((text (with-current-buffer composer-buf
                  (taut-compose--get-text-with-markup)))
          (markdown-p (with-current-buffer composer-buf
                        taut-compose-markdown-p)))
      (let ((translated (if markdown-p
                            (taut-compose-markdown-to-mrkdwn text)
                          text)))
        (with-current-buffer (get-buffer-create "*Taut Compose Preview*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "\n")
            (taut-message--insert-formatted-text translated "  ")
            (set-buffer-modified-p nil)))))))

(defun taut-compose--post-command-preview-trigger ()
  "Trigger live preview update asynchronously."
  (when (get-buffer-window "*Taut Compose Preview*")
    (when taut-compose--preview-timer
      (cancel-timer taut-compose--preview-timer))
    (setq taut-compose--preview-timer
          (run-with-idle-timer 0.15 nil #'taut-compose-preview-update-from-composer (current-buffer)))))

(defun taut-compose--cleanup-preview-timer ()
  "Cancel active preview update timer."
  (when taut-compose--preview-timer
    (cancel-timer taut-compose--preview-timer)
    (setq taut-compose--preview-timer nil)))

;;;###autoload
(defun taut-compose-toggle-preview ()
  "Toggle the live Markdown/mrkdwn preview window for Taut message composition."
  (interactive)
  (let* ((preview-buf-name "*Taut Compose Preview*")
         (preview-win (get-buffer-window preview-buf-name)))
    (if preview-win
        (progn
          (delete-window preview-win)
          (when (get-buffer preview-buf-name)
            (kill-buffer preview-buf-name))
          (message "Taut Live Preview: closed"))
      (let* ((orig-window (selected-window))
             (new-win (split-window-right)))
        (with-selected-window new-win
          (let ((buf (get-buffer-create preview-buf-name)))
            (with-current-buffer buf
              (unless (eq major-mode 'taut-compose-preview-mode)
                (taut-compose-preview-mode)))
            (switch-to-buffer buf)))
        (taut-compose-preview-update)
        (select-window orig-window)
        (message "Taut Live Preview: opened")))))

;;;; =========================================================================
;;;; 👁️ Taut Compose Preview Mode Definition
;;;; =========================================================================

(defvar taut-compose-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") (lambda () (interactive)
                                (let ((win (get-buffer-window "*Taut Compose Preview*")))
                                  (when win (delete-window win))
                                  (kill-buffer "*Taut Compose Preview*"))))
    map)
  "Keymap for `taut-compose-preview-mode'.")

(define-derived-mode taut-compose-preview-mode special-mode "Taut-Preview"
  "Major mode for live previewing composed messages.

\\{taut-compose-preview-mode-map}"
  (setq-local header-line-format " 👁️ Taut Message Live Preview  [C-c C-c in composer to send, q to close]")
  (setq word-wrap t)
  (visual-line-mode 1))

(provide 'taut-compose)
;;; taut-compose.el ends here

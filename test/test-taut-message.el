;;; test-taut-message.el --- Unit tests for taut-message.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; Unit tests for Taut message rendering, formatting, emoticons, and actions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'taut-message)
(require 'taut-model)
(require 'taut-test-fixtures)

(ert-deftest taut-message-format-ts-test ()
  "Test timestamp formatting in `taut-message--format-ts`."
  (should (equal (taut-message--format-ts nil) "--:--:--"))
  (should (equal (taut-message--format-ts "invalid") "--:--:--"))
  (let ((res (taut-message--format-ts "1688450000.0001")))
    ;; Format should match: weekday month day, year, time
    (should (string-match-p "^[A-Za-z]+ [A-Za-z]+ [0-9]+, [0-9]\\{4\\}, [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}$" res))))

(ert-deftest taut-message-format-reaction-tooltip-test ()
  "Test tooltip construction in `taut-message--format-reaction-tooltip`."
  (taut-initialize-mock-data)
  (let ((tooltip (taut-message--format-reaction-tooltip '("U_ALICE" "U_BOB" "U_UNKNOWN"))))
    (should (equal tooltip "Reacted by: @alice, @bob, @user-U_UNKNOWN"))))

(ert-deftest taut-message-emoji-translate-test ()
  "Test emoji translation in `taut-emoji-translate`."
  (should (equal (taut-emoji-translate "thumbsup") "👍"))
  (should (equal (taut-emoji-translate ":thumbsup:") "👍"))
  (should (equal (taut-emoji-translate "rocket") "🚀"))
  (should (equal (taut-emoji-translate ":rocket:") "🚀"))
  (should (equal (taut-emoji-translate "party") "🎉"))
  ;; Dynamic lookup using char-from-name
  (should (equal (taut-emoji-translate "twisted_rightwards_arrows") "🔀"))
  (should (equal (taut-emoji-translate ":twisted_rightwards_arrows:") "🔀"))
  (should (equal (taut-emoji-translate "thinking_face") "🤔"))
  (should (equal (taut-emoji-translate "unknown_emoji") ":unknown_emoji:"))
  (should (equal (taut-emoji-translate ":unknown_emoji:") ":unknown_emoji:")))

(ert-deftest taut-message-emoticon-translate-test ()
  "Test emoticon to emoji translation in `taut-emoticon-translate-string`."
  (should (equal (taut-emoticon-translate-string ":)") "🙂"))
  (should (equal (taut-emoticon-translate-string "hello :) world") "hello 🙂 world"))
  ;; Alphanumeric immediately preceding emoticon should NOT translate
  (should (equal (taut-emoticon-translate-string "hello:)") "hello:)"))
  ;; Alphanumeric immediately following emoticon should NOT translate
  (should (equal (taut-emoticon-translate-string ":)hello") ":)hello"))
  ;; Emoticon list check
  (should (equal (taut-emoticon-translate-string "<3") "❤️")))

(ert-deftest taut-message-insert-formatted-line-test ()
  "Test Slack advanced formatting parser `taut-message--insert-formatted-line`."
  (taut-initialize-mock-data)
  
  ;; Bold
  (with-temp-buffer
    (taut-message--insert-formatted-line "Hello *world* of Slack")
    (should (equal (buffer-string) "Hello world of Slack"))
    (goto-char (point-min))
    (search-forward "world")
    (should (eq (get-text-property (match-beginning 0) 'face) 'bold)))
  
  ;; Italic
  (with-temp-buffer
    (taut-message--insert-formatted-line "Hello _italic_ text")
    (should (equal (buffer-string) "Hello italic text"))
    (goto-char (point-min))
    (search-forward "italic")
    (should (eq (get-text-property (match-beginning 0) 'face) 'italic)))

  ;; Strike-through
  (with-temp-buffer
    (taut-message--insert-formatted-line "Hello ~strike~ text")
    (should (equal (buffer-string) "Hello strike text"))
    (goto-char (point-min))
    (search-forward "strike")
    (let ((face (get-text-property (match-beginning 0) 'face)))
      (should (equal (plist-get face :strike-through) t))
      (should (equal (plist-get face :foreground) "#8a8a8a"))))

  ;; Inline code
  (with-temp-buffer
    (taut-message--insert-formatted-line "Hello `code` here")
    (should (equal (buffer-string) "Hello code here"))
    (goto-char (point-min))
    (search-forward "code")
    (should (eq (get-text-property (match-beginning 0) 'face) 'taut-message-code)))

  ;; Emoji shortcode
  (with-temp-buffer
    (taut-message--insert-formatted-line "Hello :thumbsup: emoji")
    (should (equal (buffer-string) "Hello 👍 emoji")))

  ;; User mention
  (with-temp-buffer
    (taut-message--insert-formatted-line "Hello <@U_ALICE> mention")
    (should (equal (buffer-string) "Hello @alice mention"))
    (goto-char (point-min))
    (search-forward "@alice")
    (should (eq (get-text-property (match-beginning 0) 'face) 'taut-message-mention))
    (should (equal (get-text-property (match-beginning 0) 'help-echo) "Click/RET to DM @alice"))
    (should (keymapp (get-text-property (match-beginning 0) 'keymap))))

  ;; Channel mention
  (with-temp-buffer
    (taut-message--insert-formatted-line "Join <#C_GENERAL>")
    (should (equal (buffer-string) "Join #general"))
    (goto-char (point-min))
    (search-forward "#general")
    (should (eq (get-text-property (match-beginning 0) 'face) 'taut-message-mention))
    (should (equal (get-text-property (match-beginning 0) 'help-echo) "Click/RET to jump to channel #general"))
    (should (equal (get-text-property (match-beginning 0) 'taut-channel-id) "C_GENERAL"))))

(ert-deftest taut-message-resolve-point-pos-test ()
  "Test resolving point position in rebuilt buffers."
  ;; Test at-end condition
  (with-temp-buffer
    (insert "hello world")
    (should (= (taut-message--resolve-point-pos 1 nil nil nil t) (point-max))))

  ;; Test searching for old-ts in a buffer with text properties
  (with-temp-buffer
    (insert "Line 1\n")
    (let ((p1 (point)))
      (insert "Line 2 - Target\n")
      (let ((p2 (point)))
        (add-text-properties p1 p2 '(taut-message-ts "ts-123" taut-thread-ts "thread-456"))
        (insert "Line 3\n")
        ;; Test simple old-ts match
        (should (= (taut-message--resolve-point-pos 1 "ts-123" nil nil nil) p1))
        ;; Test sub-element thread-ts match
        (should (= (taut-message--resolve-point-pos 1 "ts-123" "thread-456" nil nil) p1))
        ;; Test fallback if old-ts is not found
        (should (= (taut-message--resolve-point-pos 5 "ts-999" nil nil nil) 5))))))

(ert-deftest taut-message-huddle-rendering-test ()
  "Test that Slack Huddle messages are correctly identified and formatted as beautiful boxes."
  (should (taut-message--huddle-message-p "📞 Slack Huddle: General in progress"))
  (should (taut-message--huddle-message-p "📞 Slack Huddle (Ended)"))
  (should-not (taut-message--huddle-message-p "Hello team"))
  
  (with-temp-buffer
    (taut-message--insert-huddle-box "📞 Slack Huddle: Design Session in progress" "         ")
    (let ((buf-str (buffer-string)))
      (should (string-match-p "╭───" buf-str))
      (should (string-match-p "╰───" buf-str))
      (should (string-match-p "🎧" buf-str))
      (should (string-match-p "Slack Huddle (Active)" buf-str))
      (should (string-match-p "Design Session" buf-str)))))

(ert-deftest taut-huddle-join-test ()
  "Test taut-huddle-join deep link URI generation and browse-url invocation."
  (let ((taut-team-id "T_MOCK_123")
        (opened-url nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url) (setq opened-url url))))
      ;; 1. Join with team id configured
      (let ((curr-buf (get-buffer-create "*taut-mock-msg*")))
        (with-current-buffer curr-buf
          (setq-local taut-current-channel-id "C_HUDDLE_CHAN")
          (taut-huddle-join))
        (should (equal opened-url "slack://channel?team=T_MOCK_123&id=C_HUDDLE_CHAN"))
        (kill-buffer curr-buf))
      
      ;; 2. Join without team id
      (let ((taut-team-id nil)
            (curr-buf (get-buffer-create "*taut-mock-msg*")))
        (with-current-buffer curr-buf
          (setq-local taut-current-channel-id "C_HUDDLE_CHAN")
          (taut-huddle-join))
        (should (equal opened-url "slack://channel?id=C_HUDDLE_CHAN"))
        (kill-buffer curr-buf)))))

(ert-deftest taut-message-code-block-parsing-test ()
  "Test code block parsing permutations, language detection, and first-line preservation."
  ;; 1. Test language predicate
  (should (taut-message--valid-lang-p "elisp"))
  (should (taut-message--valid-lang-p "elixir"))
  (should (taut-message--valid-lang-p "python"))
  (should (taut-message--valid-lang-p "sh"))
  (should-not (taut-message--valid-lang-p "HDISK2  0        ENABLED"))
  (should-not (taut-message--valid-lang-p "/dev/fslv01"))
  (should-not (taut-message--valid-lang-p ""))
  (should-not (taut-message--valid-lang-p nil))

  ;; 2. Test standard language-tagged block parsing
  (with-temp-buffer
    (taut-message--insert-formatted-text "```python\nprint(\"hello\")\n```")
    (let ((lang (get-text-property (point-min) 'taut-code-block-lang))
          (code (get-text-property (point-min) 'taut-code-block-content)))
      (should (equal lang "python"))
      (should (string-match-p "print(\"hello\")" code))))

  ;; 3. Test standard untagged block parsing
  (with-temp-buffer
    (taut-message--insert-formatted-text "```\njust text\n```")
    (let ((lang (get-text-property (point-min) 'taut-code-block-lang))
          (code (get-text-property (point-min) 'taut-code-block-content)))
      (should (equal lang "text"))
      (should (string-match-p "just text" code))))

  ;; 4. Test first-line content preservation (the bug fix)
  (with-temp-buffer
    (taut-message--insert-formatted-text "```HDISK2  0        ENABLED  SEL,OPT      FSCSI0\nhdisk2  1        Enabled  Non          fscsi0\n```")
    (let ((lang (get-text-property (point-min) 'taut-code-block-lang))
          (code (get-text-property (point-min) 'taut-code-block-content)))
      ;; Language should default to "text" instead of treating first line as lang
      (should (equal lang "text"))
      ;; Code content must preserve the entire first line
      (should (string-match-p "HDISK2  0        ENABLED  SEL,OPT      FSCSI0" code))
      (should (string-match-p "hdisk2  1        Enabled  Non          fscsi0" code)))))

(ert-deftest taut-message-runnable-block-rendering-test ()
  "Test that code blocks tagged with # @taut-runnable render interactively with executable text properties."
  (with-temp-buffer
    (taut-message--insert-formatted-text "```bash\n# @taut-runnable\ngit status\njust test\n```")
    ;; Search for "git status" and "just test" and verify text properties
    (goto-char (point-min))
    (let ((git-pos (search-forward "git status" nil t)))
      (should git-pos)
      (let ((cmds (get-text-property (1- git-pos) 'taut-block-commands))
            (kmap (get-text-property (1- git-pos) 'keymap)))
        (should (equal cmds '("git status" "just test")))
        (should (eq kmap taut-runnable-block-manage-map))))
    
    (goto-char (point-min))
    (let ((just-pos (search-forward "just test" nil t)))
      (should just-pos)
      (let ((cmds (get-text-property (1- just-pos) 'taut-block-commands))
            (kmap (get-text-property (1- just-pos) 'keymap)))
        (should (equal cmds '("git status" "just test")))
        (should (eq kmap taut-runnable-block-manage-map))))

    ;; Verify no "[Run]" button exists in the buffer
    (goto-char (point-min))
    (should-not (search-forward "[Run]" nil t))

    ;; Search for "[Manage Steps Table]" button and verify keymap & commands list
    (goto-char (point-min))
    (let ((manage-pos (search-forward "[Manage Steps Table]" nil t)))
      (should manage-pos)
      (let ((cmds (get-text-property (1- manage-pos) 'taut-block-commands))
            (kmap (get-text-property (1- manage-pos) 'keymap)))
        (should (equal cmds '("git status" "just test")))
        (should (eq kmap taut-runnable-block-manage-map))))))

(ert-deftest taut-message-code-block-toggle-line-numbers-test ()
  "Test dynamic line number toggling within rendered code blocks."
  (with-temp-buffer
    (taut-message--insert-formatted-text "```python\nprint(1)\nprint(2)\n```")
    (goto-char (point-min))
    ;; Initially, line numbers should be OFF
    (should-not (get-text-property (point) 'taut-code-block-show-line-numbers))
    (should-not (string-match-p "1 │" (buffer-string)))
    
    ;; Toggle line numbers ON
    (taut-code-block-toggle-line-numbers)
    (should (get-text-property (point) 'taut-code-block-show-line-numbers))
    (should (string-match-p "1 │ print" (buffer-string)))
    
    ;; Toggle line numbers OFF again
    (taut-code-block-toggle-line-numbers)
    (should-not (get-text-property (point) 'taut-code-block-show-line-numbers))
    (should-not (string-match-p "1 │" (buffer-string))))

  ;; Test dynamic inline expansion of truncated blocks when toggled ON
  (with-temp-buffer
    (let ((taut-code-block-max-lines 5))
      ;; Insert 8 lines of code
      (taut-message--insert-formatted-text "```python
line1
line2
line3
line4
line5
line6
line7
line8
```")
      (goto-char (point-min))
      ;; Truncation limit is 5, so there should be 3 lines hidden
      (should (string-match-p "\\.\\.\\. (\\+3 lines hidden" (buffer-string)))
      (should-not (string-match-p "line8" (buffer-string)))
      
      ;; Toggle line numbers ON -> should fully expand inline showing all lines
      (taut-code-block-toggle-line-numbers)
      (should (string-match-p "line8" (buffer-string)))
      (should-not (string-match-p "lines hidden" (buffer-string)))
      
      ;; Toggle line numbers OFF -> should truncate again
      (taut-code-block-toggle-line-numbers)
      (should (string-match-p "\\.\\.\\. (\\+3 lines hidden" (buffer-string)))
      (should-not (string-match-p "line8" (buffer-string))))))

(ert-deftest taut-code-block-local-edits-and-lang-assignment-test ()
  "Test that local edits and language assignments persist across model updates."
  (taut-model-clear-all)
  (let* ((msg-ts "1688500000.12345")
         (orig-text "Check this unlabeled code:\n```\n(defun hello ())\n```")
         (msg (make-taut-message :ts msg-ts :text orig-text :channel-id "C1" :user-id "U_OTHER")))
    ;; Add original message
    (taut-model-add-message msg)
    
    ;; Verify message was added with original text
    (should (string= (taut-message-text msg) orig-text))
    
    ;; 1. Test assigning a language to the code block
    (with-temp-buffer
      (insert orig-text)
      ;; Simulate the properties that would be at point in the message buffer
      (goto-char (point-min))
      (search-forward "(defun hello")
      (let ((inhibit-read-only t))
        (add-text-properties (match-beginning 0) (match-end 0)
                             (list 'taut-code-block-content "(defun hello ())\n"
                                   'taut-code-block-lang "text"
                                   'taut-message-ts msg-ts)))
      
      ;; Call language assignment
      (goto-char (match-beginning 0))
      (taut-code-block-set-language "elisp")
      
      ;; Verify memory map contains the override
      (let ((override-text (gethash msg-ts taut-local-edits)))
        (should (string-match-p "```elisp" override-text))
        (should (string-match-p "(defun hello ())\n" override-text)))
      
      ;; Verify the active message has been updated
      (should (string-match-p "```elisp" (taut-message-text msg)))
      
      ;; Clear state and simulate fetching from API (overwriting memory models)
      (let ((new-msg (make-taut-message :ts msg-ts :text orig-text :channel-id "C1" :user-id "U_OTHER")))
        ;; Adding the message with original Slack text should automatically apply our local edits override!
        (taut-model-add-message new-msg)
        (should (string-match-p "```elisp" (taut-message-text new-msg)))
        (should-not (string-match-p "```\n(defun hello" (taut-message-text new-msg)))))))

(ert-deftest taut-message-thread-replies-line-properties-test ()
  "Test that the entire replies line, including indentation and newline, has correct properties."
  (taut-model-clear-all)
  (let* ((msg (make-taut-message :ts "1688500000.111"
                                 :text "Test root message"
                                 :reply-count 5
                                 :channel-id "C1"
                                 :user-id "U1")))
    (with-temp-buffer
      (taut-message--render-message-line msg)
      
      ;; Search for "replies" in the buffer
      (goto-char (point-min))
      (should (search-forward "replies" nil t))
      
      ;; Go to the start of this line (the indentation "         ")
      (forward-line 0)
      (let ((start-pos (point)))
        ;; Verify the start of the line has keymap and metadata properties
        (should (get-text-property start-pos 'keymap))
        (should (equal (get-text-property start-pos 'taut-thread-ts) "1688500000.111"))
        (should (equal (get-text-property start-pos 'taut-message-ts) "1688500000.111"))
        (should (equal (get-text-property start-pos 'taut-message-id) (taut-message-id msg))))
      
      ;; Go to the end of the line (on the newline character)
      (end-of-line)
      (let ((eol-pos (point)))
        ;; Verify the newline itself has keymap and metadata properties, but NO mouse-face
        (should (get-text-property eol-pos 'keymap))
        (should (equal (get-text-property eol-pos 'taut-thread-ts) "1688500000.111"))
        (should (equal (get-text-property eol-pos 'taut-message-ts) "1688500000.111"))
        (should (equal (get-text-property eol-pos 'taut-message-id) (taut-message-id msg)))
        (should-not (get-text-property eol-pos 'mouse-face))))))

(ert-deftest taut-message-copy-reference-test ()
  "Test copying a Slack message reference via `taut-message-copy-reference`."
  (taut-initialize-mock-data)
  (setq taut-message-reference-ring nil)
  (let ((msg-ts "1688460000.0001") ;; Alice's development channel message
        (taut-team-id "T_MY_TEAM"))
    (with-temp-buffer
      (insert "Hey team, we're building the new Emacs client Taut!")
      ;; Add message ts property to point
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (add-text-properties (point-min) (point-max) (list 'taut-message-ts msg-ts)))
      
      ;; 1. Call copy-reference on valid message at point
      (taut-message-copy-reference)
      
      ;; Check reference ring contents
      (should (= (length taut-message-reference-ring) 1))
      (let ((ref (car taut-message-reference-ring)))
        (should (equal (plist-get ref :channel-id) "C_DEV"))
        (should (equal (plist-get ref :channel-name) "development"))
        (should (equal (plist-get ref :ts) msg-ts))
        (should (equal (plist-get ref :author) "alice"))
        (should (equal (plist-get ref :snippet) "Hey team, we're building the new Emacs client *..."))
        (should (equal (plist-get ref :url) "https://T_MY_TEAM.slack.com/archives/C_DEV/p16884600000001")))
      
      ;; Check kill-ring has URL
      (should (equal (car kill-ring) "https://T_MY_TEAM.slack.com/archives/C_DEV/p16884600000001"))
      
      ;; 2. Capping limit check
      (let ((taut-message-reference-ring-max 3))
        (setq taut-message-reference-ring nil)
        ;; Push 4 references
        (dotimes (i 4)
          (taut-message-copy-reference))
        ;; Should be capped at 3
        (should (= (length taut-message-reference-ring) 3))))))

(ert-deftest taut-custom-emoji-resolution-test ()
  "Test custom emoji fetching, resolution, and recursion safety."
  (clrhash taut-custom-emojis)
  (puthash "hyper-rocket" "https://example.com/hyper-rocket.gif" taut-custom-emojis)
  (puthash "super-rocket" "alias:hyper-rocket" taut-custom-emojis)
  (puthash "mega-rocket" "alias:super-rocket" taut-custom-emojis)
  ;; Infinite loop test case
  (puthash "loop-1" "alias:loop-2" taut-custom-emojis)
  (puthash "loop-2" "alias:loop-1" taut-custom-emojis)

  ;; Check direct retrieval
  (should (equal (taut-custom-emoji-get "hyper-rocket") "https://example.com/hyper-rocket.gif"))
  ;; Check single alias resolution
  (should (equal (taut-custom-emoji-get "super-rocket") "https://example.com/hyper-rocket.gif"))
  ;; Check recursive alias resolution
  (should (equal (taut-custom-emoji-get "mega-rocket") "https://example.com/hyper-rocket.gif"))
  ;; Check loop safety - should return the unresolved alias or nil, but NOT spin forever
  (should-not (equal (taut-custom-emoji-get "loop-1") "https://example.com/hyper-rocket.gif"))
  ;; Check non-existent custom emoji
  (should-not (taut-custom-emoji-get "non-existent")))

(ert-deftest taut-custom-emoji-file-path-test ()
  "Test local file path generation for custom emojis."
  (let ((cache-dir (taut-custom-emoji-cache-dir)))
    (should (equal (taut-custom-emoji-file-path "hyper-rocket" "https://example.com/hyper-rocket.gif")
                   (expand-file-name "hyper-rocket.gif" cache-dir)))
    (should (equal (taut-custom-emoji-file-path "simple-emoji" "https://example.com/simple-emoji")
                   (expand-file-name "simple-emoji.png" cache-dir)))))

(ert-deftest taut-custom-emoji-formatting-fallback-test ()
  "Test that custom emojis properly format, falling back gracefully if images are unavailable."
  (clrhash taut-custom-emojis)
  (puthash "sparkles-custom" "https://example.com/sparkles.png" taut-custom-emojis)
  
  ;; 1. Reaction emoji formatting fallback (text-only terminal or missing cache)
  (let ((formatted (taut-message--format-reaction-emoji "sparkles-custom")))
    (should (equal formatted ":sparkles-custom:")))
  
  ;; With bracketed/coloned input
  (let ((formatted (taut-message--format-reaction-emoji ":sparkles-custom:")))
    (should (equal formatted ":sparkles-custom:")))

  ;; Standard emoji should translate to unicode
  (let ((formatted (taut-message--format-reaction-emoji "thumbsup")))
    (should (equal formatted "👍")))

  ;; 2. Inline message formatting fallback
  (with-temp-buffer
    (taut-message--insert-formatted-line "Custom emoji :sparkles-custom: inside text")
    (should (equal (buffer-string) "Custom emoji :sparkles-custom: inside text"))))

(ert-deftest taut-message-avatar-rendering-test ()
  "Test rendering of user avatars based on taut-display-avatars-inline setting."
  (taut-model-clear-all)
  (let* ((user (make-taut-user :id "U1" :username "alice" :avatar-url "https://example.com/alice.png"))
         (msg (make-taut-message :ts "1688500000.111"
                                 :text "Hello world"
                                 :channel-id "C1"
                                 :user-id "U1")))
    (taut-model-add-user user)
    ;; Mock display-images-p, create-image, and file-exists-p to simulate loaded avatar
    (cl-letf (((symbol-function 'display-images-p) (lambda () t))
              ((symbol-function 'create-image) (lambda (&rest _args) 'mock-avatar-image))
              ((symbol-function 'file-exists-p) (lambda (&rest _args) t)))
      
      ;; 1. With taut-display-avatars-inline = t
      (let ((taut-display-avatars-inline t))
        (with-temp-buffer
          (taut-message--render-message-line msg)
          (goto-char (point-min))
          ;; The buffer should start with the mock-avatar-image
          (should (equal (get-text-property (point) 'display) 'mock-avatar-image))
          ;; It should be followed by a space and then alice
          (forward-char 1)
          (should (equal (char-after) ? ))
          (forward-char 1)
          (should (search-forward "alice" nil t))))
      
      ;; 2. With taut-display-avatars-inline = nil
      (let ((taut-display-avatars-inline nil))
        (with-temp-buffer
          (taut-message--render-message-line msg)
          (goto-char (point-min))
          ;; The buffer should NOT start with any display property (just "alice")
          (should-not (get-text-property (point) 'display))
          (should (search-forward "alice" nil t)))))))

(ert-deftest taut-message-media-previews-rendering-test ()
  "Test inline media, image, and text document previews rendering."
  (taut-model-clear-all)
  (let* ((user (make-taut-user :id "U1" :username "alice"))
         (files-list '(((name . "screenshot.png")
                        (mimetype . "image/png")
                        (url_private_download . "https://files.slack.com/files-pri/T01-F12/download/screenshot.png"))
                       ((name . "bootstrap.sh")
                        (mimetype . "text/x-sh")
                        (url_private_download . "https://files.slack.com/files-pri/T01-F34/download/bootstrap.sh"))))
         (msg (make-taut-message :ts "1688500000.111"
                                 :text "Check this file"
                                 :channel-id "C1"
                                 :user-id "U1"
                                 :files files-list)))
    (taut-model-add-user user)
    
    (cl-letf (((symbol-function 'display-images-p) (lambda () t))
              ((symbol-function 'create-image) (lambda (&rest _args) 'mock-attachment-image))
              ((symbol-function 'file-exists-p) (lambda (&rest _args) t))
              ((symbol-function 'taut-message--read-file-string) (lambda (&rest _args) "echo \"hello world\"")))
      
      ;; 1. With taut-display-images-inline = t
      (let ((taut-display-images-inline t))
        (with-temp-buffer
          (taut-message--render-message-line msg)
          (goto-char (point-min))
          ;; Verify huddle/text body rendered
          (should (search-forward "Check this file" nil t))
          ;; Verify inline image mock attachment rendered (as 'display image property)
          (goto-char (point-min))
          (should (search-forward " " nil t)) ; we find space propertized with display
          (let ((found nil))
            (goto-char (point-min))
            (while (and (not found) (not (eobp)))
              (if (equal (get-text-property (point) 'display) 'mock-attachment-image)
                  (setq found t)
                (forward-char 1)))
            (should found))
          ;; Verify inline text file preview is rendered inside code block
          (goto-char (point-min))
          (should (search-forward "echo \"hello world\"" nil t))
          ;; Verify old redundant document fallback link is NOT present
          (goto-char (point-min))
          (should-not (search-forward "📎 bootstrap.sh [File] (Click/RET to open)" nil t))))
          
      ;; 2. With taut-display-images-inline = nil (images and non-text files don't render previews)
      (let ((taut-display-images-inline nil))
        (with-temp-buffer
          (taut-message--render-message-line msg)
          (goto-char (point-min))
          ;; Verify no display image exists
          (let ((found nil))
            (goto-char (point-min))
            (while (and (not found) (not (eobp)))
              (if (equal (get-text-property (point) 'display) 'mock-attachment-image)
                  (setq found t)
                (forward-char 1)))
            (should-not found))
          ;; Verify no old image/file fallback links are appended in preview block (as they are in main body now)
          (goto-char (point-min))
          (should-not (search-forward "📎 screenshot.png [Image] (Click/RET to open)" nil t)))))))

(ert-deftest taut-message-real-name-rendering-test ()
  "Test rendering of user real names instead of usernames in chat and inline replies."
  (taut-model-clear-all)
  (let* ((user-real (make-taut-user :id "U_REAL" :username "greg.rhoades" :real-name "Greg Rhoades"))
         (user-no-real (make-taut-user :id "U_NO_REAL" :username "alice" :real-name ""))
         (msg-real (make-taut-message :ts "1688500000.111"
                                      :text "Hello with real name"
                                      :channel-id "C1"
                                      :user-id "U_REAL"))
         (msg-no-real (make-taut-message :ts "1688500000.222"
                                         :text "Hello with username"
                                         :channel-id "C1"
                                         :user-id "U_NO_REAL")))
    (taut-model-add-user user-real)
    (taut-model-add-user user-no-real)
    
    ;; 1. Check message rendering for user with real name
    (with-temp-buffer
      (taut-message--render-message-line msg-real)
      (goto-char (point-min))
      (should (search-forward "Greg Rhoades" nil t))
      (goto-char (point-min))
      (should-not (search-forward "greg.rhoades" nil t)))

    ;; 2. Check message rendering for user with empty real name (fallback to username)
    (with-temp-buffer
      (taut-message--render-message-line msg-no-real)
      (goto-char (point-min))
      (should (search-forward "alice" nil t)))

    ;; 3. Check inline reply rendering for user with real name
    (with-temp-buffer
      (taut-message--render-inline-reply msg-real t "1688500000.000")
      (goto-char (point-min))
      (should (search-forward "Greg Rhoades" nil t))
      (goto-char (point-min))
      (should-not (search-forward "greg.rhoades" nil t)))

    ;; 4. Check inline reply rendering for user with empty real name (fallback to username)
    (with-temp-buffer
      (taut-message--render-inline-reply msg-no-real t "1688500000.000")
      (goto-char (point-min))
      (should (search-forward "alice" nil t)))))

(provide 'test-taut-message)
;;; test-taut-message.el ends here


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

(provide 'test-taut-message)
;;; test-taut-message.el ends here

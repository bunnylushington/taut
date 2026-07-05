;;; taut-transient.el --- Transient Dispatcher for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Google DeepMind

;; Author: Antigravity
;; Keywords: comm, slack

;;; Commentary:
;; This file implements gorgeous, discoverable keyboard-driven transient
;; menus for Taut using Emacs's built-in `transient' library.

;;; Code:

(require 'transient)
(require 'taut-model)
(require 'taut-api)

;; Forward declarations for byte-compiler peace of mind
(declare-function taut-connect "taut")
(declare-function taut-quit "taut")
(declare-function taut-dm-open "taut")
(declare-function taut-sidebar-show "taut-sidebar")
(declare-function taut-sidebar-bury "taut-sidebar")
(declare-function taut-inbox-show "taut-inbox")
(declare-function taut-inbox-bury "taut-inbox")
(declare-function taut-message-refresh "taut-message")
(declare-function taut-message-upload-file "taut-message")
(declare-function taut-message-reply-normal "taut-compose")
(declare-function taut-message-reply-quote "taut-compose")
(declare-function taut-message-start-thread "taut-message")
(declare-function taut-message-toggle-thread-inline "taut-message")
(declare-function taut-message-add-reaction "taut-message")
(declare-function taut-thread-close "taut-thread")

(declare-function taut-code-block-copy "taut-message")
(declare-function taut-code-block-view "taut-message")
(declare-function taut-code-block-save "taut-message")

(declare-function taut-compose-send "taut-compose")
(declare-function taut-compose-abort "taut-compose")
(declare-function taut-compose-insert-code-block "taut-compose")
(declare-function taut-compose-insert-link "taut-compose")
(declare-function taut-compose-insert-user-mention "taut-compose")

;;;###autoload
(transient-define-prefix taut-dispatch ()
  "The main discoverable control center for the Taut Slack client."
  ["💬 Taut: Premium Slack Client"
   ["Workspace & Navigation"
    ("c" "Connect to Slack"    taut-connect)
    ("d" "Direct Message (User)" taut-dm-open)
    ("s" "Show/Focus Sidebar"  taut-sidebar-show)
    ("i" "Show/Focus Inbox"    taut-inbox-show)
    ("q" "Quit / Bury Pane"    quit-window)
    ("Q" "Hard Quit Taut"      taut-quit)]
   ["Chat Actions (Conversation Buffer)"
    ("r" "Reply / Send Message" taut-message-reply-normal)
    ("R" "Reply quoting Msg"   taut-message-reply-quote)
    ("a" "Add Emoji Reaction"  taut-message-add-reaction)
    ("u" "Upload File"         taut-message-upload-file)
    ("g" "Refresh Chat History" taut-message-refresh)]
   ["Thread Actions"
    ("RET" "Open Sidebar Thread (Pane)" taut-message-start-thread)
    ("TAB" "Toggle Thread Inline (Chat)" taut-message-toggle-thread-inline)
    ("x"   "Close Right Thread Pane"    taut-thread-close)]])

;;;###autoload
(transient-define-prefix taut-code-block-dispatch ()
  "Operations for the code block under the cursor."
  ["💻 Code Block Actions"
   ["Operations"
    ("c" "Copy Block to Clipboard" taut-code-block-copy)
    ("v" "View in Native Major Mode" taut-code-block-view)
    ("s" "Save Block to File"      taut-code-block-save)]])

;;;###autoload
(transient-define-prefix taut-compose-dispatch ()
  "Helper operations for composing messages in Taut."
  ["✍️ Taut Compose Actions"
   ["Actions"
    ("C-c" "Send Message"         taut-compose-send)
    ("C-k" "Abort/Discard Draft"  taut-compose-abort)]
   ["Formatting Helpers"
    ("b" "Insert Code Block"     taut-compose-insert-code-block)
    ("l" "Insert Link"           taut-compose-insert-link)
    ("u" "Insert User Mention"   taut-compose-insert-user-mention)]])

(provide 'taut-transient)
;;; taut-transient.el ends here

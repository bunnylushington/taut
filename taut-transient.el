;;; taut-transient.el --- Transient Dispatcher for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Google DeepMind

;; Author: Antigravity
;; Keywords: comm, slack

;;; Commentary:
;; This file implements a gorgeous, discoverable keyboard-driven transient
;; menu for Taut using Emacs's built-in `transient' library. Pressing `?` in
;; any Taut buffer displays this panel to guide new and power users alike.

;;; Code:

(require 'transient)
(require 'taut-model)
(require 'taut-api)

;; Forward declarations for byte-compiler peace of mind
(declare-function taut-connect "taut")
(declare-function taut-sidebar-show "taut-sidebar")
(declare-function taut-sidebar-bury "taut-sidebar")
(declare-function taut-inbox-show "taut-inbox")
(declare-function taut-inbox-bury "taut-inbox")
(declare-function taut-message-refresh "taut-message")
(declare-function taut-message-send "taut-message")
(declare-function taut-message-start-thread "taut-message")
(declare-function taut-message-toggle-thread-inline "taut-message")
(declare-function taut-message-add-reaction "taut-message")
(declare-function taut-thread-close "taut-thread")

;;;###autoload
(transient-define-prefix taut-dispatch ()
  "The main discoverable control center for the Taut Slack client."
  ["💬 Taut: Premium Slack Client"
   ["Workspace & Navigation"
    ("c" "Connect to Slack"    taut-connect)
    ("s" "Show/Focus Sidebar"  taut-sidebar-show)
    ("i" "Show/Focus Inbox"    taut-inbox-show)
    ("q" "Quit / Bury Pane"    quit-window)]
   ["Chat Actions (Conversation Buffer)"
    ("r" "Send Message"        taut-message-send)
    ("a" "Add Emoji Reaction"  taut-message-add-reaction)
    ("g" "Refresh Chat History" taut-message-refresh)]
   ["Thread Actions"
    ("RET" "Open Sidebar Thread (Pane)" taut-message-start-thread)
    ("TAB" "Toggle Thread Inline (Chat)" taut-message-toggle-thread-inline)
    ("x"   "Close Right Thread Pane"    taut-thread-close)]])

(provide 'taut-transient)
;;; taut-transient.el ends here

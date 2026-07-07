;;; taut-transient.el --- Transient Dispatcher for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington

;; Author: bunny@bapi.us
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
(declare-function taut-reset-layout "taut")
(declare-function taut-dm-open "taut")
(declare-function taut-sidebar-show "taut-sidebar")
(declare-function taut-sidebar-bury "taut-sidebar")
(declare-function taut-inbox-show "taut-inbox")
(declare-function taut-inbox-bury "taut-inbox")
(declare-function taut-message-refresh "taut-message")
(declare-function taut-message-upload-file "taut-message")
(declare-function taut-message-reply-normal "taut-compose")
(declare-function taut-message-reply-quote "taut-compose")
(declare-function taut-message-edit "taut-message")
(declare-function taut-message-start-thread "taut-message")
(declare-function taut-message-toggle-thread-inline "taut-message")
(declare-function taut-message-add-reaction "taut-message")
(declare-function taut-thread-close "taut-thread")

(declare-function taut-sidebar-toggle-section-at-point "taut-sidebar")
(declare-function taut-sidebar-mark-all-read "taut-sidebar")
(declare-function taut-sidebar-toggle-channel-hidden "taut-sidebar")

(declare-function taut-inbox-activate "taut-inbox")
(declare-function taut-inbox-mark-read "taut-inbox")
(declare-function taut-inbox-mark-channel-read "taut-inbox")
(declare-function taut-inbox-filter-all "taut-inbox")
(declare-function taut-inbox-filter-unreads "taut-inbox")
(declare-function taut-inbox-filter-dms "taut-inbox")
(declare-function taut-inbox-filter-mentions "taut-inbox")
(declare-function taut-inbox-filter-threads "taut-inbox")
(declare-function taut-inbox-date-filter-today "taut-inbox")
(declare-function taut-inbox-date-filter-last-7 "taut-inbox")
(declare-function taut-inbox-date-filter-last-30 "taut-inbox")
(declare-function taut-inbox-date-filter-all "taut-inbox")
(declare-function taut-inbox-refresh "taut-inbox")

(declare-function taut-message-delete "taut-message")
(declare-function taut-message-toggle-star "taut-message")
(declare-function taut-message-next "taut-message")
(declare-function taut-message-previous "taut-message")

(declare-function taut-message-copy-at-point "taut-message")
(declare-function taut-message-view-at-point "taut-message")
(declare-function taut-message-save-at-point "taut-message")

(declare-function taut-thread-refresh "taut-thread")

(declare-function taut-code-block-copy "taut-message")
(declare-function taut-code-block-view "taut-message")
(declare-function taut-code-block-save "taut-message")

(declare-function taut-compose-send "taut-compose")
(declare-function taut-compose-abort "taut-compose")
(declare-function taut-compose-insert-code-block "taut-compose")
(declare-function taut-compose-insert-link "taut-compose")
(declare-function taut-compose-insert-user-mention "taut-compose")
(declare-function taut-search-quick "taut-search")
(declare-function taut-search-advanced "taut-search")


;;;###autoload
(transient-define-prefix taut-dispatch ()
  "The main discoverable control center for the Taut Slack client."
  ["🌑 Taut"
   ["Workspace & Navigation"
    ("Z" "Connect to Slack"    taut-connect)
    ("U" "Direct Message (User)" taut-dm-open)
    ("S" "Show/Focus Sidebar"  taut-sidebar-show)
    ("I" "Show/Focus Slack Activity" taut-inbox-show)
    ("W" "Reset Layout"        taut-reset-layout)
    ("F" "Search / Find"       taut-search-quick)
    ("P" "Advanced Search"     taut-search-advanced)
    ("q" "Quit / Bury Pane"    quit-window :if (lambda () (not (eq major-mode 'taut-thread-mode))))
    ("Q" "Hard Quit Taut"      taut-quit)]
   
   ["Sidebar Actions"
    :if (lambda () (eq major-mode 'taut-sidebar-mode))
    ("TAB" "Toggle Section"     taut-sidebar-toggle-section-at-point)
    ("M"   "Mark Channel Read"  taut-sidebar-mark-all-read)
    ("h"   "Toggle Hidden"      taut-sidebar-toggle-channel-hidden)]
   
   ["Slack Activity (Inbox) Actions"
    :if (lambda () (eq major-mode 'taut-inbox-mode))
    ("RET" "Open Channel/Thread" taut-inbox-activate)
    ("d"   "Mark Read / Archive" taut-inbox-mark-read)
    ("e"   "Dismiss/Archive Msg" taut-inbox-mark-read)
    ("M"   "Mark Channel Read"   taut-inbox-mark-channel-read)
    ("g"   "Refresh Inbox"       taut-inbox-refresh)]
   
   ["Slack Activity Filters"
    :if (lambda () (eq major-mode 'taut-inbox-mode))
    ("a"   "Show All"            taut-inbox-filter-all)
    ("u"   "Show Unreads Only"   taut-inbox-filter-unreads)
    ("D"   "Show DMs Only"       taut-inbox-filter-dms)
    ("m"   "Show Mentions Only"  taut-inbox-filter-mentions)
    ("t"   "Show Threads Only"   taut-inbox-filter-threads)]

   ["Slack Activity Date Filters"
    :if (lambda () (eq major-mode 'taut-inbox-mode))
    ("1"   "Filter Today"        taut-inbox-date-filter-today)
    ("2"   "Filter Last 7 Days"  taut-inbox-date-filter-last-7)
    ("3"   "Filter Last 30 Days" taut-inbox-date-filter-last-30)
    ("4"   "Filter All Time"     taut-inbox-date-filter-all)]
   
   ["Conversation Actions"
    :if (lambda () (memq major-mode '(taut-message-mode taut-thread-mode)))
    ("r"   "Reply / Send Message" taut-message-reply-normal)
    ("R"   "Reply quoting Msg"    taut-message-reply-quote)
    ("e"   "Edit Message"         taut-message-edit)
    ("d"   "Delete Message"       taut-message-delete)
    ("a"   "Add Emoji Reaction"   taut-message-add-reaction)
    ("b"   "Toggle Bookmark/Star" taut-message-toggle-star)
    ("u"   "Upload File"          taut-message-upload-file)
    ("g"   "Refresh History"      taut-message-refresh :if (lambda () (eq major-mode 'taut-message-mode)))
    ("g"   "Refresh Thread"       taut-thread-refresh :if (lambda () (eq major-mode 'taut-thread-mode)))]
   
   ["Message Navigation"
    :if (lambda () (memq major-mode '(taut-message-mode taut-thread-mode)))
    ("n"   "Next Message"         taut-message-next)
    ("p"   "Previous Message"     taut-message-previous)]
   
   ["Code & File Actions"
    :if (lambda () (memq major-mode '(taut-message-mode taut-thread-mode)))
    ("v"   "View Code/Attachment" taut-message-view-at-point)
    ("c"   "Copy Code/Text"       taut-message-copy-at-point)
    ("s"   "Save Attachment"      taut-message-save-at-point)]
   
   ["Thread Layout Actions"
    :if (lambda () (eq major-mode 'taut-message-mode))
    ("RET" "Open Sidebar Thread"  taut-message-start-thread)
    ("TAB" "Toggle Thread Inline" taut-message-toggle-thread-inline)]
   
   ["Thread Buffer Actions"
    :if (lambda () (eq major-mode 'taut-thread-mode))
    ("q"   "Close Thread Pane"    taut-thread-close)]])

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

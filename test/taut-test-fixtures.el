;;; taut-test-fixtures.el --- High-fidelity test fixtures for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; This file contains high-fidelity mock data and simulators for Taut's
;; testing environment, extracted from the legacy demo/mock mode.

;;; Code:

(require 'cl-lib)
(require 'taut-model)

(defvar taut-mock-timer nil
  "Timer object running the background simulator.")

(defvar taut-mock-messages-pool
  '(("@alice" "C_GENERAL" "Just finished testing the migration scripts. Looks completed!")
    ("@bob" "C_GENERAL" "Can anyone review my PR for *taut-sidebar*?")
    ("@carol" "C_DEV" "Hey <@U_ME>, can you verify if standard global mirror relationships delete cleanly?")
    ("@dave" "C_DEV" "I'm experiencing some drift on the local caches. Anyone else?")
    ("@alice" "C_ALICE_DM" "Hey! Don't miss this thread update. I just left some answers.")
    ("@bob" "C_BOB_DM" "Sent over the specs. Ping me when you get a chance.")
    ("@carol" "C_IDEAS" "What about adding support for custom theme palettes? Let's discuss.")
    ;; Thread replies to Bob's thread in C_IDEAS (which is watched)
    ("@dave" "C_IDEAS" "I tried running it, but I'm getting a timeout on socket connections." "1688470000.0001")
    ("@alice" "C_IDEAS" "Let's debug it together. Are you free?" "1688470000.0001"))
  "Pool of random mock events (sender, channel-id, text, [thread-ts]).")

(defun taut-initialize-mock-data ()
  "Pre-populate the local database with high-fidelity sample data."
  (taut-model-clear-all)

  ;; 1. Register Users
  (taut-model-add-user (make-taut-user :id "U_ME" :username "me" :real-name "Bunny Lushington" :presence 'online :is-me t))
  (taut-model-add-user (make-taut-user :id "U_ALICE" :username "alice" :real-name "Alice Smith" :presence 'online))
  (taut-model-add-user (make-taut-user :id "U_BOB" :username "bob" :real-name "Bob Jones" :presence 'away))
  (taut-model-add-user (make-taut-user :id "U_CAROL" :username "carol" :real-name "Carol Danvers" :presence 'offline))
  (taut-model-add-user (make-taut-user :id "U_DAVE" :username "dave" :real-name "Dave Bowman" :presence 'online))

  ;; 2. Register Channels
  (taut-model-add-channel (make-taut-channel :id "C_GENERAL" :name "general" :type 'public :unread-count 0 :mention-count 0 :is-starred t :topic "Company-wide announcements"))
  (taut-model-add-channel (make-taut-channel :id "C_DEV" :name "development" :type 'public :unread-count 0 :mention-count 0 :is-starred t :topic "Core technical discussion & reviews"))
  (taut-model-add-channel (make-taut-channel :id "C_IDEAS" :name "ideas" :type 'public :unread-count 0 :mention-count 0 :is-starred nil :topic "Brainstorming and blue-sky initiatives"))
  (taut-model-add-channel (make-taut-channel :id "C_ALICE_DM" :name "alice" :type 'dm :unread-count 0 :mention-count 0 :is-starred nil))
  (taut-model-add-channel (make-taut-channel :id "C_BOB_DM" :name "bob" :type 'dm :unread-count 0 :mention-count 0 :is-starred nil))

  ;; 3. Seed Conversations (Sorted by timestamp ascending)
  ;; #general
  (taut-model-add-message (make-taut-message :id "m1_1" :channel-id "C_GENERAL" :user-id "U_BOB" :text "Welcome to the new Slack workspace! Let's get things rolling." :ts "1688450000.0001" :is-unread nil))
  (taut-model-add-message (make-taut-message :id "m1_2" :channel-id "C_GENERAL" :user-id "U_ALICE" :text "Excited to be here! Check out the `development` channel." :ts "1688450100.0001" :is-unread nil))

  ;; #development
  (taut-model-add-message (make-taut-message :id "m2_1" :channel-id "C_DEV" :user-id "U_ALICE" :text "Hey team, we're building the new Emacs client *Taut*! Focus is on extreme UX and notifications." :ts "1688460000.0001" :is-unread nil))
  (taut-model-add-message (make-taut-message :id "m2_2" :channel-id "C_DEV" :user-id "U_DAVE" :text "Amazing! Will it support thread indicators?" :ts "1688460500.0001" :is-unread nil :thread-ts "1688460000.0001"))
  (taut-model-add-message (make-taut-message :id "m2_3" :channel-id "C_DEV" :user-id "U_ALICE" :text "Yes, clicking the thread replies will open a dedicated side panel." :ts "1688460600.0001" :is-unread nil :thread-ts "1688460000.0001"))

  ;; #ideas (With active thread)
  (taut-model-add-message (make-taut-message :id "m3_1" :channel-id "C_IDEAS" :user-id "U_BOB" :text "Should we run our client tests with `mix testall`?" :ts "1688470000.0001" :is-unread nil))

  ;; Direct Messages (Alice - Read, Bob - Unread DM)
  (taut-model-add-message (make-taut-message :id "mdm1_1" :channel-id "C_ALICE_DM" :user-id "U_ALICE" :text "Hey, are you free for a review today?" :ts "1688480000.0001" :is-unread nil))
  (taut-model-add-message (make-taut-message :id "mdm2_1" :channel-id "C_BOB_DM" :user-id "U_BOB" :text "Did you get that script command for standard storage migration tests?" :ts "1688490000.0001" :is-unread t))

  ;; Set up watch thread list with #development thread
  (push "1688460000.0001" taut-watched-threads))

(provide 'taut-test-fixtures)
;;; taut-test-fixtures.el ends here

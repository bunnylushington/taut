;;; test-taut-api.el --- Unit tests for taut-api.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; Unit tests for Taut REST API layer and parser logic (taut-api.el).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'taut-api)
(require 'taut-model)

(ert-deftest taut-api-bool-test ()
  "Test the boolean utility function `taut-api--bool'."
  (should (taut-api--bool t))
  (should (taut-api--bool 'yes))
  (should-not (taut-api--bool nil))
  (should-not (taut-api--bool :json-false))
  (should (taut-api--bool 0)))

(ert-deftest taut-api-clean-mpim-name-test ()
  "Test cleaning up mpim names."
  (should (equal (taut-api--clean-mpim-name "mpdm-alice--bob--charles-1") "alice, bob, charles"))
  (should (equal (taut-api--clean-mpim-name "some-other-name") "some-other-name")))

(ert-deftest taut-api-load-tokens-test ()
  "Test loading tokens from auth-source."
  (let ((taut-bot-token nil)
        (taut-app-token nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (let ((host (plist-get args :host))
                       (user (plist-get args :user)))
                   (cond
                    ((and (equal host "api.slack.com") (equal user "bot"))
                     '((:host "api.slack.com" :user "bot" :secret "mock-bot-xoxb")))
                    ((and (equal host "api.slack.com") (equal user "app"))
                     '((:host "api.slack.com" :user "app" :secret "mock-app-xapp"))))))))
      (taut-api-load-tokens-from-authinfo)
      (should (equal taut-bot-token "mock-bot-xoxb"))
      (should (equal taut-app-token "mock-app-xapp")))))

(ert-deftest taut-api-fetch-users-test ()
  "Test fetching and registering users from Slack API payload."
  (taut-model-clear-all)
  (cl-letf (((symbol-function 'taut-api--request)
             (lambda (endpoint &optional _params _method _apptoken)
               (cond
                ((equal endpoint "users.list")
                 '((ok . t)
                   (members . (((id . "U_ALICE")
                                (name . "alice")
                                (real_name . "Alice Smith")
                                (presence . "active")
                                (is_bot . nil)
                                (deleted . nil))
                               ((id . "U_BOB")
                                (name . "bob")
                                (real_name . "Bob Jones")
                                (presence . "away")
                                (is_bot . nil)
                                (deleted . nil))))))))))
    (taut-api-fetch-users)
    (let ((alice (taut-model-get-user "U_ALICE"))
          (bob (taut-model-get-user "U_BOB")))
      (should alice)
      (should (equal (taut-user-username alice) "alice"))
      (should (equal (taut-user-real-name alice) "Alice Smith"))
      (should (eq (taut-user-presence alice) 'offline)) ; defaults to offline unless marked away, then synced dynamically
      
      (should bob)
      (should (equal (taut-user-username bob) "bob"))
      (should (eq (taut-user-presence bob) 'away)))))

(ert-deftest taut-api-fetch-channels-test ()
  "Test fetching and registering channels (including public, private, and DMs)."
  (taut-model-clear-all)
  ;; Add mock user for DM mapping
  (taut-model-add-user (make-taut-user :id "U_ALICE" :username "alice" :real-name "Alice Smith"))
  
  (cl-letf (((symbol-function 'taut-api--request)
             (lambda (endpoint &optional _params _method _apptoken)
               (cond
                ((equal endpoint "users.conversations")
                 '((ok . t)
                   (channels . (((id . "C_GEN")
                                 (name . "general")
                                 (is_channel . t)
                                 (is_private . nil)
                                 (is_member . t))
                                ((id . "C_PRIV")
                                 (name . "secret-sauce")
                                 (is_channel . nil)
                                 (is_private . t)
                                 (is_member . t))
                                ((id . "C_DM_ALICE")
                                 (user . "U_ALICE")
                                 (is_im . t)
                                 (is_member . t))))))))))
    (taut-api-fetch-channels)
    (let ((gen (taut-model-get-channel "C_GEN"))
          (priv (taut-model-get-channel "C_PRIV"))
          (dm (taut-model-get-channel "C_DM_ALICE")))
      (should gen)
      (should (equal (taut-channel-name gen) "general"))
      (should (eq (taut-channel-type gen) 'public))
      
      (should priv)
      (should (equal (taut-channel-name priv) "secret-sauce"))
      (should (eq (taut-channel-type priv) 'private))
      
      (should dm)
      (should (equal (taut-channel-name dm) "alice"))
      (should (eq (taut-channel-type dm) 'dm)))))

(ert-deftest taut-api-fetch-channels-scope-retry-test ()
  "Test that missing scope (e.g. private channel) triggers a retry request."
  (taut-model-clear-all)
  (let ((requests nil))
    (cl-letf (((symbol-function 'taut-api--request)
               (lambda (endpoint params method &optional apptoken)
                 (push (list endpoint (cdr (assoc 'types params)) method) requests)
                 ;; First call throws error
                 (if (= (length requests) 1)
                     (error "Slack API Error (users.conversations): missing_scope")
                   ;; Second call succeeds
                   '((ok . t)
                     (channels . (((id . "C_GEN")
                                   (name . "general")
                                   (is_channel . t)
                                   (is_member . t)))))))))
      (taut-api-fetch-channels)
      ;; Assert we made exactly two requests: the first with private channels, the second without
      (should (= (length requests) 2))
      (should (equal (nth 1 requests) '("users.conversations" "public_channel,private_channel,im,mpim" "GET")))
      (should (equal (nth 0 requests) '("users.conversations" "public_channel,im,mpim" "GET")))
      (should (taut-model-get-channel "C_GEN")))))

(ert-deftest taut-api-fetch-history-join-test ()
  "Test fetching history joins the channel if not already in it."
  (taut-model-clear-all)
  (let ((chan (make-taut-channel :id "C_DEV" :name "development" :type 'public)))
    (taut-model-add-channel chan)
    (let ((actions nil))
      (cl-letf (((symbol-function 'taut-api--request)
                 (lambda (endpoint params method &optional _apptoken)
                   (push (list endpoint method) actions)
                   (cond
                    ((and (equal endpoint "conversations.history") (= (length actions) 1))
                     (error "Slack API Error (conversations.history): not_in_channel"))
                    ((equal endpoint "conversations.join")
                     '((ok . t)))
                    ((and (equal endpoint "conversations.history") (= (length actions) 3))
                     '((ok . t)
                       (messages . (((ts . "1688460000.0001")
                                     (user . "U_ALICE")
                                     (text . "Hello development!"))))))
                    ((equal endpoint "conversations.info")
                     '((ok . t)
                       (channel . ((last_read . "1688450000.0000")))))))))
        (taut-api-fetch-history "C_DEV")
        ;; Verify operations happened in order: history (fail) -> join -> history (success)
        (should (= (length actions) 4)) ; including info fetch
        (should (equal (nth 3 actions) '("conversations.history" "GET")))
        (should (equal (nth 2 actions) '("conversations.join" "POST")))
        (should (equal (nth 1 actions) '("conversations.history" "GET")))
        
        ;; Verify message is added to database
        (let ((msgs (taut-model-get-messages "C_DEV")))
          (should (= (length msgs) 1))
          (should (equal (taut-message-text (car msgs)) "Hello development!")))))))

(ert-deftest taut-api-get-or-fetch-channel-test ()
  "Test that taut-api-get-or-fetch-channel retrieves cached channels, or fetches on-demand."
  (taut-model-clear-all)
  (let ((taut-bot-token "mock-token"))
    ;; Case 1: Channel is already in cache
    (let ((c-general (make-taut-channel :id "C_GENERAL" :name "general" :type 'public)))
      (taut-model-add-channel c-general)
      (should (equal (taut-api-get-or-fetch-channel "C_GENERAL") c-general)))
    
    ;; Case 2: Channel is not in cache, fetch it on-demand
    (let ((requests nil))
      (cl-letf (((symbol-function 'taut-api--request)
                 (lambda (endpoint params method &optional _apptoken)
                   (push (list endpoint params method) requests)
                   '((ok . t)
                     (channel . ((id . "C_PRIVATE_FE")
                                 (name . "private-feed")
                                 (is_private . t)
                                 (unread_count . 5)
                                 (unread_count_display_messages . 2)
                                 (topic . ((value . "Topics!")))
                                 (purpose . ((value . "Purposes!")))))))))
        (let ((chan (taut-api-get-or-fetch-channel "C_PRIVATE_FE")))
          (should chan)
          (should (equal (taut-channel-id chan) "C_PRIVATE_FE"))
          (should (equal (taut-channel-name chan) "private-feed"))
          (should (eq (taut-channel-type chan) 'private))
          (should (= (taut-channel-unread-count chan) 5))
          (should (= (taut-channel-mention-count chan) 2))
          (should (equal (taut-channel-topic chan) "Topics!"))
          (should (equal (taut-channel-purpose chan) "Purposes!"))
          
          ;; Verify cache population
          (should (equal (taut-model-get-channel "C_PRIVATE_FE") chan))
          
          ;; Verify request payload
          (should (= (length requests) 1))
          (should (equal (car requests) '("conversations.info" ((channel . "C_PRIVATE_FE")) "GET"))))))))

(ert-deftest taut-api-channel-lifecycle-test ()
  "Test the channel lifecycle API functions: create, invite, kick, set-topic, archive."
  (taut-model-clear-all)
  (let ((taut-bot-token "mock-token")
        (requests nil))
    (cl-letf (((symbol-function 'taut-api--request)
               (lambda (endpoint params method &optional _apptoken)
                 (push (list endpoint params method) requests)
                 (cond
                  ((equal endpoint "conversations.create")
                   '((ok . t)
                     (channel . ((id . "C_NEW")
                                 (name . "new-channel")
                                 (is_private . nil)))))
                  ((equal endpoint "conversations.info")
                   '((ok . t)
                     (channel . ((id . "C_NEW")
                                 (name . "new-channel")
                                 (is_private . nil)))))
                  (t
                   '((ok . t)))))))
      
      ;; 1. Test create channel
      (let ((res (taut-api-create-channel "new-channel" nil)))
        (should (equal (cdr (assoc 'id (cdr (assoc 'channel res)))) "C_NEW"))
        ;; Verify conversations.create request was made
        (should (member '("conversations.create" ((name . "new-channel") (is_private . nil)) "POST") requests)))

      ;; 2. Test invite to channel
      (setq requests nil)
      (let ((res (taut-api-invite-to-channel "C_NEW" '("U1" "U2"))))
        (should (cdr (assoc 'ok res)))
        (should (equal (car requests) '("conversations.invite" ((channel . "C_NEW") (users . "U1,U2")) "POST"))))

      ;; 3. Test kick from channel
      (setq requests nil)
      (let ((res (taut-api-kick-from-channel "C_NEW" "U1")))
        (should (cdr (assoc 'ok res)))
        (should (equal (car requests) '("conversations.kick" ((channel . "C_NEW") (user . "U1")) "POST"))))

      ;; 4. Test set topic
      (setq requests nil)
      (let ((c (make-taut-channel :id "C_NEW" :name "new-channel" :type 'public)))
        (taut-model-add-channel c)
        (let ((res (taut-api-set-channel-topic "C_NEW" "Exciting topic!")))
          (should (cdr (assoc 'ok res)))
          (should (equal (car requests) '("conversations.setTopic" ((channel . "C_NEW") (topic . "Exciting topic!")) "POST")))
          (should (equal (taut-channel-topic (taut-model-get-channel "C_NEW")) "Exciting topic!"))))

      ;; 5. Test archive channel
      (setq requests nil)
      (let ((res (taut-api-archive-channel "C_NEW")))
        (should (cdr (assoc 'ok res)))
        (should (equal (car requests) '("conversations.archive" ((channel . "C_NEW")) "POST")))
        ;; Check channel is removed from model
        (should-not (taut-model-get-channel "C_NEW"))))))

(ert-deftest taut-api-get-channel-members-test ()
  "Test retrieving channel members user IDs."
  (let ((taut-bot-token "mock-token")
        (requests nil))
    (cl-letf (((symbol-function 'taut-api--request)
               (lambda (endpoint params method &optional _apptoken)
                 (push (list endpoint params method) requests)
                 '((ok . t)
                   (members . ("U_ALICE" "U_BOB"))))))
      (let ((res (taut-api-get-channel-members "C_DEV")))
        (should (equal res '("U_ALICE" "U_BOB")))
        (should (equal (car requests) '("conversations.members" ((channel . "C_DEV")) "GET")))))))

(provide 'test-taut-api)
;;; test-taut-api.el ends here

;;; test-runner.el --- Main test runner for Taut -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bunny Lushington
;; Author: bunny@bapi.us

;;; Commentary:
;; This script initializes the test environment for Taut, adding the source
;; and test directories to the load-path, loading all Taut modules, loading
;; all test files, and executing the ERT test suite.

;;; Code:

(require 'cl-lib)

;; Add current and test directories to the load-path
(let ((default-directory (file-name-directory (or load-file-name (buffer-file-name) default-directory))))
  (add-to-list 'load-path (expand-file-name ".." default-directory))
  (add-to-list 'load-path default-directory))

;; Automatically load straight.el packages in batch mode (for websocket, transient, etc.)
(let ((straight-build-dir (expand-file-name "~/.emacs.d/straight/build/")))
  (when (file-directory-p straight-build-dir)
    (dolist (dir (directory-files straight-build-dir t "^[a-zA-Z0-9]"))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; Load all core source files
(require 'taut-model)
(require 'taut-api)
(require 'taut-cache)
(require 'taut-socket)
(require 'taut-sidebar)
(require 'taut-inbox)
(require 'taut-message)
(require 'taut-thread)
(require 'taut-transient)
(require 'taut)

;; Discover and load all test-*.el files in the test directory
(let* ((test-dir (file-name-directory (or load-file-name (buffer-file-name) default-directory)))
       (test-files (directory-files test-dir t "^test-.*\\.el$")))
  (dolist (file test-files)
    (unless (string-suffix-p "test-runner.el" file)
      (load file nil t))))

(provide 'test-runner)
;;; test-runner.el ends here

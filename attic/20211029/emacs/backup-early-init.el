;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Enable init profiling
;; (require 'profiler)
;; (profiler-start 'cpu)
;; (add-hook 'after-init-hook #'profiler-report)

(with-eval-after-load 'cus-edit
 ;; Disable the custom file (cus-edit).
 (setq custom-file "/dev/null"))

(with-eval-after-load 'emacs
 ;; Properly style emacs before the UI shows up.
 (setq default-frame-alist '((width . 160) (height . 60)))

 ;; A big contributor to startup time is garbage collection.
 (setq gc-cons-threshold (* 100 1024 1024))
 (setq gc-cons-percentage 0.8)

 ;; Prevent an early unstyled Emacs by handling UI elements.
 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (set-scroll-bar-mode nil)
 (set-fringe-style '(8 . 8))

 ;; Disable tramp when loading .el and .elc files.
 (setq file-name-handler-alist nil)

 ;; This slows down normal operation by trying to handle taller-than-normal lines.
 (setq auto-window-vscroll nil)

 ;; Frame-related improvements.
 (setq frame-resize-pixelwise t)
 (setq frame-title-format "%b - emacs")

 ;; Resizing the Emacs frame can be an expensive part of changing the font. Disable it and
 ;; easily improve the startup times with fonts that are larger than the system's default.
 (setq frame-inhibit-implied-resize t)

 ;; Improves text rendering performance.
 (setq bidi-paragraph-direction 'left-to-right)
 (setq bidi-inhibit-bpa t))

(with-eval-after-load 'vc-hooks
 ;; Only handle Git repositories.
 (setq vc-handled-backends '(Git))

 ;; Disable version control when opening files.
 (remove-hook 'find-file-hook #'vc-refresh-state))

;; We will disable auto-save, so disable the filename prefix.
(setq auto-save-list-file-prefix nil)

;; Ignore X resources. Its settings are redundant with the other settings in this file and
;; can conflict with later config (particularly where the cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Native compilation.
(when (>= emacs-major-version 28)
 (with-eval-after-load 'native-comp
  (defvar comp-deferred-compilation t)
  (setq comp-deferred-compilation t)))

;; Run the GC after some idleness.
(run-with-idle-timer 3 t #'garbage-collect)

(with-eval-after-load 'package
 ;; Quickstart the package manager.
 (setq package-quickstart t))

(with-eval-after-load 'url-vars
 ;; Url settings needed for the package manager.
 (setq url-privacy-level 'high)
 (setq url-proxy-services '(("no_proxy" . "127.0.0.1"))))

;;; early-init.el ends here

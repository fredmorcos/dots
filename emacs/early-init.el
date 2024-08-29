;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Enable init profiling
;; (require 'profiler)
;; (profiler-start 'cpu)
;; (add-hook 'after-init-hook #'profiler-report)

;;; Use Packages

(use-package use-package
 :ensure nil
 :defer t

 :custom
 ;; (use-package-compute-statistics t)
 ;; (use-package-verbose t)
 (use-package-expand-minimally t)
 (use-package-hook-name-suffix nil))

;; (use-package emacs
;;  :ensure nil
;;  :defer t

;;  :custom
;;  (debug-on-error t))

;;; Garbage Collector

(use-package emacs
 :ensure nil
 :defer t

 :custom
 ;; A big contributor to startup time is garbage collection.
 (gc-cons-threshold (* 100 1024 1024))
 (gc-cons-percentage 0.8)

 :config
 ;; Run the GC after 5 seconds of idleness.
 (run-with-idle-timer 5 t #'garbage-collect))

;;; Configuration System

(use-package cus-edit
 :ensure nil
 :defer t

 :custom
 (custom-file (make-temp-file "emacs-custom-")))

;;; UI

(use-package emacs
 :ensure nil
 :defer t

 :custom
 ;; Skip redisplays.
 (redisplay-skip-fontification-on-input t)
 (redisplay-skip-initial-frame t)

 ;; Frame-related improvements.
 (frame-resize-pixelwise t)
 (frame-title-format "%b - emacs")

 ;; Resizing the Emacs frame can be a terribly expensive part of changing the font. By
 ;; inhibiting this, we easily halve startup times with fonts that are larger than the
 ;; system default.
 (frame-inhibit-implied-resize t))

(use-package frame
 :ensure nil
 :defer t

 :config
 (modify-all-frames-parameters
  '(;; (width . 160)
    ;; (height . 60)
    ;; (alpha-background . 95)
    (background-color . "Gray97")
    (fullscreen . maximized)
    (use-frame-synchronization . t))))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 ;; Ignore X resources.
 (inhibit-x-resources t)

 :config
 ;; Ignore X resources by disabling its function; its settings would be redundant with the
 ;; other settings in this file and can conflict with later configuration (particularly
 ;; where the cursor color is concerned).
 (advice-add #'x-apply-session-resources :override #'ignore))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 (inhibit-startup-screen t)
 (inhibit-startup-message t)
 (inhibit-startup-buffer-menu t)
 (inhibit-startup-echo-area-message user-real-login-name)
 (initial-scratch-message nil)
 (initial-major-mode 'fundamental-mode)

 :config
 ;; Don't show message in the echo area at startup.
 (fset 'display-startup-echo-area-message 'ignore))

(use-package emacs
 :ensure nil
 :defer t

 :config
 (load-theme 'modus-operandi))

(use-package menu-bar
 :ensure nil
 :defer t

 :custom
 (menu-bar-mode nil))

(use-package tool-bar
 :ensure nil
 :defer t

 :custom
 (tool-bar-mode nil))

(use-package scroll-bar
 :ensure nil
 :defer t

 :custom
 (scroll-bar-mode nil))

(use-package simple
 :ensure nil
 :defer t

 :custom
 (line-number-mode t)
 (column-number-mode t)
 (size-indication-mode t))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 (column-number-indicator-zero-based nil)
 (mode-line-position-column-format '(" C%C"))
 (mode-line-compact 'long))

(use-package frame
 :ensure nil
 :defer t

 :custom
 (blink-cursor-mode nil))

;;; UX

(use-package warnings
 :ensure nil
 :defer t

 :custom
 ;; Stop the warnings buffer from popping up, but still log warnings.
 (warning-minimum-level :emergency))

(use-package comp
 :ensure nil
 :defer t

 :custom
 ;; Silence native compilation warnings.
 (native-comp-async-report-warnings-errors 'silent))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 ;; Avoid graphical dialog boxes
 (use-dialog-box nil)

 ;; Respond to yes/no questions using Y/N
 (use-short-answers t))

;;; Performance

(use-package emacs
 :ensure nil
 :defer t

 :custom
 ;; This slows down normal operation.
 (auto-window-vscroll nil))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 ;; Improve text rendering performance.
 (bidi-paragraph-direction 'left-to-right)
 (bidi-inhibit-bpa t))

(use-package vc-hooks
 :ensure nil
 :defer t

 :custom
 ;; Only use Git as version control.
 (vc-handled-backends '(Git)))

(use-package files
 :ensure nil
 :defer t

 :config
 ;; Disable version control when opening files.
 (remove-hook 'find-file-hook #'vc-refresh-state))

(use-package emacs
 :ensure nil
 :defer t

 ;; :custom
 ;; Disable tramp when loading .el and .elc files. That doesn't work correctly. It instead
 ;; completely disables support for documentation linking to show source code available in
 ;; .el and .elc files.
 ;; (file-name-handler-alist nil)

 :config
 ;; Disable tramp.
 (remove-hook 'after-init-hook #'tramp-register-archive-autoload-file-name-handler))

(use-package emacs
 :ensure nil
 :defer t

 :config
 ;; LSP speed improvement.
 (setenv "LSP_USE_PLISTS" "true"))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 ;; Improves LSP performance.
 (setq-default read-process-output-max (* 1024 1024)))

;;; Packages

(use-package package
 :ensure nil
 :defer t

 :defines
 package-archives

 :config
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

 :custom
 (package-native-compile t))

(use-package url-vars
 :ensure nil
 :defer t

 :custom
 ;; Use this when unsetting any proxies for localhost.
 ;; url-proxy-services '(("no_proxy" . "127.0.0.1"))
 (url-privacy-level 'paranoid))

(provide 'early-init)
;;; early-init.el ends here

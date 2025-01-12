;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Use Package

(use-package use-package
 :ensure nil
 :defer t

 :custom
 (use-package-expand-minimally t)
 (use-package-hook-name-suffix nil)
 (use-package-always-ensure t)
 (use-package-always-defer t))

(use-package use-package
 :disabled
 :ensure nil

 :custom
 (use-package-compute-statistics t)
 (use-package-verbose t))

;;; Emacs Debugging & Profiling

(use-package emacs
 :disabled
 :ensure nil
 :defer t

 :custom
 (debug-on-error t))

(use-package profiler
 :disabled
 :ensure nil
 :defer t

 :init
 (profiler-start 'cpu)

 :hook
 (after-init-hook . profiler-report))

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
 (frame-inhibit-implied-resize t)

 :custom-face
 (mode-line ((t (:box "#bcbcbc")))))

(use-package frame
 :ensure nil
 :defer t

 :config
 (modify-all-frames-parameters
  '((background-color . "Gray97")
    (fullscreen . maximized))))

(use-package emacs
 :ensure nil
 :defer t

 :config
 (load-theme 'modus-operandi))

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
 ;; Don't show messages in the echo area at startup.
 (fset 'display-startup-echo-area-message 'ignore))

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

(use-package comp
 :ensure nil
 :defer t

 :custom
 ;; Silence native compilation warnings.
 (native-comp-async-report-warnings-errors 'silent)
 (native-comp-async-query-on-exit t))

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
 (max-lisp-eval-depth 10000))

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

 :config
 ;; Disable tramp.
 (remove-hook 'after-init-hook #'tramp-register-archive-autoload-file-name-handler)
 (remove-hook 'after-init-hook #'pgtk-use-im-context-handler))

;;; Packages

(use-package url-vars
 :ensure nil
 :defer t

 :custom
 ;; Use this when unsetting any proxies for localhost.
 ;; url-proxy-services '(("no_proxy" . "127.0.0.1"))
 (url-privacy-level 'paranoid))

(use-package package
 :ensure nil
 :defer t
 :defines package-archives

 :config
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

 :custom
 (package-native-compile t))

(provide 'early-init)
;;; early-init.el ends here

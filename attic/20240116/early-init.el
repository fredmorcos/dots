;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Enable init profiling
;; (require 'profiler)
;; (profiler-start 'cpu)
;; (add-hook 'after-init-hook #'profiler-report)

(load-theme 'modus-operandi)

(use-package use-package
 :ensure nil
 :custom
 (use-package-always-defer t)
 (use-package-expand-minimally t))

(use-package package
 :ensure nil
 :config
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
 :custom
 (package-native-compile t))

(use-package use-package-ensure
 :ensure nil
 :custom
 (use-package-always-ensure t))

(use-package comp
 :ensure nil
 :custom
 (native-comp-async-report-warnings-errors 'silent))

(use-package cus-edit
 :ensure nil
 :custom
 (custom-file "/dev/null"))

(use-package warnings
 :ensure nil
 :custom
 (warning-minimum-level :emergency "Log warnings without popping the buffer"))

(use-package frame
 :ensure nil
 :config
 (modify-all-frames-parameters
  '((fullscreen . maximized)
    (use-frame-synchronization . t))))

(use-package tool-bar
 :ensure nil
 :custom
 (tool-bar-mode nil))

(use-package scroll-bar
 :ensure nil
 :custom
 (scroll-bar-mode nil))

(use-package emacs
 :ensure nil
 :config
 ;; Run the GC after 5 seconds of idleness.
 (run-with-idle-timer 5 t #'garbage-collect)
 :custom
 (menu-bar-mode nil)
 ;; Frame-related improvements.
 (frame-title-format "%b - emacs")
 (frame-resize-pixelwise t)
 ;; Resizing the Emacs frame can be a terribly expensive part of changing the font. By
 ;; inhibiting this, we easily halve startup times with fonts that are larger than the
 ;; system default.
 (frame-inhibit-implied-resize t)
 (redisplay-skip-fontification-on-input t "Skip redisplays")
 (redisplay-skip-initial-frame t "Skip redisplays")
 ;; A big contributor to startup time is garbage collection.
 (gc-cons-threshold (* 100 1024 1024) "Almost disable the GC")
 (gc-cons-percentage 0.8 "Almost disable the GC")
 (auto-window-vscroll nil "This slows down normal operation")
 ;; Improves text rendering performance.
 (bidi-paragraph-direction 'left-to-right)
 (bidi-inhibit-bpa t))

(use-package simple
 :ensure nil
 :custom
 (line-number-mode t)
 (column-number-mode t)
 (size-indication-mode t))

;;; early-init.el ends here

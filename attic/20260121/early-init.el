;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Quality of Life

(eval-and-compile
 (push "~/Workspace/dots/emacs/" load-path)
 (require 'init-macros))

;;; Use Package

(init/after 'use-package-core
 (setopt
  use-package-expand-minimally t
  use-package-hook-name-suffix nil
  use-package-always-defer t))

(init/after 'use-package-ensure
 (setopt use-package-always-ensure t))

(defvar init/use-package-statistics :disabled)
(when (not (eq init/use-package-statistics :disabled))
 (init/after 'use-package-core
  (setopt
   use-package-compute-statistics t
   use-package-verbose t)))

;;; Debugging

(defvar init/emacs-debugging :disabled)
(when (not (eq init/emacs-debugging :disabled))
 (init/after 'emacs
  (setopt debug-on-error t)))

;;; Profiling

(defvar init/emacs-profiling :disabled)
(when (not (eq init/emacs-profiling :disabled))
 (profiler-start 'cpu)
 (init/after 'startup
  (init/autoload profiler-report "profiler")
  (add-hook 'after-init-hook 'profiler-report)))

;;; Garbage Collector

(init/after 'emacs
 ;; Run the GC after 5 seconds of idleness.
 (init/autoload run-with-idle-timer "timer")
 (run-with-idle-timer 5 t #'garbage-collect)
 (setopt
  ;; A big contributor to startup time is garbage collection.
  gc-cons-threshold (* 100 1024 1024)
  gc-cons-percentage 0.8))

;;; Configuration System

(init/after 'cus-edit
 (setopt custom-file (make-temp-file "emacs-custom-")))

;;; User Interface

(init/after 'emacs
 (setopt
  ;; Skip redisplays.
  redisplay-skip-fontification-on-input t
  redisplay-skip-initial-frame t
  ;; Frame-related improvements.
  frame-resize-pixelwise t
  frame-title-format "%b - emacs"
  ;; Resizing the Emacs frame can be a terribly expensive part of changing the font. By
  ;; inhibiting this, we easily halve startup times with fonts that are larger than the
  ;; system default.
  frame-inhibit-implied-resize t)

 (load-theme 'modus-operandi)
 (init/face mode-line :box "#c8c8c8")
 (init/face mode-line-inactive :box "#e6e6e6"))

(init/after 'frame
 (modify-all-frames-parameters
  '((background-color . "Gray97")
    (fullscreen . maximized))))

;; Actually should be (init/after 'startup ...) but the startup.el package file does not
;; (provide 'startup) so (with-eval-after-load 'startup ...) does not work.
(init/after 'emacs
 (setopt
  inhibit-startup-screen t
  inhibit-startup-message t
  inhibit-startup-buffer-menu t
  inhibit-startup-echo-area-message user-real-login-name
  startup-screen-inhibit-startup-screen t
  initial-scratch-message nil
  initial-major-mode 'fundamental-mode))

;; Actually should be (init/after 'startup ...) but the startup.el package file does not
;; (provide 'startup) so (with-eval-after-load 'startup ...) does not work.
(init/after 'emacs
 ;; Don't show messages in the echo area at startup.
 (fset 'display-startup-echo-area-message 'ignore))

(init/after 'menu-bar
 (setopt menu-bar-mode nil))

(init/after 'tool-bar
 (setopt tool-bar-mode nil))

(init/after 'scroll-bar
 (setopt scroll-bar-mode nil))

(init/after 'simple
 (setopt
  line-number-mode t
  column-number-mode t
  size-indication-mode t))

(init/after 'emacs
 (setopt
  column-number-indicator-zero-based nil
  mode-line-position-column-format '(" C%C")
  mode-line-compact 'long))

(init/after 'frame
 (setopt blink-cursor-mode nil))

;;; User Experience

(init/after 'comp
 (setopt
  ;; Silence native compilation warnings.
  native-comp-async-report-warnings-errors 'silent
  native-comp-async-query-on-exit t))

(init/after 'emacs
 (setopt
  ;; Avoid graphical dialog boxes
  use-dialog-box nil
  ;; Respond to yes/no questions using Y/N
  use-short-answers t))

;;; Performance

(init/after 'emacs
 (setopt
  ;; This slows down normal operation.
  auto-window-vscroll nil

  ;; Improve rendering performance at the expense of support for left-to-right languages.
  bidi-paragraph-direction 'left-to-right
  bidi-inhibit-bpa t

  ;; Only use Git as version control.
  vc-handled-backends '(Git)))

;; Actually should be (init/after 'startup ...) but the startup.el package file does not
;; (provide 'startup) so (with-eval-after-load 'startup ...) does not work.
(init/after 'emacs
  ;; Disable input contexts (for Windows).
  (init/after 'term/pgtk-win
   (remove-hook 'after-init-hook #'pgtk-use-im-context-handler)))

(init/after 'files
 ;; Disable transparent file encryption support.
 (init/after 'epa-hook
  (remove-hook 'find-file-hook #'epa-file-find-file-hook))

 (init/after 'vc-hooks
  ;; Disable version control when opening files.
  (remove-hook 'find-file-hook #'vc-refresh-state)))

;;; Packages

(init/after 'url-vars
 (setopt
  ;; Use this when unsetting any proxies for localhost.
  ;; url-proxy-services '(("no_proxy" . "127.0.0.1"))
  url-privacy-level 'paranoid))

(init/after 'package
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
 (setopt
  package-native-compile t
  ;; Highest number gets priority (what is not mentioned has priority 0)
  package-archive-priorities '(("gnu" . 3) ("melpa" . 2) ("nongnu" . 1))))

(provide 'early-init)
;;; early-init.el ends here

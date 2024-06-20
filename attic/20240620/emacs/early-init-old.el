;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Enable init profiling
;; (require 'profiler)
;; (profiler-start 'cpu)
;; (add-hook 'after-init-hook #'profiler-report)

(eval-when-compile
 (defconst emacs-dots-dir "~/Workspace/dots/emacs/")
 (push emacs-dots-dir load-path))
(require 'init-macros)

(im/config cus-edit
 (im/after
  (setopt custom-file (make-temp-file "emacs-custom-"))))

(im/config emacs
 (im/after
  (setopt
   ;; A big contributor to startup time is garbage collection.
   gc-cons-threshold (* 100 1024 1024)
   gc-cons-percentage 0.8

   ;; Skip redisplays.
   redisplay-skip-fontification-on-input t
   redisplay-skip-initial-frame t

   ;; This slows down normal operation.
   auto-window-vscroll nil

   ;; Frame-related improvements.
   frame-resize-pixelwise t
   frame-title-format "%b - emacs"

   ;; Resizing the Emacs frame can be a terribly expensive part of
   ;; changing the font. By inhibiting this, we easily halve startup
   ;; times with fonts that are larger than the system default.
   frame-inhibit-implied-resize t

   ;; Improves text rendering performance.
   bidi-paragraph-direction 'left-to-right
   bidi-inhibit-bpa t

   ;; Ignore X resources.
   inhibit-x-resources t

   ;; Startup.
   inhibit-startup-screen t
   inhibit-startup-message t
   inhibit-startup-buffer-menu t
   inhibit-startup-echo-area-message user-real-login-name
   initial-scratch-message nil
   initial-major-mode 'fundamental-mode)))

(im/after warnings
 ;; Stop the warnings buffer from popping up.
 (setopt warning-minimum-level :emergency))

(im/after frame
 (modify-all-frames-parameters
  '((fullscreen . maximized)
    (use-frame-synchronization . t))))

;; Ignore X resources; its settings would be redundant with the other
;; settings in this file and can conflict with later config
;; (particularly where the cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(im/after menu-bar
 (setopt menu-bar-mode nil))

(im/after tool-bar
 (setopt tool-bar-mode nil))

(im/after scroll-bar
 (setopt scroll-bar-mode nil))

(im/after simple
 (setopt
  line-number-mode t
  column-number-mode t
  size-indication-mode t))

(load-theme 'modus-operandi)

(im/after vc-hooks
 ;; Only use Git as version control.
 (setopt vc-handled-backends '(Git))

 ;; Disable version control when opening files.
 (remove-hook 'find-file-hook #'vc-refresh-state))

(im/after comp
 ;; Silence native compilation warnings.
 (setopt native-comp-async-report-warnings-errors 'silent))

;; Run the GC after 5 seconds of idleness.
(run-with-idle-timer 5 t #'garbage-collect)

(im/after package
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
 (setopt package-native-compile t))

(im/after url-vars
 (setopt url-privacy-level 'paranoid))

;; LSP speed improvement.
(setenv "LSP_USE_PLISTS" "true")

;; Don't show message in the echo area at startup.
(fset 'display-startup-echo-area-message 'ignore)

(im/after loaddefs
 ;; Disable tramp.
 (remove-hook 'after-init-hook #'tramp-register-archive-autoload-file-name-handler))

;;; early-init.el ends here

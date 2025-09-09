;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
 (defconst emacs-dots-dir "~/Workspace/dots/emacs/")
 (push emacs-dots-dir load-path)
 (require 'init-macros))

(im/config "Use Package"
 (im/after use-package-core
  (setopt
   use-package-expand-minimally t
   use-package-hook-name-suffix nil
   use-package-always-defer t))

 (im/after use-package-ensure
  (setopt
   use-package-always-ensure t)))

(im/config "Use Package Debugging" :disabled
 (im/after use-package-core
  (setopt
   use-package-compute-statistics t
   use-package-verbose t)))

(im/config "Debugging" :disabled
 (im/after emacs
  (setopt
   debug-on-error t)))

(im/config "Profiling" :disabled
 (profiler-start 'cpu)
 (im/after startup
  (im/hook after-init-hook profiler-report "profiler")))

(im/config "Garbage Collector"
 (im/after emacs
  ;; Run the GC after 1 second of idleness.
  (run-with-idle-timer 1 t #'garbage-collect)
  (setopt
   ;; A big contributor to startup time is garbage collection.
   gc-cons-threshold (* 100 1024 1024)
   gc-cons-percentage 0.8)))

(im/config "Configuration System"
 (im/after cus-edit
  (setopt custom-file (make-temp-file "emacs-custom-"))))

(im/config "User Interface"
 (im/after emacs
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
  (im/face mode-line :box "#c8c8c8")
  (im/face mode-line-inactive :box "#e6e6e6"))

 (im/after frame
  (modify-all-frames-parameters
   '((background-color . "Gray97")
     (fullscreen . maximized))))

 (im/after emacs
  (setopt
   inhibit-startup-screen t
   inhibit-startup-message t
   inhibit-startup-buffer-menu t
   inhibit-startup-echo-area-message user-real-login-name
   initial-scratch-message nil
   initial-major-mode 'fundamental-mode)

  ;; Don't show messages in the echo area at startup.
  (fset 'display-startup-echo-area-message 'ignore))

 (im/after menu-bar (setopt menu-bar-mode nil))
 (im/after tool-bar (setopt tool-bar-mode nil))
 (im/after scroll-bar (setopt scroll-bar-mode nil))

 (im/after simple
  (setopt
   line-number-mode t
   column-number-mode t
   size-indication-mode t))

 (im/after emacs
  (setopt
   column-number-indicator-zero-based nil
   mode-line-position-column-format '(" C%C")
   mode-line-compact 'long))

 (im/after frame (setopt blink-cursor-mode nil)))

(im/config "User Experience"
 (im/after comp
  (setopt
   ;; Silence native compilation warnings.
   native-comp-async-report-warnings-errors 'silent
   native-comp-async-query-on-exit t))

 (im/after emacs
  (setopt
   ;; Avoid graphical dialog boxes
   use-dialog-box nil
   ;; Respond to yes/no questions using Y/N
   use-short-answers t)))

(im/config "Performance"
 (im/after emacs
  (setopt
   ;; When evaluating deep ELisp recursions.
   ;; max-lisp-eval-depth 10000

   ;; This slows down normal operation.
   auto-window-vscroll nil

   ;; Improve rendering performance at the expense of support for left-to-right languages.
   bidi-paragraph-direction 'left-to-right
   bidi-inhibit-bpa t

   ;; Only use Git as version control.
   vc-handled-backends '(Git)))

 ;; Actually should be (im/after startup ...) but the startup.el package file does not
 ;; (provide 'startup) so (with-eval-after-load startup ...) does not work.
 (im/after emacs
  ;; ;; Disable tramp.
  ;; (im/after tramp-archive
  ;;  (remove-hook 'after-init-hook #'tramp-register-archive-autoload-file-name-handler))

  ;; Disable input contexts (for Windows).
  (im/after term/pgtk-win
   (remove-hook 'after-init-hook #'pgtk-use-im-context-handler)))

 (im/after files
  ;; (im/after subr
  ;;  ;; Disable tramp.
  ;;  (declare-function tramp-set-connection-local-variables-for-buffer "subr")
  ;;  (remove-hook 'find-file-hook #'tramp-set-connection-local-variables-for-buffer))

  ;; Disable transparent file encryption support.
  (im/after epa-hook
   (remove-hook 'find-file-hook #'epa-file-find-file-hook))

  ;; ;; Disable setting correct URL handlers when visiting remote files.
  ;; (im/after url-handlers
  ;;  (remove-hook 'find-file-hook #'url-handlers-set-buffer-mode))

  (im/after vc-hooks
   ;; Disable version control when opening files.
   (remove-hook 'find-file-hook #'vc-refresh-state))))

(im/config "Packages"
 (im/after url-vars
  (setopt
   ;; Use this when unsetting any proxies for localhost.
   ;; url-proxy-services '(("no_proxy" . "127.0.0.1"))
   url-privacy-level 'paranoid))

 (im/after package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (setopt package-native-compile t)))

(provide 'early-init)
;;; early-init.el ends here

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

(im/config garbage-collector
 :init
 ;; Run the GC after 5 seconds of idleness.
 (run-with-idle-timer 5 t #'garbage-collect)
 ;; TODO Use :hook when it's implemented in im/config.
 (add-hook 'emacs-startup-hook
  (lambda ()
   (im/config garbage-collector
    :custom
    (gc-cons-threshold (* 20 1024 1024) "GC frequency"))))
 :custom
 ;; A big contributor to startup time is garbage collection.
 (gc-cons-threshold most-positive-fixnum "Reduce collection frequency")
 (gc-cons-percentage 0.2 "Reduce collection frequency"))

(im/config theme :init (load-theme 'modus-operandi))

(im/config cus-edit
 :custom
 (custom-file (make-temp-file "emacs-custom-")))

(im/config package
 :after
 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
 (add-to-list 'package-selected-packages "setup")
 :custom
 (package-native-compile t))

(im/config warnings :custom (warning-minimum-level :emergency "Disable warnings buffer"))

;; TODO Delete when moved to im/config
(im/config use-package
 :custom
 (use-package-always-defer t)
 (use-package-expand-minimally t))

;; TODO Delete when moved to im/config
(im/config use-package-ensure
 :custom
 (use-package-always-ensure t))

(im/config comp :custom (native-comp-async-report-warnings-errors 'silent))

(im/config frame
 :custom
 ;; Resizing the Emacs frame can be a terribly expensive part of changing the font. By
 ;; inhibiting this, we easily halve startup times with fonts that are larger than the
 ;; system default.
 (frame-inhibit-implied-resize t)
 (frame-title-format "%b - emacs")
 (frame-resize-pixelwise t)
 :init
 (modify-all-frames-parameters
  '((fullscreen . maximized)
    (use-frame-synchronization . t))))

(im/config menu-bar :custom (menu-bar-mode nil))
(im/config tool-bar :custom (tool-bar-mode nil))
(im/config scroll-bar :custom (scroll-bar-mode nil))

(im/config lsp-mode :init (setenv "LSP_USE_PLISTS" "true"))

(im/config display-and-rendering
 :custom
 ;; Frame-related improvements.
 (redisplay-skip-fontification-on-input t "Skip redisplays")
 (redisplay-skip-initial-frame t "Skip redisplays")
 ;; Improves text rendering performance.
 (bidi-paragraph-direction 'left-to-right)
 (bidi-inhibit-bpa t)
 (auto-window-vscroll nil "This slows down normal operation"))

(im/config vc-hooks
 :require
 :custom
 (vc-handled-backends '(Git))
 :after
 ;; TODO Use :remove-hook when it's implemented in im/config.
 (remove-hook 'find-file-hook #'vc-refresh-state))

(im/config simple
 :custom
 (line-number-mode t)
 (column-number-mode t)
 (size-indication-mode t))

(im/config startup
 :init
 (fset 'display-startup-echo-area-message 'ignore)
 :after
 ;; X resources settings would be redundant with the other settings in this file and can
 ;; conflict with later config. Ignore them.
 (advice-add #'x-apply-session-resources :override #'ignore)
 :custom
 (inhibit-startup-screen t)
 (inhibit-startup-message t)
 (inhibit-startup-buffer-menu t)
 (inhibit-startup-echo-area-message t)
 (inhibit-x-resources t)
 (initial-scratch-message nil)
 (initial-major-mode 'fundamental-mode))

(im/config loaddefs
 :after
 ;; Disable tramp.
 (remove-hook 'after-init-hook #'tramp-register-archive-autoload-file-name-handler)
 :custom
 (file-name-handler-alist nil "Disable tramp when loading .el/.elc files"))

;;; early-init.el ends here

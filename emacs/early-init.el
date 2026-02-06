;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
 (push "~/Workspace/dots/emacs/" load-path)
 (require 'init-macros))

(config "Configuration Options"
 (defvar *init/use-package-statistics* :disabled "Whether to enable use-package stats")
 (defvar *init/emacs-debugging* :disabled "Enable debugging startup elisp")
 (defvar *init/emacs-profiling* :disabled "Enable profiling startup"))

(config "Use Package"
 (after 'use-package-core
  (setopt
   use-package-expand-minimally t
   use-package-hook-name-suffix nil
   use-package-always-defer t))

 (unless (eq *init/use-package-statistics* :disabled)
  (after 'use-package-core
   (setopt
    use-package-compute-statistics t
    use-package-verbose t))))

(config "Debugging and Profiling"
 (unless (eq *init/emacs-debugging* :disabled)
  (after 'emacs (setopt debug-on-error t)))

 (unless (eq *init/emacs-profiling* :disabled)
  (add-hook 'after-init-hook 'profiler-report)
  (profiler-start 'cpu)))

(config "Garbage Collection"
 (after 'emacs
  (setopt
   ;; A big contributor to startup time is garbage collection. Increase the thresholds for
   ;; GC to run to 100MB or 80% of memory consumption.
   gc-cons-threshold (* 100 1024 1024)
   gc-cons-percentage 0.8))

 ;; Run the GC after 5 seconds of idleness.
 (run-with-idle-timer 5 t #'garbage-collect))

(config "Customization File"
 (after 'cus-edit (setopt custom-file (make-temp-file "emacs-custom-"))))

(config "Render Performance"
 (after 'emacs
  (setopt
   ;; Resizing the Emacs frame can be an expensive part of changing the font. By
   ;; inhibiting this, we easily halve startup times with fonts that are larger than the
   ;; system default.
   frame-inhibit-implied-resize t
   ;; Skip re-display.
   redisplay-skip-fontification-on-input t
   redisplay-skip-initial-frame t)))

(config "User Interface"
 (after 'emacs
  (setopt
   frame-resize-pixelwise t
   frame-title-format "%b - emacs"))

 (modify-all-frames-parameters
  '((background-color . "Gray94")
    (fullscreen . maximized)
    (font . "Monospace-15")))

 (after 'menu-bar (setopt menu-bar-mode nil))
 (after 'tool-bar (setopt tool-bar-mode nil))
 (after 'scroll-bar (setopt scroll-bar-mode nil))
 (after 'frame (setopt blink-cursor-mode nil))

 (after 'simple
  (setopt
   line-number-mode t
   column-number-mode t
   size-indication-mode t))

 ;; bindings.el
 (after 'emacs
  (setopt
   column-number-indicator-zero-based nil
   mode-line-position-column-format '(" C%C")
   mode-line-compact 'long)))

(config "Theme"
 (load-theme 'modus-operandi)
 (face mode-line :box "#c8c8c8")
 (face mode-line-inactive :box "#e6e6e6"))

(config "User Experience"
 ;; startup.el
 (after 'emacs
  (setopt
   inhibit-startup-screen t
   inhibit-startup-message t
   inhibit-startup-buffer-menu t
   inhibit-startup-echo-area-message user-real-login-name
   startup-screen-inhibit-startup-screen t
   initial-scratch-message nil
   initial-major-mode 'fundamental-mode))

 (after 'emacs
  (setopt
   ;; Avoid graphical dialog boxes
   use-dialog-box nil
   ;; Respond to yes/no questions using Y/N
   use-short-answers t))

 ;; Don't show messages in the echo area at startup.
 (fset 'display-startup-echo-area-message 'ignore))

(config "Native Compilation"
 (after 'comp
  (setopt
   ;; Silence native compilation warnings.
   native-comp-async-report-warnings-errors 'silent
   native-comp-async-query-on-exit t)))

(config "Performance"
 (after 'emacs
  (setopt
   ;; This slows down normal operation.
   auto-window-vscroll nil

   ;; Improve render performance at the expense of support for left-to-right languages.
   bidi-paragraph-direction 'left-to-right
   bidi-inhibit-bpa t

   ;; Only use Git as version control.
   vc-handled-backends '(Git)

   ;; Increase process output read buffer.
   read-process-output-max (* 10 1024 1024)))

 ;; Improve LSP performance.
 (setenv "LSP_USE_PLISTS" "true")
 (autoload 'jsonrpc--log-event "jsonrpc")
 (after 'jsonrpc (fset #'jsonrpc--log-event #'ignore))

 (after 'term/pgtk-win
  ;; Disable input contexts (for Windows).
  (remove-hook 'after-init-hook #'pgtk-use-im-context-handler))

 (after 'files
  (after 'epa-hook
   ;; Disable transparent file encryption support.
   (remove-hook 'find-file-hook #'epa-file-find-file-hook))

  (after 'vc-hooks
   ;; Disable version control when opening files.
   (remove-hook 'find-file-hook #'vc-refresh-state))))

(config "Packages"
 (after 'url-vars
  (setopt
   ;; Use this when unsetting any proxies for localhost.
   ;; url-proxy-services '(("no_proxy" . "127.0.0.1"))
   url-privacy-level 'paranoid))

 (after 'package
  (setopt
   package-install-upgrade-built-in t
   ;; Highest number gets priority (what is not mentioned has priority 0).
   package-archive-priorities '(("gnu" . 3) ("melpa" . 2) ("nongnu" . 1))
   package-native-compile t))

 (after 'package
  (push '("melpa" . "https://melpa.org/packages/") package-archives)))

(provide 'early-init)
;;; early-init.el ends here

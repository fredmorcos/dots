;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (push "~/Workspace/dots/emacs/" load-path)
  (require 'init-macros))

(config "Configuration"
  (defvar *init/use-package-statistics* :disabled)
  (defvar *init/emacs-debugging* :disabled)
  (defvar *init/emacs-profiling* :disabled))

(config "Use Package"
  (after 'use-package-core
    (setopt
      use-package-expand-minimally t
      use-package-hook-name-suffix nil
      use-package-always-defer t))

  (after 'use-package-ensure
    (setopt
      use-package-always-ensure t))

  (when (not (eq *init/use-package-statistics* :disabled))
    (after 'use-package-core
      (setopt
        use-package-compute-statistics t
        use-package-verbose t))))

(config "Debugging"
  (when (not (eq *init/emacs-debugging* :disabled))
    (after 'emacs
      (setopt debug-on-error t))))

(config "Profiling"
  (when (not (eq *init/emacs-profiling* :disabled))
    (profiler-start 'cpu)
    (after 'emacs                       ; Provided by startup.el.
      (autoloads "profiler" 'profiler-report)
      (add-hook 'after-init-hook 'profiler-report))))

(config "Garbage Collection"
  (after 'emacs
    ;; Run the GC after 5 seconds of idleness.
    (autoloads "timer" 'run-with-idle-timer)
    (run-with-idle-timer 5 t #'garbage-collect)
    (setopt
      ;; A big contributor to startup time is garbage collection. Increase the thresholds
      ;; for GC to run to 100MB or 80% of memory consumption.
      gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.8)))

(config "Custom File"
  (after 'cus-edit
    (setopt
      custom-file (make-temp-file "emacs-custom-"))))

(config "User Interface"
  (after 'emacs
    (setopt
      ;; Skip redisplays.
      redisplay-skip-fontification-on-input t
      redisplay-skip-initial-frame t
      ;; Frame-related improvements.
      frame-resize-pixelwise t
      frame-title-format "%b - emacs"
      ;; Resizing the Emacs frame can be an expensive part of changing the font. By
      ;; inhibiting this, we easily halve startup times with fonts that are larger than
      ;; the system default.
      frame-inhibit-implied-resize t)

    (load-theme 'modus-operandi)
    (face mode-line :box "#c8c8c8")
    (face mode-line-inactive :box "#e6e6e6"))

  (after 'frame
    (modify-all-frames-parameters
      '((background-color . "Gray94")
         (fullscreen . maximized)
         (font . "Monospace-15"))))

  (after 'menu-bar
    (setopt
      menu-bar-mode nil))

  (after 'tool-bar
    (setopt
      tool-bar-mode nil))

  (after 'scroll-bar
    (setopt
      scroll-bar-mode nil))

  (after 'simple
    (setopt
      line-number-mode t
      column-number-mode t
      size-indication-mode t))

  (after 'emacs
    (setopt
      column-number-indicator-zero-based nil
      mode-line-position-column-format '(" C%C")
      mode-line-compact 'long))

  (after 'frame
    (setopt
      blink-cursor-mode nil)))

(config "User Experience"
  (after 'emacs                         ; Provided by startup.el.
    (setopt
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t
      inhibit-startup-echo-area-message user-real-login-name
      startup-screen-inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

    ;; Don't show messages in the echo area at startup.
    (fset 'display-startup-echo-area-message 'ignore))

  (after 'comp
    (setopt
     ;; Silence native compilation warnings.
     native-comp-async-report-warnings-errors 'silent
     native-comp-async-query-on-exit t))

  (after 'emacs
    (setopt
     ;; Avoid graphical dialog boxes
     use-dialog-box nil
     ;; Respond to yes/no questions using Y/N
     use-short-answers t)))

(config "Performance"
  (after 'emacs
    (setopt
      ;; This slows down normal operation.
      auto-window-vscroll nil

      ;; Improve render performance at the expense of support for left-to-right languages.
      bidi-paragraph-direction 'left-to-right
      bidi-inhibit-bpa t

      ;; Only use Git as version control.
      vc-handled-backends '(Git)))

  (after 'emacs                         ; Provided by startup.el.
    (after 'term/pgtk-win
      ;; Disable input contexts (for Windows).
      (remove-hook 'after-init-hook #'pgtk-use-im-context-handler)))

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
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (setopt
      ;; Highest number gets priority (what is not mentioned has priority 0).
      package-archive-priorities '(("gnu" . 3)
                                    ("melpa" . 2)
                                    ("nongnu" . 1))
      package-native-compile t)))

(provide 'early-init)
;;; early-init.el ends here

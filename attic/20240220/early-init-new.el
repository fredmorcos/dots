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

(use-package warnings
 :ensure nil
 :custom
 (warning-minimum-level :emergency "Log warnings without popping the buffer"))

(use-package frame
 :ensure nil
 :config
 (modify-all-frames-parameters '((fullscreen . maximized))))

(use-package emacs
 :ensure nil
 :custom
 (frame-title-format "%b - emacs")
 (tool-bar-style 'image)
 (tool-bar-position 'left))

(use-package simple
 :ensure nil
 :custom
 (line-number-mode t)
 (column-number-mode t)
 (size-indication-mode t))

;;; early-init.el ends here

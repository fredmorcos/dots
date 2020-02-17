;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Enable init profiling
;; (require 'profiler)
;; (profiler-start 'cpu)
;; (add-hook 'after-init-hook #'profiler-report)

(fset 'display-startup-echo-area-message 'ignore)
(run-with-idle-timer 5 t #'garbage-collect)

(setq-default gc-cons-threshold most-positive-fixnum
              gc-cons-percentage 0.6
              custom-file (concat temporary-file-directory "emacs/custom.el")
              file-name-handler-alist nil
              auto-window-vscroll nil
              vc-handled-backends '(Git)
              recentf-auto-cleanup 'never
              after-init-hook nil
              package-enable-at-startup nil
              package--init-file-ensured t
              auto-save-list-file-prefix nil
              frame-inhibit-implied-resize t

              url-privacy-level 'high
              url-proxy-services '(("no_proxy" . "127.0.0.1"))

              use-package-always-defer t
              use-package-always-ensure t
              use-package-expand-minimally t)

;;; early-init.el ends here

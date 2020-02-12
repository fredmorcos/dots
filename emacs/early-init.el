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
              recentf-auto-cleanup 'never)

;;; early-init.el ends here

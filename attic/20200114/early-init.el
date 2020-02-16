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
              auto-save-list-file-prefix nil
              frame-inhibit-implied-resize t)

;; (defconst emacs-site-lisp-dir (concat "/usr/share/emacs/" emacs-version "/"))

;; (setq load-path `("/usr/share/emacs/site-lisp"
;; 		  ,(concat emacs-site-lisp-dir "site-lisp")
;; 		  ,(concat emacs-site-lisp-dir "lisp")
;; 		  ,(concat emacs-site-lisp-dir "lisp/vc")
;; 		  ,(concat emacs-site-lisp-dir "lisp/cedet")
;; 		  ,(concat emacs-site-lisp-dir "lisp/url")
;; 		  ,(concat emacs-site-lisp-dir "lisp/textmodes")
;; 		  ,(concat emacs-site-lisp-dir "lisp/progmodes")
;; 		  ,(concat emacs-site-lisp-dir "lisp/nxml")
;; 		  ,(concat emacs-site-lisp-dir "lisp/net")
;; 		  ,(concat emacs-site-lisp-dir "lisp/international")
;; 		  ,(concat emacs-site-lisp-dir "lisp/emacs-lisp")
;; 		  ,(concat emacs-site-lisp-dir "lisp/calendar")))

;;; early-init.el ends here

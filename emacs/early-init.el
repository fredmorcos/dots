;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Enable init profiling
;; (require 'profiler)
;; (profiler-start 'cpu)
;; (add-hook 'after-init-hook #'profiler-report)

;; A big contributor to startup times is garbage collection.
(setq-default
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-style '(8 . 8))

;; faces
(custom-set-faces
 '(default            ((t . (:family     "Monospace"
                             :height     130
                             :background "Gray97"
                             :foreground "Gray25"))))
 '(cursor             ((t . (:background "SlateGray3"))))
 '(region             ((t . (:background "LightSteelBlue1"))))
 '(mode-line          ((t . (:height     110
                             :background "Gray90"
                             :foreground "Gray30"
                             :box        nil))))
 '(mode-line-inactive ((t . (:background "Gray90"
                             :foreground "Gray50"
                             :box        nil))))
 '(fringe             ((t (:background "Gray96")))))

(fset 'display-startup-echo-area-message 'ignore)
(run-with-idle-timer 5 t #'garbage-collect)

;; Ignore X resources; its settings would be redundant with the other settings in this
;; file and can conflict with later config (particularly where the cursor color is
;; concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(setq-default
 custom-file (concat temporary-file-directory "emacs/custom.el")
 file-name-handler-alist nil
 auto-window-vscroll nil
 vc-handled-backends '(Git)
 recentf-auto-cleanup 'never
 after-init-hook nil
 ;; package--init-file-ensured t
 auto-save-list-file-prefix nil

 ;; Resizing the Emacs frame can be a terribly expensive part of changing the font. By
 ;; inhibiting this, we easily halve startup times with fonts that are larger than the
 ;; system default.
 frame-inhibit-implied-resize t

 url-privacy-level 'high
 url-proxy-services '(("no_proxy" . "127.0.0.1")))

;;; early-init.el ends here

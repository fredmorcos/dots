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

;; Prevent the glimpse of un-styled Emacs by disabling these UI
;; elements early.
(global-tab-line-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-style '(8 . 8))

;; Ignore X resources; its settings would be redundant with the other
;; settings in this file and can conflict with later config
;; (particularly where the cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; faces
(custom-set-faces
 '(default                ((t (:family     "Monospace"
                               :height     120
                               :background "Gray98"
                               :foreground "Gray40"))))
 '(fringe                 ((t (:background "Gray98"))))
 '(cursor                 ((t (:background "SlateGray3"))))
 '(region                 ((t (:background "LightSteelBlue1"))))
 '(mode-line              ((t (:background "Gray95"
                               :foreground "Gray50"
                               :box        (:color "Lavender")))))
 '(mode-line-inactive     ((t (:inherit    mode-line
                               :foreground "Gray80"))))
 '(bold                   ((t (:weight     medium))))
 '(tab-line               ((t (:family     "Iosevka Aile"
                               :background "Gray85"
                               :box        (:color "Lavender")
                               :height     0.9
                               :inherit    'variable-pitch))))
 '(tab-line-tab           ((t (:inherit    'tab-line
                               :box        (:color "Lavender")))))
 '(tab-line-tab-current   ((t (:inherit    'tab-line-tab
                               :background "Gray90"))))
 '(tab-line-tab-inactive  ((t (:inherit    'tab-line-tab)))))

(fset 'display-startup-echo-area-message 'ignore)
(run-with-idle-timer 5 t #'garbage-collect)

(setq-default
 custom-file "/dev/null"
 ;; custom-file (concat temporary-file-directory "emacs/custom.el")
 file-name-handler-alist nil
 auto-window-vscroll nil
 vc-handled-backends '(Git)
 recentf-auto-cleanup 'never
 after-init-hook nil
 package-quickstart t
 ;; package--init-file-ensured t
 auto-save-list-file-prefix nil

 ;; Resizing the Emacs frame can be a terribly expensive part of
 ;; changing the font. By inhibiting this, we easily halve startup
 ;; times with fonts that are larger than the system default.
 frame-inhibit-implied-resize t

 url-privacy-level 'high
 url-proxy-services '(("no_proxy" . "127.0.0.1")))

;;; early-init.el ends here

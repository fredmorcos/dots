;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(fset 'display-startup-echo-area-message 'ignore)

(setq-default
 custom-file (concat temporary-file-directory "emacs/custom.el")
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6
 file-name-handler-alist nil
 ;; package-enable-at-startup nil
 auto-window-vscroll nil)

;;; early-init.el ends here

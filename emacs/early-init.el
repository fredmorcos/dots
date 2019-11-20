;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default
 custom-file (concat user-emacs-directory "custom.el")
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6
 file-name-handler-alist nil
 ;; package-enable-at-startup nil
 auto-window-vscroll nil)

;;; early-init.el ends here

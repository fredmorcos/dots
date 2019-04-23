;;; .emacs --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (call-interactively 'profiler-start)

(defvar old-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 100000000)

(setq vc-handled-backends nil)

(defvar old-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(setq custom-file "~/Workspace/dots/emacs/custom.el")

(defalias 'yes-or-no-p 'y-or-n-p)

;; (setq package-selected-packages
;;       '(f ht
;; 	diminish use-package bind-key
;; 	which-key fzf flx avy ivy ivy-rich swiper counsel smex
;; 	org-bullets git-gutter-fringe+ symbol-overlay multiple-cursors iedit
;; 	magit vdiff vdiff-magit
;; 	yasnippet yasnippet-snippets
;; 	hledger-mode
;; 	rmsbolt
;; 	flycheck company company-tabnine company-box
;; 	lsp-mode lsp-ui company-lsp
;; 	toml-mode markdown-mode json-mode gnuplot-mode dockerfile-mode meson-mode
;; 	yaml-mode flycheck-yamllint
;; 	rust-mode
;; 	lsp-java
;; 	irony irony-eldoc flycheck-irony company-irony company-irony-c-headers
;; 	cquery ccls
;; 	z3-mode boogie-friends))

(setq package-enable-at-startup nil)

(package-initialize)

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(use-package f)
(use-package ht)
(use-package diminish)
(use-package bind-key)
(use-package flx)
(use-package smex)
(use-package lv)
(use-package hydra)

(require 'diminish)
(require 'bind-key)

(use-package faces
  :ensure nil
  :commands (set-face-font
	     set-face-foreground
	     set-face-background
	     set-face-attribute)
  :init (progn
	  (setq font-use-system-font t)
	  (set-face-font 'default "Hack 11")
	  (set-face-foreground 'mode-line "gray20")
	  (set-face-background 'mode-line "gray80")
	  (set-face-attribute 'mode-line nil :box '(:line-width -1 :color "grey75" :style nil))
	  (set-face-attribute 'mode-line-highlight nil :box '(:line-width 1 :color "grey40" :style nil))))

(use-package url-vars
  :ensure nil
  :custom ((url-proxy-services '(("no_proxy" . "127.0.0.1")))
	   (url-privacy-level 'high)))

(use-package package
  :ensure nil
  :custom ((package-check-signature nil)
	   (package-archives
	    '(("gnu"   . "http://elpa.gnu.org/packages/")
	      ("melpa" . "http://melpa.org/packages/")))))

(use-package scroll-bar
  :ensure nil
  :init (progn
	  (setq scroll-conservatively 4)
	  (setq hscroll-margin 1)
	  (setq hscroll-step 1)
	  (setq auto-hscroll-mode 'current-line))
  :custom (scroll-bar-mode nil))

(use-package tool-bar
  :ensure nil
  :custom (tool-bar-mode nil))

(use-package menu-bar
  :ensure nil
  :custom (menu-bar-mode nil))

(defconst emacs-temp-dir (concat temporary-file-directory "emacs/"))
(defconst emacs-autosaves-dir (concat emacs-temp-dir "autosaves"))
(defconst emacs-backups-dir (concat emacs-temp-dir "backups"))
(defconst emacs-places-file (concat user-emacs-directory "places"))
(defconst emacs-recentf-file (concat user-emacs-directory "recentf"))
(defconst emacs-autosaves-pattern (concat emacs-autosaves-dir "/\\1"))
(defconst emacs-backups-pattern (concat emacs-backups-dir "/"))

(make-directory emacs-autosaves-dir t)
(make-directory emacs-backups-dir t)

(setq load-prefer-newer t)
(setq history-delete-duplicates t)
(setq history-length 30)

(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

(use-package saveplace
  :ensure nil
  :custom ((save-place t)
	   (save-place-mode t)
	   (save-place-file emacs-places-file)))

(use-package savehist
  :ensure nil
  :custom (savehist-mode t))

(use-package recentf
  :ensure nil
  :custom ((recentf-save-file emacs-recentf-file)
	   (recentf-mode t)))

(use-package files
  :ensure nil
  :hook (before-save-hook . delete-trailing-whitespace)
  :custom ((make-backup-files t)
	   (delete-old-versions t)
	   (auto-save-file-name-transforms `((".*" ,emacs-autosaves-pattern t)))
	   (backup-directory-alist `((".*" . ,emacs-backups-pattern)))
	   (backup-inhibited nil)
	   (mode-require-final-newline 'visit-save)
	   (require-final-newline 'visit-save)))

(use-package dash
  :ensure nil
  :init (setq show-trailing-whitespace nil)
  :custom ((whitespace-action '(cleanup))
	   (whitespace-style
	    '(face tabs lines-tail trailing
	      space-before-tab indentation
	      empty space-after-tab))))

(use-package vc
  :ensure nil
  :custom (vc-make-backup-files t))

(use-package newcomment
  :ensure nil
  :init (setq fill-column 70)
  :custom (comment-fill-column 70))

(use-package fill
  :ensure nil
  :custom ((colon-double-space t)
	   (default-justification 'left)))

(use-package indent
  :ensure nil
  :init (progn
	  (setq indent-tabs-mode nil)
	  (setq tab-width 2))
  :custom (standard-indent 2))

(use-package ediff-wind
  :ensure nil
  :commands (ediff-setup-windows-plain)
  :custom ((ediff-split-window-function #'split-window-horizontally)
	   (ediff-window-setup-function #'ediff-setup-windows-plain)))

(use-package which-key
  :diminish
  :custom (which-key-mode t))

(use-package ivy
  :diminish
  :requires ivy-rich
  :bind (:map ivy-minibuffer-map
	      ("RET" . ivy-alt-done))
  :custom ((ivy-mode t)
	   (ivy-use-selectable-prompt t)
	   (ivy-use-virtual-buffers t)
	   (ivy-display-style 'fancy)
	   (ivy-count-format "(%d/%d) ")
	   (ivy-wrap t)
	   (ivy-regex-ignore-order t)
	   (ivy--regex-ignore-order t)
	   (ivy-virtual-abbreviate 'full)
	   (ivy-action-wrap t)
	   (ivy-initial-inputs-alist nil)))

(use-package ivy-rich
  :after ivy
  :commands ivy-set-display-transformer
  :config (ivy-set-display-transformer
	   'ivy-switch-buffer
	   'ivy-rich-switch-buffer-transformer)
  :custom ((ivy-rich-mode t)
	   (ivy-rich-switch-buffer-align-virtual-buffer t)
	   (ivy-rich-path-style 'abbrev)))

(use-package counsel
  :diminish
  :custom (counsel-mode t)
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

(use-package swiper
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(use-package elisp-mode
  :ensure nil
  :mode ("\\emacs\\'" . emacs-lisp-mode))

(use-package checkdoc
  :ensure nil
  :diminish "CD"
  :hook (prog-mode . checkdoc-minor-mode))

(use-package eldoc
  :ensure nil
  :diminish "ED"
  :custom (eldoc-echo-area-use-multiline-p t))

(use-package display-line-numbers
  :ensure nil
  :custom ((global-display-line-numbers-mode t)
	   (display-line-numbers-grow-only t)
	   (display-line-numbers-width-start t))
  :init (progn
	  (set-face-foreground 'line-number "grey80")
	  (set-face-foreground 'line-number-current-line "grey60")
	  (set-face-background 'line-number-current-line "cornsilk")))

(use-package hl-line
  :ensure nil
  :custom (global-hl-line-mode t)
  :init (set-face-background 'hl-line "cornsilk"))

(use-package paren
  :ensure nil
  :custom (show-paren-mode t)
  :init (progn
	  (set-face-background 'show-paren-match "powder blue")
	  (set-face-background 'show-paren-match-expression "powder blue")
	  (set-face-background 'show-paren-mismatch "light salmon")))

(use-package cua-base
  :ensure nil
  :init (cua-selection-mode 1))

(use-package simple
  :ensure nil
  :custom ((column-number-mode t)
	   (size-indication-mode t)))

(use-package startup
  :ensure nil
  :custom ((inhibit-startup-screen t)
	   (inhibit-startup-message t)
	   (inhibit-startup-buffer-menu t)
	   (initial-scratch-message nil)
	   (initial-major-mode 'fundamental-mode)))

(use-package help
  :ensure nil
  :custom (help-window-select t))

(use-package frame
  :ensure nil
  :init (progn
	  (setq frame-resize-pixelwise t)
	  (setq frame-title-format "%b - emacs"))
  :custom (blink-cursor-mode nil))

(use-package window
  :ensure nil
  :bind ("C-z" . bury-buffer))

(use-package windmove
  :ensure nil
  :init (windmove-default-keybindings))

(use-package electric
  :ensure nil
  :commands (electric-pair-mode
	     electric-indent-mode
	     electric-quote-mode
	     electric-layout-mode)
  :init (progn
	  (electric-pair-mode)
	  (electric-indent-mode)
	  (electric-quote-mode)
	  (electric-layout-mode)))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

(use-package autorevert
  :ensure nil
  :diminish "AR"
  :hook (dired-mode . auto-revert-mode))

(use-package flyspell
  :diminish "Spell"
  :ensure nil
  :hook ((flyspell-mode . flyspell-buffer)
	 (prog-mode . flyspell-prog-mode)))

(use-package org
  :ensure nil
  :custom ((org-cycle-separator-lines 0)
	   (org-indent-indentation-per-level 2)
	   (org-startup-folded t))
  :hook ((org-mode . org-indent-mode)
	 (org-mode . org-bullets-mode)
	 (org-mode . flyspell-buffer)
	 (org-mode . (lambda nil
		       (add-hook 'after-save-hook #'flyspell-buffer nil t)))))

(use-package sh-script
  :ensure nil
  :custom ((sh-indentation 2)
	   (sh-basic-offset 2)))

(use-package fzf
  :bind (("M-p" . fzf-git-files)
	 ("M-P" . fzf-git-grep)))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package symbol-overlay
  :diminish
  :hook (prog-mode . symbol-overlay-mode)
  :bind (("M->" . symbol-overlay-jump-next)
	 ("M-<" . symbol-overlay-just-prev))
  :custom (symbol-overlay-idle-time 0.1)
  :config (set-face-background 'symbol-overlay-default-face "honeydew2"))

(use-package iedit)

(use-package git-gutter-fringe+
  :requires diminish
  :config (set-face-foreground 'git-gutter+-added "yellow green")
  :custom (git-gutter+-lighter " GG")
  :hook ((prog-mode . git-gutter+-mode)
	 (org-mode . git-gutter+-mode)
	 (hledger-mode . git-gutter+-mode)
	 (git-gutter+-mode . (lambda () (diminish git-gutter+-mode)))))

(use-package company
  :diminish "Com"
  :hook (prog-mode . company-mode)
  :custom ((company-tooltip-align-annotations t)
	   (company-idle-delay 0.2)))

(use-package company-box
  :diminish
  :after company
  :hook (company-mode . company-box-mode))

(use-package multiple-cursors
  :bind (("C-c C-v" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package yasnippet
  :hook ((hledger-mode . yas-minor-mode)
	 (rust-mode . yas-minor-mode))
  :commands (yas-reload-all)
  :config (progn
	    (push "~/Workspace/dots/emacs/snippets" yas-snippet-dirs)
	    (yas-reload-all)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package vdiff
  :bind (:map vdiff-mode-map
	      ("C-c" . vdiff-mode-prefix-map)))

(use-package hledger-mode
  :mode (("\\.journal\\'" . hledger-mode)
	 ("\\.ledger\\'" . hledger-mode))
  :custom ((hledger-comments-column 2)
	   (hledger-currency-string "EUR")
	   (hledge-current-overlay t))
  :commands toggle-truncate-lines
  :hook (hledger-mode . (lambda nil (toggle-truncate-lines t))))

(use-package flycheck
  :config (setq flycheck-mode-line-prefix "Chk")
  :hook (prog-mode . flycheck-mode))

;; (defvar package-thread
;;   (make-thread
;;    (lambda ()
;;      (progn
;;        ;; emacs lisp
;;        (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

;; (defvar background-thread
;;   (make-thread
;;    (lambda ()
;;      (progn
;;        (custom-set-faces
;;         '(rust-question-mark-face ((t (:inherit (font-lock-builtin-face)))))
;;         '(lsp-ui-doc-background ((t (:background "white smoke"))))
;;         '(lsp-ui-sideline-code-action ((t (:foreground "orange"))))
;;         '(lsp-ui-sideline-current-symbol
;;           ((t (:height 0.99 :weight ultra-bold :box
;;                        (:line-width -1 :color "dim gray" :style nil)
;;                        :foreground "dim gray")))))

;; ;; lsp
;; (require 'lsp)
;; (require 'lsp-clients)
;; (require 'lsp-ui)

;; (define-key lsp-ui-mode-map
;;   [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map
;;   [remap xref-find-references] #'lsp-ui-peek-find-references)

;; ;; c/c++
;; (add-hook 'c++-mode-hook   #'irony-mode)
;; (add-hook 'c++-mode-hook   #'flycheck-mode)
;; (add-hook 'c++-mode-hook   #'company-mode)
;; (add-hook 'c-mode-hook     #'irony-mode)
;; (add-hook 'c-mode-hook     #'flycheck-mode)
;; (add-hook 'c-mode-hook     #'company-mode)
;; (add-hook
;;  'c-mode-hook
;;  #'(lambda ()
;;      (setq-default flycheck-cppcheck-standards '("c11" "posix"))))
;; (add-hook 'irony-mode-hook #'irony-eldoc)
;; (add-hook 'irony-mode-hook #'flycheck-irony-setup)
;; (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
;; (add-hook
;;  'irony-mode-hook
;;  #'(lambda ()
;;      (add-to-list 'company-backends #'company-irony)
;;      (add-to-list 'company-backends #'company-irony-c-headers)
;;      (setq-default flycheck-cppcheck-checks '("all"))
;;      (setq-default flycheck-cppcheck-suppressions '("missingIncludeSystem"))))
;; (with-eval-after-load 'flycheck
;;   (with-eval-after-load 'flycheck-irony
;;     (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
;;     (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
;;     (flycheck-add-next-checker 'irony '(t . c/c++-cppcheck))))

;; ;; rust
;; (add-hook 'rust-mode-hook #'lsp)

;; ;; java
;; (require 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp)

;; ;; customizations
;; (custom-set-variables
;;  '(rust-indent-method-chain t)
;;  '(rust-always-locate-project-on-open t)
;;  '(rust-indent-where-clause t)
;;  '(rust-format-on-save t)

;;  '(lsp-prefer-flymake nil)
;;  '(lsp-ui-doc-include-signature t)
;;  '(lsp-ui-doc-border "light salmon")
;;  '(lsp-ui-doc-position 'at-point)
;;  '(lsp-ui-doc-max-width 60)
;;  '(lsp-ui-flycheck-enable t)
;;  '(lsp-ui-sideline-ignore-duplicate t)
;;  '(lsp-ui-sideline-update-mode 'point)

(setq file-name-handler-alist old-file-name-handler-alist)

(setq gc-cons-threshold old-gc-cons-threshold)

;; (call-interactively 'profiler-report)

(provide '.emacs)
;;; .emacs ends here

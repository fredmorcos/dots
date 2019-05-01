;;; .emacs --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Enable init profiling
;; (require 'profiler)
;; (call-interactively #'profiler-start)
;; (add-hook 'after-init-hook #'profiler-report)

(run-with-idle-timer 5 t #'garbage-collect)

(defconst file-name-handler-alist-old file-name-handler-alist)

(setq-default
 custom-file "/dev/null"
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6
 file-name-handler-alist nil
 auto-window-vscroll nil
 vc-handled-backends nil

 save-interprogram-paste-before-kill t

 url-privacy-level 'high
 url-proxy-services '(("no_proxy" . "127.0.0.1"))

 package-check-signature nil
 package-enable-at-startup nil
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("melpa" . "http://melpa.org/packages/"))

 use-package-always-defer t
 use-package-always-ensure t
 use-package-enable-imenu-support t
 use-package-expand-minimally t)

(require 'package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package subr
  :ensure nil
  :commands (eval-after-load add-hook add-to-list)
  :init (defalias 'yes-or-no-p 'y-or-n-p))

(use-package startup
  :ensure nil
  :hook (after-init
         . (lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old))))

(use-package faces
  :ensure nil
  :commands (set-face-font
             set-face-foreground
             set-face-background
             set-face-attribute)
  :init (progn
          (setq font-use-system-font t)
          (set-face-font 'default "Hack 11")
          (set-face-background 'cursor "gray30")
          (set-face-background 'region "LightSteelBlue1")
          (set-face-attribute
           'mode-line nil
           :box '(:line-width -1 :color "gray75" :style nil)
           :foreground "gray20"
           :background "gray80")
          (set-face-attribute
           'mode-line-highlight nil
           :box '(:line-width 1 :color "gray40" :style nil))))

(use-package scroll-bar
  :ensure nil
  :custom ((scroll-bar-mode nil)
           (horizontal-scroll-bar-mode nil)
           (scroll-conservatively 4)
           (hscroll-margin 1)
           (hscroll-step 1)
           (auto-hscroll-mode 'current-line)))

(use-package tool-bar
  :ensure nil
  :custom (tool-bar-mode nil))

(use-package menu-bar
  :ensure nil
  :custom (menu-bar-mode nil))

(use-package startup
  :ensure nil
  :custom ((inhibit-startup-screen t)
           (inhibit-startup-message t)
           (inhibit-startup-buffer-menu t)
           (initial-scratch-message nil)
           (initial-major-mode 'fundamental-mode)))

(use-package frame
  :ensure nil
  :custom ((blink-cursor-mode nil)
           (frame-resize-pixelwise t)
           (frame-title-format "%b - emacs")))

(use-package display-line-numbers
  :ensure nil
  :custom ((global-display-line-numbers-mode t)
           (display-line-numbers-grow-only t)
           (display-line-numbers-width-start t))
  :init (progn
          (set-face-foreground 'line-number "gray80")
          (set-face-attribute
           'line-number-current-line nil
           :foreground "gray60"
           :background "CornSilk")))

(use-package hl-line
  :ensure nil
  :custom (global-hl-line-mode t)
  :init (set-face-background 'hl-line "CornSilk"))

(use-package apropos
  :ensure nil
  :custom (apropos-do-all t))

(use-package expand
  :ensure nil
  :bind ("M-/" . hippie-expand))

(use-package fringe
  :ensure nil
  :config (progn
            (set-window-fringes nil 16)
            (set-face-background 'fringe "Gray97")))

(defconst emacs-places-file (concat user-emacs-directory "places"))
(defconst emacs-recentf-file (concat user-emacs-directory "recentf"))
(defconst emacs-temp-dir (concat temporary-file-directory "emacs/"))
(defconst emacs-autosaves-dir (concat emacs-temp-dir "autosaves"))
(defconst emacs-autosaves-pattern (concat emacs-autosaves-dir "/\\1"))
(defconst emacs-backups-dir (concat emacs-temp-dir "backups"))
(defconst emacs-backups-pattern (concat emacs-backups-dir "/"))
(make-directory emacs-autosaves-dir t)
(make-directory emacs-backups-dir t)

(use-package saveplace
  :ensure nil
  :custom ((save-place t)
           (save-place-mode t)
           (save-place-file emacs-places-file)))

(use-package savehist
  :ensure nil
  :custom ((savehist-mode t)
           (history-delete-duplicates t)
           (history-length 30)))

(use-package recentf
  :ensure nil
  :custom ((recentf-save-file emacs-recentf-file)
           (recentf-mode t)))

(use-package files
  :ensure nil
  :custom ((auto-save-file-name-transforms `((".*" ,emacs-autosaves-pattern t)))
           (backup-directory-alist `((".*" . ,emacs-backups-pattern)))
           (backup-inhibited nil)
           (make-backup-files t)
           (delete-old-versions t)
           (mode-require-final-newline 'visit-save)
           (require-final-newline 'visit-save)
           (load-prefer-newer t)
           (coding-system-for-read 'utf-8-unix)
           (coding-system-for-write 'utf-8-unix)))

(use-package electric
  :ensure nil
  :custom ((electric-layout-mode t)
           (electric-pair-mode t)))

(use-package cua-base
  :ensure nil
  :commands cua-selection-mode
  :init (cua-selection-mode 1))

(use-package help
  :ensure nil
  :custom (help-window-select t))

(use-package window
  :ensure nil
  :bind (("C-z" . bury-buffer)
         ("s-w" . delete-window)
         ("s-o" . other-window)
         ("s-n" . next-buffer)
         ("s-b" . previous-buffer)
         ("s-v" . split-window-below)
         ("s-h" . split-window-right)))

(use-package page
  :ensure nil
  :commands (backward-page
             forward-page)
  :bind (("s-a" . #'backward-page)
         ("s-e" . #'forward-page)))

(use-package windmove
  :ensure nil
  :commands windmove-default-keybindings
  :init (windmove-default-keybindings))

(use-package simple
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :custom ((column-number-mode t)
           (size-indication-mode t)
           (auto-save-mode t)))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(use-package vc
  :ensure nil
  :custom (vc-make-backup-files t))

(use-package newcomment
  :ensure nil
  :custom (comment-fill-column 70))

(use-package fill
  :ensure nil
  :custom ((fill-column 70)
           (colon-double-space t)
           (default-justification 'left)))

(use-package indent
  :ensure nil
  :custom ((standard-indent 2)
           (indent-tabs-mode nil)
           (tab-width 2)))

(use-package ediff-wind
  :ensure nil
  :commands (split-window-horizontally
             ediff-setup-windows-plain)
  :custom ((ediff-split-window-function #'split-window-horizontally)
           (ediff-window-setup-function #'ediff-setup-windows-plain)))

(use-package whitespace
  :ensure nil
  :custom ((show-trailing-whitespace nil)
           (whitespace-action '(cleanup))
           (whitespace-style
            '(face
              tabs
              lines-tail
              trailing
              space-before-tab
              indentation
              empty
              space-after-tab))))

(use-package elisp-mode
  :ensure nil
  :mode ("\\emacs\\'" . emacs-lisp-mode))

(use-package checkdoc
  :ensure nil
  :diminish "CD"
  :hook (prog-mode . checkdoc-minor-mode)
  :custom (checkdoc-minor-mode-string " CD"))

(use-package eldoc
  :ensure nil
  :diminish "ED"
  :custom (eldoc-echo-area-use-multiline-p t))

(use-package paren
  :ensure nil
  :custom ((show-paren-mode t)
           (show-paren-when-point-inside-paren t)
           (show-paren-when-point-in-periphery t)
           (show-paren-style 'mixed))
  :init (progn
          (set-face-background 'show-paren-match "PowderBlue")
          (set-face-background 'show-paren-match-expression "PowderBlue")
          (set-face-background 'show-paren-mismatch "LightSalmon")))

(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . auto-revert-mode))
  :custom ((dired-listing-switches "-l --group-directories-first")
           (dired-hide-details-hide-symlink-targets nil)))

(use-package autorevert
  :ensure nil
  :diminish "AR"
  :custom ((auto-revert-interval 2)
           (auto-revert-mode-text " AR")))

(use-package sh-script
  :ensure nil
  :custom ((sh-indentation 2)
           (sh-basic-offset 2)))

(use-package org
  :ensure nil
  :custom ((org-cycle-separator-lines 0)
           (org-indent-indentation-per-level 2)
           (org-startup-folded t))
  :hook ((org-mode . org-indent-mode)
         (org-mode . org-bullets-mode)
         (org-mode . flyspell-buffer)
         (org-mode
          . (lambda ()
              (add-hook 'after-save-hook #'flyspell-buffer nil t))))
  :config (progn (set-face-attribute
                  'org-ellipsis nil
                  :underline nil
                  :foreground "DarkGoldenRod")
                 (setq org-ellipsis "   ▼")))

(use-package f)
(use-package ht)
(use-package dash)
(use-package diminish)
(use-package bind-key)
(use-package flx)
(use-package amx)
(use-package smex)
(use-package lv)
(use-package all-the-icons)
(use-package org-bullets)

(use-package iedit
  :init (require 'iedit))

(use-package which-key
  :diminish
  :custom (which-key-mode t))

(use-package prescient
  :custom ((prescient-persist-mode t)
           (prescient-filter-method '(literal regexp initialism fuzzy))))

(use-package ivy-prescient
  :config (ivy-prescient-mode))

(use-package company-prescient
  :config (company-prescient-mode))

(use-package counsel
  :diminish
  :custom (counsel-mode t)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-A" . counsel-ag)))

(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map ("RET" . ivy-alt-done))
  :custom ((ivy-mode t)
           (ivy-use-selectable-prompt t)
           (ivy-use-virtual-buffers t)
           (ivy-display-style 'fancy)
           (ivy-count-format "(%d/%d) ")
           (ivy-wrap t)
           (ivy-regex-ignore-order t)
           (ivy-virtual-abbreviate 'full)
           (ivy-action-wrap t)
           (ivy-initial-inputs-alist nil)))

(use-package ivy-rich
  :commands (ivy-set-display-transformer
             ivy-format-function-line)
  :config (ivy-set-display-transformer
           'ivy-switch-buffer
           'ivy-rich--ivy-switch-buffer-transformer)
  :custom ((ivy-rich-mode t)
           (ivy-rich-switch-buffer-align-virtual-buffer t)
           (ivy-rich-path-style 'abbrev)
           (ivy-format-function #'ivy-format-function-line)))

(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package fzf
  :bind (("M-F" . fzf-git-files)
         ("M-P" . fzf-git-grep)))

(use-package magit
  :bind ("C-x g" . magit-status)
  :defines magit-status-buffer-switch-function
  :commands magit-display-buffer-same-window-except-diff-v1
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package ivy-xref
  :init (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package dired-subtree
  :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle))
  :config (progn
            (set-face-background 'dired-subtree-depth-1-face "LightBlue")
            (set-face-background 'dired-subtree-depth-2-face "LightGreen")
            (set-face-background 'dired-subtree-depth-3-face "LightYellow")
            (set-face-background 'dired-subtree-depth-4-face "LightBlue")
            (set-face-background 'dired-subtree-depth-5-face "LightGreen")
            (set-face-background 'dired-subtree-depth-6-face "LightYellow")))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-c n" . flycheck-next-error)
              ("C-c p" . flycheck-previous-error)
              ("C-c l" . flycheck-list-errors))
  :custom ((flycheck-checker-error-threshold nil)
           (flycheck-mode-line-prefix "Chk")))

(use-package company
  :diminish "Com"
  :commands (local-set-key company-indent-or-complete-common)
  :hook ((prog-mode . company-mode)
         (company-mode . (lambda ()
                           (local-set-key
                            (kbd "TAB")
                            #'company-indent-or-complete-common))))
  :custom ((company-auto-complete 'company-explicit-action-p)
           (company-auto-complete-chars '(32 95 41 46))
           (company-echo-truncate-lines nil)
           (company-selection-wrap-around t)
           (company-tooltip-limit 100)
           (company-tooltip-minimum 10)
           (company-tooltip-align-annotations t)
           (company-idle-delay 0.2)
           (company-echo-delay 0)
           (company-begin-commands '(self-insert-command))
           (company-transformers '(company-sort-by-backend-importance))))

(use-package symbol-overlay
  :diminish
  :hook ((prog-mode . symbol-overlay-mode)
         (hledger-mode . symbol-overlay-mode))
  :bind (("M->" . symbol-overlay-jump-next)
         ("M-<" . symbol-overlay-just-prev))
  :custom (symbol-overlay-idle-time 0.1)
  :config (set-face-background 'symbol-overlay-default-face "honeydew2"))

(use-package multiple-cursors
  :bind (("C-c C-v" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  :defines (mc/cursor-bar-face mc/cursor-face)
  :custom (mc/always-run-for-all t)
  :config (progn
            (set-face-attribute
             mc/cursor-bar-face nil
             :background "gray40" :foreground "white")
            (set-face-attribute
             mc/cursor-face nil
             :background "gray50" :foreground "white")))

(use-package yasnippet
  :hook ((hledger-mode . yas-minor-mode)
         (prog-mode . yas-minor-mode))
  :commands (yas-reload-all)
  :defines yas-snippet-dirs
  :config (progn
            (push "~/Workspace/dots/emacs/snippets" yas-snippet-dirs)
            (make-thread #'yas-reload-all)))

(use-package yasnippet-snippets)

(use-package git-gutter-fringe+
  :init (set-face-foreground 'git-gutter+-added "YellowGreen")
  :custom ((global-git-gutter+-mode t)
           (git-gutter+-lighter " GG"))
  :hook (magit-status-refresh . git-gutter+-on-magit-refresh-status))

(use-package hledger-mode
  :mode (("\\.journal\\'" . hledger-mode)
         ("\\.ledger\\'" . hledger-mode))
  :custom ((hledger-comments-column 2)
           (hledger-currency-string "EUR")
           (hledger-current-overlay t))
  :commands toggle-truncate-lines
  :hook (hledger-mode . (lambda nil (toggle-truncate-lines t))))

(use-package z3-mode)

(use-package eglot)
(use-package rustic)

(provide '.emacs)
;;; .emacs ends here

;;; init --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Enable init profiling
;; (require 'profiler)
;; (call-interactively #'profiler-start)
;; (add-hook 'after-init-hook #'profiler-report)

(run-with-idle-timer 5 t #'garbage-collect)
(defconst fnh-alist-old file-name-handler-alist)

(setq-default
 url-privacy-level 'high
 url-proxy-services '(("no_proxy" . "127.0.0.1")))

(setq-default
 package-archives
 '(("gnu" . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
   ("org" . "https://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default
 use-package-always-defer t
 use-package-always-ensure t
 use-package-expand-minimally t)

(eval-when-compile
  (require 'use-package))

(use-package subr
  :ensure nil

  :commands
  eval-after-load
  add-hook
  add-to-list
  sit-for

  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package startup
  :ensure nil

  :init
  (defun replace-escapes ()
    (interactive)
    (goto-char (point-min))
    (while (search-forward "\\n" nil t)
      (replace-match (char-to-string ?\n) nil t))
    (while (search-forward "\\t" nil t)
      (replace-match (char-to-string ?\t) nil t)))

  :bind
  ("C-x e" . replace-escapes)

  :custom
  (inhibit-startup-screen t)
  (inhibit-startup-message t)
  (inhibit-startup-buffer-menu t)
  (initial-scratch-message nil)
  (initial-major-mode 'fundamental-mode)
  (auto-save-list-file-prefix nil)

  :hook
  (after-init . (lambda () (setq file-name-handler-alist fnh-alist-old))))

(use-package faces
  :ensure nil

  :custom
  (font-use-system-font t)

  :custom-face
  (default ((t (:font "Monospace 12"))))
  (cursor ((t (:background "SlateGray3"))))
  (region ((t (:background "LightSteelBlue1"))))
  (mode-line ((t (:foreground "Gray20" :background "Gray80")))))

(use-package scroll-bar
  :ensure nil

  :custom
  (scroll-bar-mode nil)
  (horizontal-scroll-bar-mode nil)
  (scroll-conservatively 4)
  (hscroll-margin 1)
  (hscroll-step 1)
  (auto-hscroll-mode 'current-line))

(use-package tool-bar
  :ensure nil

  :custom
  (tool-bar-mode nil))

(use-package menu-bar
  :ensure nil

  :custom
  (menu-bar-mode nil))

(use-package tab-bar
  :ensure nil

  :custom
  (tab-bar-mode t)

  :bind
  ("C-<next>" . tab-bar-switch-to-next-tab)
  ("C-<prior>" . tab-bar-switch-to-prev-tab)

  :custom-face
  (tab-bar ((t (:background "Gray97"))))
  (tab-bar-tab ((t (:background "LightSteelBlue1" :box "DeepSkyBlue"))))
  (tab-bar-tab-inactive ((t (:background "Gray90" :box "Gray60")))))

(use-package frame
  :ensure nil

  :custom
  (blink-cursor-mode nil)
  (frame-resize-pixelwise t)
  (frame-title-format "%b - emacs"))

(use-package display-line-numbers
  :ensure nil

  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)

  :custom-face
  (line-number ((t (:foreground "Gray90"))))
  (line-number-current-line ((t (:foreground "Gray60"))))

  :hook
  (prog-mode . display-line-numbers-mode))

(use-package hl-line
  :ensure nil

  :custom-face
  (hl-line ((t (:background "CornSilk"))))

  :hook
  ((prog-mode text-mode) . hl-line-mode))

(use-package apropos
  :ensure nil

  :custom
  (apropos-do-all t))

(use-package expand
  :ensure nil

  :bind
  ("M-/" . hippie-expand))

(use-package fringe
  :ensure nil

  :custom-face
  (fringe ((t (:background "Gray97"))))

  :commands
  set-fringe-style

  :config
  (set-fringe-style '(8 . 8)))

(use-package minibuffer
  :ensure nil

  :custom
  (completion-styles
   '(initials substring basic partial-completion emacs22 emacs21)))

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

  :custom
  (save-place t)
  (save-place-mode t)
  (save-place-file emacs-places-file))

(use-package savehist
  :ensure nil

  :custom
  (savehist-mode t)
  (history-delete-duplicates t)
  (history-length 50))

(use-package recentf
  :ensure nil

  :commands
  recentf-cleanup

  :config
  (add-to-list 'recentf-exclude "~/.config/emacs/elpa")
  (run-with-idle-timer 30 t #'recentf-cleanup)

  :custom
  (recentf-save-file emacs-recentf-file)
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 50)
  (recentf-mode t))

(use-package files
  :ensure nil

  :custom
  (confirm-kill-processes nil)
  (auto-save-file-name-transforms `((".*" ,emacs-autosaves-pattern t)))
  (backup-directory-alist `((".*" . ,emacs-backups-pattern)))
  (backup-inhibited nil)
  (make-backup-files t)
  (delete-old-versions t)
  (mode-require-final-newline 'visit-save)
  (require-final-newline 'visit-save)
  (load-prefer-newer t)
  (coding-system-for-read 'utf-8-unix)
  (coding-system-for-write 'utf-8-unix))

(use-package keyboard
  :ensure nil

  :custom
  (suggest-key-bindings 10))

(use-package electric
  :ensure nil

  :custom
  (electric-layout-mode t)
  (electric-pair-mode t))

(use-package cua-base
  :ensure nil

  :commands
  cua-selection-mode

  :init
  (cua-selection-mode 1))

(use-package help
  :ensure nil

  :custom
  (help-window-select t)
  (help-at-pt-display-when-idle t)
  (help-at-pt-timer-delay 0.1))

(use-package window
  :ensure nil

  :bind
  ("C-z" . bury-buffer)
  ("s-w" . delete-window)
  ("s-o" . other-window)
  ("s-n" . next-buffer)
  ("s-b" . previous-buffer)
  ("s-v" . split-window-below)
  ("s-h" . split-window-right)

  :custom
  (display-buffer-alist
   '(("\\*help"
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . right)
      (window-width . 100)))))

(use-package page
  :ensure nil

  :bind
  ("s-a" . backward-page)
  ("s-e" . forward-page))

(use-package windmove
  :ensure nil

  :commands
  windmove-default-keybindings

  :init
  (windmove-default-keybindings))

(use-package simple
  :ensure nil

  :hook
  (before-save . delete-trailing-whitespace)

  :custom
  (column-number-mode t)
  (line-number-mode nil)
  (auto-save-mode t)
  (save-interprogram-paste-before-kill t)

  :init
  (defun move-line-up ()
    (interactive)
    (transpose-lines 1)
    (forward-line -2))
  (defun move-line-down ()
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1))

  :bind
  ("M-<up>" . move-line-up)
  ("M-<down>" . move-line-down))

(use-package bindings
  :ensure nil

  :custom
  (column-number-indicator-zero-based nil))

(use-package uniquify
  :ensure nil

  :custom
  (uniquify-buffer-name-style 'forward))

(use-package vc
  :ensure nil

  :custom
  (vc-make-backup-files t))

(use-package abbrev
  :ensure nil
  :diminish "Abb")

(use-package newcomment
  :ensure nil

  :custom
  (comment-fill-column 70))

(use-package display-fill-column-indicator
  :ensure nil

  :custom-face
  (fill-column-indicator ((t (:foreground "Azure2"))))

  :hook
  ((emacs-lisp-mode c-mode) . display-fill-column-indicator-mode))

(use-package fill
  :ensure nil

  :custom
  (fill-column 70)
  (colon-double-space t)
  (default-justification 'left))

(use-package indent
  :ensure nil

  :custom
  (standard-indent 2)
  (indent-tabs-mode nil)
  (tab-width 2))

(use-package ediff-wind
  :ensure nil

  :commands
  split-window-horizontally
  ediff-setup-windows-plain

  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package whitespace
  :ensure nil

  :custom
  (show-trailing-whitespace nil)
  (whitespace-action '(cleanup))
  (whitespace-style
   '(face
     tabs
     lines-tail
     trailing
     space-before-tab
     indentation
     empty
     space-after-tab)))

(use-package lisp-mode
  :ensure nil

  :init
  (put 'use-package 'lisp-indent-function 1))

(use-package emacs-lisp-mode
  :ensure nil

  :hook
  (emacs-lisp-mode . (lambda ()
                       (setq fill-column 90)
                       (setq-local comment-fill-column 90)))

  :mode
  "\\emacs\\'"
  "\\.config/emacs/init\\'")

(use-package text-mode
  :ensure nil

  :mode
  "\\Passwords.txt\\'"
  "\\Passwords_old.txt\\'"

  :hook
  (text-mode . (lambda () (toggle-truncate-lines t))))

(use-package eldoc
  :ensure nil
  :diminish "ED"

  :custom
  (eldoc-echo-area-use-multiline-p t)

  :hook
  (prog-mode . eldoc-mode))

(use-package js-mode
  :ensure nil

  :mode
  "\\.hocon\\'")

(use-package antlr-mode
  :ensure nil

  :custom
  (antlr-tab-offset-alist
   '((antlr-mode nil 2 nil)
     (java-mode "antlr" 2 nil)))

  :mode
  "\\.g4\\'")

(use-package llvm-mode
  :ensure nil

  :mode
  "\\.ll\\'"

  :init
  (let* ((url (concat "https://raw.githubusercontent.com/llvm/llvm-project"
                      "/master/llvm/utils/emacs/llvm-mode.el"))
         (dst-dir (concat user-emacs-directory "extra/"))
         (dst (concat dst-dir "llvm-mode.el"))
         (dst-bc (concat dst-dir "llvm-mode.elc")))
    (when (not (file-readable-p dst-bc))
      (make-directory dst-dir t)
      (url-copy-file url dst t)
      (byte-compile-file dst))
    (add-to-list 'load-path dst-dir))

  :hook
  (llvm-mode . (lambda () (toggle-truncate-lines t))))

(use-package ra-emacs-lsp
  :ensure nil

  :init
  (let* ((url (concat "https://raw.githubusercontent.com/rust-analyzer/rust-analyzer"
                      "/master/editors/emacs/ra-emacs-lsp.el"))
         (dst-dir (concat user-emacs-directory "extra/"))
         (dst (concat dst-dir "ra-emacs-lsp.el"))
         (dst-bc (concat dst-dir "ra-emacs-lsp.elc")))
    (when (not (file-readable-p dst-bc))
      (make-directory dst-dir t)
      (url-copy-file url dst t)
      (byte-compile-file dst))
    (add-to-list 'load-path dst-dir))

  :custom
  (rust-analyzer-inlay-hints-mode t)

  :commands
  rust-analyzer-join-lines
  rust-analyzer-extend-selection

  :hook
  (rust-mode . rust-analyzer-inlay-hints-mode))

(use-package tree-sitter
  :ensure nil

  :commands
  ts-require-language

  :init
  (let* ((default-directory "~/Build/emacs-tree-sitter/")
         (core-dst (expand-file-name "tree-sitter-core.el"))
         (core-bc (expand-file-name "tree-sitter-core.elc"))
         ;; (debug-dst (expand-file-name "tree-sitter-debug.el"))
         ;; (debug-bc (expand-file-name "tree-sitter-debug.elc"))
         (dst (expand-file-name "tree-sitter.el"))
         (bc (expand-file-name "tree-sitter.elc")))
    (add-to-list 'load-path default-directory)
    (when (not (file-readable-p bc))
      (byte-compile-file dst))
    ;; (when (not (file-readable-p debug-bc))
    ;;   (byte-compile-file debug-dst))
    (when (not (file-readable-p core-bc))
      (byte-compile-file core-dst)))

  :hook
  ((rust-mode python-mode c-mode java-mode sh-mode) . tree-sitter-mode)
  (rust-mode   . (lambda () (ts-require-language 'rust)))
  (python-mode . (lambda () (ts-require-language 'python)))
  (c-mode      . (lambda () (ts-require-language 'c)))
  (java-mode   . (lambda () (ts-require-language 'java)))
  (sh-mode     . (lambda () (ts-require-language 'bash))))

(use-package paren
  :ensure nil

  :custom
  (show-paren-mode t)
  (show-paren-delay 0)
  (show-paren-when-point-inside-paren t)
  (show-paren-style 'mixed)
  (show-paren-highlight-openparen t)

  :custom-face
  (show-paren-match ((t (:background "PowderBlue"))))
  (show-paren-match-expression ((t (:background "AliceBlue"))))
  (show-paren-mismatch ((t (:background "LightSalmon")))))

(use-package dired
  :ensure nil

  :hook
  (dired-mode . dired-hide-details-mode)

  :custom
  (dired-listing-switches "-l --group-directories-first")
  (dired-hide-details-hide-symlink-targets nil))

(use-package autorevert
  :ensure nil
  :diminish "AR"

  :custom
  (auto-revert-interval 2)
  (auto-revert-mode-text " AR")

  :hook
  (dired-mode . auto-revert-mode))

(use-package sh-script
  :ensure nil

  :custom
  (sh-indentation 2)
  (sh-basic-offset 2))

(use-package eshell
  :ensure nil

  :custom
  (eshell-destroy-buffer-when-process-dies t))

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
(use-package org-present)

(use-package org-sticky-header
  :hook
  (org-mode . org-sticky-header-mode)

  :custom
  (org-sticky-header-full-path 'full)
  (org-sticky-header-outline-path-separator " > "))

(use-package org-bullets
  :config
  (setq org-bullets-bullet-list (seq-take org-bullets-bullet-list 2))

  :hook
  (org-mode . org-bullets-mode))

(use-package org
  :pin org

  :custom
  (org-cycle-separator-lines 0)
  (org-indent-indentation-per-level 2)
  (org-startup-folded t)

  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda () (jit-lock-register 'flyspell-region)))
  (org-mode . (lambda () (add-hook 'after-save-hook #'flyspell-buffer nil t)))

  :custom-face
  (org-ellipsis ((t (:underline nil :foreground "DarkGoldenRod"))))

  :config
  (setq org-ellipsis "   ▾"))

(use-package markdown-mode
  :ensure nil

  :hook
  (markdown-mode . (lambda () (jit-lock-register 'flyspell-region)))
  (markdown-mode . (lambda () (add-hook 'after-save-hook #'flyspell-buffer nil t))))

(use-package iedit
  :bind
  ("C-;" . iedit-mode))

(use-package which-key
  :diminish

  :custom
  (which-key-idle-delay 0.3)
  (which-key-mode t))

(use-package counsel
  :diminish

  :custom
  (counsel-mode t)

  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("M-A" . counsel-ag)
  ("M-R" . counsel-rg))

(use-package ivy
  :diminish

  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-alt-done))

  :custom
  (ivy-mode t)
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")
  (ivy-wrap t)
  (ivy-regex-ignore-order t)
  (ivy-virtual-abbreviate 'full)
  (ivy-action-wrap t)
  (ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :commands
  ivy-set-display-transformer
  ivy-format-function-line

  :config
  (ivy-set-display-transformer
   'ivy-switch-buffer
   'ivy-rich--ivy-switch-buffer-transformer)

  :custom
  (ivy-rich-mode t)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  (ivy-format-function #'ivy-format-function-line))

(use-package swiper
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper-backward))

(use-package fzf
  :bind
  ("M-F" . fzf-git-files)
  ("M-P" . fzf-git-grep))

(use-package deadgrep
  :bind
  ("M-G" . deadgrep))

(use-package transient
  ;; Magit-related

  :custom
  (transient-default-level 7))

(use-package magit
  :bind
  ("C-x g" . magit-status)

  :hook
  (after-save . magit-after-save-refresh-status)

  :defines
  magit-status-buffer-switch-function

  :commands
  magit-display-buffer-same-window-except-diff-v1

  :custom
  (magit-auto-revert-tracked-only nil)
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :hook
  (magit-mode . magit-todos-mode))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package ivy-xref
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle))

  :custom-face
  (dired-subtree-depth-1-face ((t (:background "LightBlue"))))
  (dired-subtree-depth-2-face ((t (:background "LightGreen"))))
  (dired-subtree-depth-3-face ((t (:background "LightYellow"))))
  (dired-subtree-depth-4-face ((t (:background "LightBlue"))))
  (dired-subtree-depth-5-face ((t (:background "LightGreen"))))
  (dired-subtree-depth-6-face ((t (:background "LightYellow")))))

(use-package flycheck
  :pin melpa

  :bind
  (:map flycheck-mode-map
        ("M-n" . flycheck-next-error)
        ("M-p" . flycheck-previous-error)
        ("C-c l" . flycheck-list-errors))

  :custom
  (flycheck-checker-error-threshold nil)
  (flycheck-mode-line-prefix "Chk")

  :hook
  (prog-mode . flycheck-mode))

(use-package company
  :pin melpa
  :diminish "Com"

  :bind
  ("TAB" . company-indent-or-complete-common)

  :custom
  (company-backends '(company-capf company-dabbrev-code company-keywords
                                   company-dabbrev company-files))
  (completion-ignore-case t)
  (company-etags-ignore-case t)
  (company-dabbrev-minimum-length 1)
  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-ignore-case t)
  (company-echo-truncate-lines nil)
  (company-echo-delay 0)
  (company-idle-delay nil)
  (company-tooltip-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-tooltip-minimum 10)
  (company-tooltip-limit 20)
  (company-tooltip-align-annotations t)
  (company-transformers '(company-sort-by-backend-importance))

  :hook
  (prog-mode . company-mode))

(use-package company-quickhelp
  :pin melpa
  :after company

  :hook
  (company-mode . company-quickhelp-mode)

  :custom
  (company-quickhelp-use-propertized-text t)
  (company-quickhelp-delay nil)

  :bind
  (:map company-active-map
        ("C-c h" . company-quickhelp-manual-begin)))

(use-package diff-hl
  :after magit

  :custom
  (global-diff-hl-mode t)
  (diff-hl-flydiff-mode t)
  (diff-hl-draw-borders nil)
  (diff-hl-flydiff-delay 0.1)

  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)

  :custom-face
  (diff-hl-delete ((t (:background "RosyBrown1"))))
  (diff-hl-insert ((t (:background "DarkSeaGreen2"))))
  (diff-hl-change ((t (:background "PowderBlue")))))

(use-package symbol-overlay
  :pin melpa
  :diminish

  :bind
  ("M->" . symbol-overlay-jump-next)
  ("M-<" . symbol-overlay-just-prev)

  :custom
  (symbol-overlay-idle-time 0.1)

  :custom-face
  (symbol-overlay-default-face ((t (:background "HoneyDew2"))))

  :hook
  ((hledger-mode prog-mode z3-smt2-mode) . symbol-overlay-mode))

(use-package multiple-cursors
  :bind
  ("C-c C-v" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)

  :custom
  (mc/always-run-for-all t)

  :custom-face
  (mc/cursor-bar-face ((t (:background "Gray40" :foreground "White"))))
  (mc/cursor-face ((t (:background "Gray50" :foreground "White")))))

(use-package yasnippet
  :commands
  yas-reload-all

  :defines
  yas-snippet-dirs

  :config
  (push (expand-file-name "~/Workspace/dots/emacs/snippets") yas-snippet-dirs)
  (make-thread #'yas-reload-all)

  :hook
  ((hledger-mode java-mode c-mode python-mode rust-mode) . yas-minor-mode))

(use-package yasnippet-snippets)

(use-package hledger-mode
  :mode
  "\\.journal\\'"
  "\\.ledger\\'"

  :custom
  (hledger-comments-column 2)
  (hledger-currency-string "EUR")
  (hledger-current-overlay t)

  :commands
  toggle-truncate-lines

  :hook
  (hledger-mode . (lambda () (toggle-truncate-lines t))))

(use-package toml-mode)
(use-package json-mode)
(use-package pkgbuild-mode)

(use-package rmsbolt
  :diminish "Bolt"

  :custom
  (rms-bolt-lighter "Bolt"))

(use-package boogie-friends
  :custom
  (z3-smt2-prover-custom-args
   '("smt.relevancy=1" "sat.acce=true" "smt.arith.solver=6")))

(use-package lsp-java
  :requires lsp-mode)

(use-package java-mode
  :ensure nil
  :after lsp-mode

  :custom
  (lsp-enable-file-watchers nil)

  :hook
  (java-mode . (lambda ()
                 (setq-local standard-indent 2)
                 (setq-local comment-fill-column 90)
                 (setq tab-width 2
                       fill-column 90))))

(use-package rust-mode
  :requires tree-sitter ra-emacs-lsp

  :hook
  (rust-mode . (lambda ()
                 (setq-local standard-indent 4)
                 (setq-local comment-fill-column 90)
                 (setq tab-width 4
                       fill-column 90)))

  :custom-face
  (rust-question-mark-face ((t (:inherit (font-lock-builtin-face))))))

(use-package cargo
  :after rust-mode

  :init
  (defun cargo-fmt-and-lint ()
    (interactive)
    (cargo-process-fmt)
    (sit-for 1)
    (cargo-process-clippy))

  :bind
  (:map cargo-minor-mode-map
        ("C-c C-c C-c" . cargo-fmt-and-lint))

  :hook
  (rust-mode . cargo-minor-mode))

(use-package c-mode
  :ensure nil

  :custom
  (lsp-clients-clangd-args
   '("--background-index"
     "--clang-tidy"
     "--completion-style=detailed"
     "--header-insertion=iwyu"
     "--header-insertion-decorators"
     "--suggest-missing-includes"
     "--fallback-style=llvm"
     "-j=13"
     "--pch-storage=memory")))

(use-package lsp-mode
  :commands
  lsp
  lsp-deferred

  :bind
  (:map lsp-mode-map
        ("C-c f" . lsp-format-buffer)
        ("C-c r" . lsp-rename))

  :custom
  (lsp-prefer-flymake nil)
  (lsp-document-highlight-delay 0.1)

  (lsp-rust-build-bin t)
  (lsp-rust-build-lib t)
  (lsp-rust-clippy-preference "on")
  (lsp-rust-full-docs t)
  (lsp-rust-wait-to-build 0.1)
  (lsp-rust-analyzer-inlay-hints-mode t)

  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-border "black")
  (lsp-ui-sideline-update-mode 'line)

  :hook
  ((c-mode python-mode java-mode rust-mode) . lsp))

(use-package lsp-ui
  :after lsp-mode

  :commands
  lsp-ui-mode

  :custom
  (lsp-ui-sideline-show-symbol nil)
  (lsp-ui-sideline-delay 0.1)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-list-mode t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 0.1)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)

  :custom-face
  (lsp-lens-face ((t (:inherit shadow))))
  (lsp-lens-mouse-face ((t (:inherit link))))
  (lsp-ui-doc-background ((t (:background "Gray95"))))
  (lsp-ui-doc-header ((t (:background "Pale Turquoise"))))
  (lsp-ui-doc-border ((t (:background "Gray70"))))
  (lsp-ui-sideline-code-action ((t (:foreground "Tan"))))
  (lsp-ui-sideline-global ((t (:foreground "Gray70"))))
  (lsp-ui-sideline-symbol-info ((t (:foreground "Gray70" :slant italic))))
  (lsp-ui-sideline-current-symbol ((t (:foreground "White" :background "Gray75"))))
  (lsp-ui-sideline-symbol ((t (:foreground "White" :background "Gray75")))))

(use-package company-lsp
  :after lsp-mode

  :commands
  company-lsp

  :custom
  (company-lsp-cache-candidates 'auto))

(use-package aggressive-indent
  :pin melpa

  :hook
  ((emacs-lisp-mode java-mode rust-mode) . aggressive-indent-mode))

(use-package olivetti
  :pin melpa

  :hook
  ((org-mode markdown-mode) . olivetti-mode)

  :custom
  (olivetti-body-width 100))

(use-package jsonnet-mode)

(use-package indent-guide
  :pin melpa

  :hook
  ((python-mode json-mode jsonnet-mode) . indent-guide-mode)

  :custom-face
  (indent-guide-face ((t (:foreground "gray80")))))

(use-package vterm
  :bind
  ("<f6>" . vterm-other-window))

;; TODO Check out EEV
(use-package eev)

(provide 'init)
;;; init ends here

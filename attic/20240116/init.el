;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun fm/generate-password ()
 "Generate a password and insert it."
 (interactive)
 (shell-command "pwgen -c -n -y -s -B -1 34 1" (current-buffer)))

(use-package delight)

(use-package no-littering
 :demand
 :autoload
 no-littering-theme-backups
 no-littering-expand-etc-file-name
 no-littering-expand-var-file-name
 :config
 (no-littering-theme-backups))

(use-package move-text
 :commands move-text-default-bindings
 :init
 (move-text-default-bindings))

(use-package misc
 :ensure nil
 :bind
 ("C-x c" . duplicate-dwim))

(use-package startup
 :ensure nil
 :init
 (fset 'display-startup-echo-area-message 'ignore)
 :custom
 (inhibit-startup-screen t)
 (inhibit-startup-message t)
 (inhibit-startup-buffer-menu t)
 (initial-scratch-message nil)
 (initial-major-mode 'fundamental-mode))

(use-package windmove
 :ensure nil
 :init
 (windmove-default-keybindings)
 (windmove-delete-default-keybindings))

(use-package cua-base
 :init
 (cua-selection-mode 1))

(use-package frame
 :ensure nil
 :custom
 (blink-cursor-mode nil))

(use-package indent
 :ensure nil
 :custom
 (tab-always-indent 'complete)
 (tab-first-completion 'word-or-paren-or-punct))

(use-package bindings
 :ensure nil
 :custom
 (column-number-indicator-zero-based nil))

(use-package modeline
 :ensure nil
 :custom
 (mode-line-compact 'long))

(use-package display-fill-column-indicator
 :ensure nil
 :custom
 (global-display-fill-column-indicator-mode t))

(use-package emacs
 :ensure nil
 :custom
 (resize-mini-windows nil)
 (use-dialog-box nil "Avoid graphical dialog boxes")
 (completion-ignore-case t)
 (read-buffer-completion-ignore-case t)
 (fill-column 90)
 (history-delete-duplicates t)
 (history-length 150)
 (read-process-output-max (* 1024 1024))
 (scroll-conservatively 101)
 (fast-but-imprecise-scrolling t)
 (load-prefer-newer t)
 (coding-system-for-read 'utf-8-unix)
 (coding-system-for-write 'utf-8-unix)
 (use-short-answers t "Respond to yes/no questions using Y/N"))

(use-package xref
 :config
 (add-hook 'xref-after-return #'recenter))

(use-package hippie-exp
 :ensure nil
 :bind
 ([remap dabbrev-expand] . hippie-expand)
 :custom
 (hippie-expand-try-functions-list
  '(try-expand-dabbrev-visible
    try-expand-dabbrev
    try-expand-line
    try-expand-dabbrev-all-buffers
    try-expand-line-all-buffers
    try-expand-dabbrev-from-kill
    try-expand-all-abbrevs
    try-complete-file-name
    try-complete-file-name-partially
    try-expand-list
    try-expand-list-all-buffers
    try-complete-lisp-symbol
    try-complete-lisp-symbol-partially)))

(use-package simple
 :ensure nil
 :custom
 (indent-tabs-mode nil)
 (read-extended-command-predicate #'command-completion-default-include-p
  "Hide commands in M-x that do not work in the current mode")
 (undo-limit (* 1024 1024))
 (suggest-key-bindings 10)
 (save-interprogram-paste-before-kill t)
 (backward-delete-char-untabify-method 'hungry)
 (next-error-message-highlight 'keep)
 :hook
 (before-save . delete-trailing-whitespace))

(use-package window
 :ensure nil
 :bind
 ("C-<f11>"     . scroll-other-window)
 ("C-<f12>"     . scroll-other-window-down)
 ("<f12>"       . delete-other-windows)
 ("M-S-<right>" . next-buffer)
 ("M-S-<left>"  . previous-buffer)
 :custom
 (switch-to-buffer-in-dedicated-window 'pop)
 (switch-to-buffer-obey-display-actions t)
 (split-height-threshold 160)
 (even-window-sizes 'width-only)
 (switch-to-prev-buffer-skip-regexp '("\\`\\*.*\\'")))

(use-package hotfuzz
 :demand)

(use-package orderless
 :demand)

(use-package prescient
 :demand
 :autoload prescient-persist-mode
 :custom
 (prescient-sort-full-matches-first t)
 :config
 (prescient-persist-mode))

(use-package minibuffer
 :ensure nil
 :custom
 (minibuffer-electric-default-mode t)
 (minibuffer-message-clear-timeout 4)
 ;; (completion-auto-help 'visible)
 ;; (completions-format 'one-column)
 ;; (completion-category-defaults nil)
 (completions-sort #'prescient-sort)
 (completion-styles '(prescient orderless hotfuzz))
 (completions-max-height 20)
 (read-file-name-completion-ignore-case t)
 (read-answer-short t)
 (completions-detailed t)
 (completions-group t))

(use-package files
 :ensure nil
 :custom
 (confirm-kill-processes nil)
 (auto-save-default nil)
 (backup-inhibited nil)
 (make-backup-files t)
 (delete-old-versions t)
 (mode-require-final-newline 'visit-save)
 (require-final-newline 'visit-save)
 (major-mode-remap-alist
  '((c-mode . c-ts-mode)
    (c++-mode . c++-ts-mode)
    (c-or-c++-mode . c-or-c++-ts-mode)
    (rust-mode . rust-ts-mode)
    (sh-mode . bash-ts-mode)
    (toml-mode . toml-ts-mode)
    (json-mode . json-ts-mode)
    (cmake-mode . cmake-ts-mode)
    (python-mode . python-ts-mode)
    (dockerfile-mode . dockerfile-ts-mode))))

(use-package saveplace
 :ensure nil
 :init
 (save-place-mode))

(use-package savehist
 :ensure nil
 :init
 (savehist-mode))

(use-package recentf
 :ensure nil
 :init
 (recentf-mode)
 :config
 (defun init/do-recentf-exclude (dir)
  (add-to-list 'recentf-exclude dir))
 (declare-function init/do-recentf-exclude 'init)
 (init/do-recentf-exclude (expand-file-name package-user-dir))
 (init/do-recentf-exclude (no-littering-expand-etc-file-name ""))
 (init/do-recentf-exclude (no-littering-expand-var-file-name ""))
 (init/do-recentf-exclude "/usr/share/emacs")
 (mapc 'init/do-recentf-exclude native-comp-eln-load-path)
 :custom
 (recentf-max-menu-items 50)
 (recentf-max-saved-items 100))

(use-package help
 :ensure nil
 :custom
 (help-window-select t))

(use-package fill
 :ensure nil
 :custom
 (colon-double-space t)
 (default-justification 'left))

(use-package mouse
 :ensure nil
 :custom
 (mouse-yank-at-point t))

(use-package uniquify
 :ensure nil
 :custom
 (uniquify-buffer-name-style 'forward))

(use-package tooltip
 :ensure nil
 :custom
 (tooltip-use-echo-area t))

(use-package vc
 :ensure nil
 :custom
 (vc-make-backup-files t))

(use-package newcomment
 :ensure nil
 :custom
 (comment-fill-column 80))

(use-package ediff-wind
 :ensure nil
 :custom
 (ediff-split-window-function 'split-window-horizontally)
 (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package display-line-numbers
 :ensure nil
 :custom
 (display-line-numbers-grow-only t)
 (display-line-numbers-width-start t)
 :hook
 (prog-mode conf-desktop-mode))

(use-package whitespace
 :ensure nil
 :delight " Ws"
 :custom
 (whitespace-line-column fill-column)
 (show-trailing-whitespace nil)
 (whitespace-action '(cleanup auto-cleanup))
 (whitespace-style
  '(face tabs lines-tail empty tab-mark missing-newline-at-eof
    space-after-tab  space-after-tab::tab    space-after-tab::space
    space-before-tab space-before-tab::space space-before-tab::tab
    indentation      indentation::tab        indentation::space))
 :hook
 (make-mode emacs-lisp-mode hledger-mode))

(use-package symbol-overlay
 :delight
 :custom
 (symbol-overlay-idle-time 0.1)
 :hook
 (emacs-lisp-mode hledger-mode meson-mode))

(use-package elisp-mode
 :ensure nil
 :custom
 (lisp-indent-offset 1)
 (lisp-indent-function 'common-lisp-indent-function))

(use-package highlight-defined :hook emacs-lisp-mode)
(use-package highlight-quoted  :hook emacs-lisp-mode)
(use-package eros              :hook emacs-lisp-mode)
(use-package ipretty           :hook emacs-lisp-mode)
(use-package suggest)

(use-package org-indent
 :ensure nil
 :hook org-mode)

(use-package eldoc
 :ensure nil
 :delight " Ed"
 :custom
 (eldoc-documentation-strategy 'eldoc-documentation-compose)
 :hook
 prog-mode)

(use-package paren
 :ensure nil
 :custom
 (show-paren-style 'mixed)
 (show-paren-highlight-openparen t)
 (show-paren-context-when-offscreen 'overlay)
 (show-paren-when-point-inside-paren t)
 (show-paren-when-point-in-periphery t)
 :hook
 ((prog-mode conf-desktop-mode) . show-paren-mode))

(use-package elec-pair
 :ensure nil
 :hook
 ((prog-mode conf-desktop-mode) . electric-pair-local-mode))

(use-package electric
 :ensure nil
 :hook
 ((prog-mode conf-desktop-mode) . electric-layout-local-mode))

(use-package hl-line
 :ensure nil
 :hook
 (prog-mode conf-desktop-mode))

(use-package bug-reference
 :ensure nil
 :hook
 prog-mode)

(use-package subword
 :ensure nil
 :delight " Sw"
 :hook (rust-mode rust-ts-mode))

(use-package which-key
 :delight
 :init
 (which-key-mode)
 :custom
 (which-key-idle-delay 0.5)
 (which-key-show-docstrings nil)
 (which-key-add-column-padding 3)
 (which-key-max-description-length nil)
 (which-key-max-display-columns nil))

(use-package spell-fu
 :autoload
 spell-fu-dictionary-add
 spell-fu-get-ispell-dictionary
 spell-fu-get-personal-dictionary
 :hook
 ((text-mode)
  (spell-fu-mode .
   (lambda ()
    (spell-fu-dictionary-add
     (spell-fu-get-ispell-dictionary "en"))
    (spell-fu-dictionary-add
     (spell-fu-get-personal-dictionary "en-personal" "~/.aspell.en.pws")))))
 :config
 (add-to-list 'spell-fu-faces-exclude 'link))

(use-package flyspell
 :delight " Fs"
 :custom
 (ispell-program-name "aspell")
 (ispell-local-dictionary "en_US")
 :config
 (add-to-list 'ispell-extra-args "--sug-mode=ultra")
 :hook
 ((prog-mode conf-desktop-mode) . flyspell-prog-mode))

(use-package autorevert
 :custom
 (auto-revert-interval 1)
 (auto-revert-avoid-polling t)
 (buffer-auto-revert-by-notification t)
 (auto-revert-mode-text " Ar"))

(use-package dirvish
 :init
 (dirvish-override-dired-mode))

(use-package sh-script
 :ensure nil
 :custom
 (sh-basic-offset 2)
 (sh-indentation 2)
 :hook
 ((sh-mode bash-ts-mode) .
  (lambda ()
   (use-package executable
    :ensure nil
    :after files
    :hook
    (after-save . executable-make-buffer-file-executable-if-script-p)))))

(use-package python
 :hook ((python-mode python-ts-mode) . (lambda () (setopt fill-column 80))))

(use-package jit-lock
 :ensure nil
 :custom
 (jit-lock-stealth-time 0.1)
 (jit-lock-chunk-size 4000 "A little more than what can fit on the screen")
 (jit-lock-antiblink-grace nil))

(use-package markdown-mode)
(use-package dockerfile-mode)
(use-package pkgbuild-mode)
(use-package meson-mode)
(use-package toml-mode)
(use-package systemd)

(use-package eldoc-toml
 :delight
 :hook toml-mode)

(use-package indent-guide
 :hook (json-mode json-ts-mode))

(use-package tree-sitter
 :delight " Ts"
 :hook yaml-mode)

(use-package tree-sitter-langs
 :hook (tree-sitter-mode . (lambda () (tree-sitter-langs-install-grammars t))))

(use-package tree-sitter-hl
 :ensure nil
 :hook tree-sitter-mode)

(use-package treesit
 :ensure nil
 :custom
 (treesit-language-source-alist
  '((bash   . ("https://github.com/tree-sitter/tree-sitter-bash"))
    (c      . ("https://github.com/tree-sitter/tree-sitter-c"))
    (cpp    . ("https://github.com/tree-sitter/tree-sitter-cpp"))
    (json   . ("https://github.com/tree-sitter/tree-sitter-json.git"))
    (python . ("https://github.com/tree-sitter/tree-sitter-python.git"))
    (toml   . ("https://github.com/ikatyang/tree-sitter-toml.git"))
    (yaml   . ("https://github.com/ikatyang/tree-sitter-yaml.git")))))

(use-package mwim
 :bind
 ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line-or-comment)
 ([remap move-end-of-line]       . mwim-end-of-code-or-line))

(use-package expand-region
 :bind
 ("C-=" . er/expand-region))

(use-package transient
 :custom
 (transient-default-level 7))

(use-package magit
 :autoload magit-after-save-refresh-status
 :bind
 ("C-x g" . magit-status)
 :custom
 (magit-log-section-commit-count 20)
 (magit-auto-revert-tracked-only nil)
 (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
 (magit-bury-buffer-function 'magit-restore-window-configuration)
 (magit-repository-directories '(("~/Workspace" . 3)))
 :config
 (add-hook 'after-save-hook #'magit-after-save-refresh-status))

(use-package magit-diff
 :ensure nil
 :custom
 (magit-revision-show-gravatars t)
 (magit-revision-fill-summary-line fill-column))

(use-package diff-hl
 :hook (prog-mode conf-desktop-mode)
 :custom
 (diff-hl-draw-borders nil)
 (diff-hl-flydiff-delay 0.1)
 :hook
 (magit-pre-refresh . diff-hl-magit-pre-refresh)
 (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package multiple-cursors
 :bind
 ("C->" . mc/mark-next-like-this)
 ("C-<" . mc/mark-previous-like-this))

(use-package multiple-cursors-core
 :ensure nil
 :custom
 (mc/always-run-for-all t))

(use-package volatile-highlights
 :commands volatile-highlights-mode
 :delight
 :init
 (volatile-highlights-mode))

(use-package yaml-mode
 :mode "\\clang-format\\'"
 :bind (:map yaml-mode-map ("C-c p" . fm/generate-password)))

(use-package hledger-mode
 :mode ("\\.journal\\'" "\\.ledger\\'")
 :custom
 (hledger-currency-string "EUR")
 (hledger-current-overlay t)
 (hledger-comments-column 1)
 :hook
 (hledger-mode . (lambda () (setq-local tab-width 1))))

(use-package buffer-move
 :bind
 ("C-x m" . buf-move))

(use-package deadgrep
 :bind
 ("M-F" . deadgrep)
 :config
 (require 'wgrep-deadgrep))

(use-package wgrep-deadgrep)

(use-package flycheck
 :commands flycheck-next-error flycheck-previous-error
 :hook
 (prog-mode yaml-mode hledger-mode)
 :custom
 (flycheck-checker-error-threshold nil)
 (flycheck-mode-line-prefix "Fc")
 (flycheck-idle-change-delay 0.2)
 (flycheck-idle-buffer-switch-delay 0.2)
 (flycheck-display-errors-delay 0.2)
 :bind
 (:map flycheck-mode-map
  ("M-n" .
   (lambda ()
    (interactive)
    (flycheck-next-error)
    (recenter)))
  ("M-p" .
   (lambda ()
    (interactive)
    (flycheck-previous-error)
    (recenter)))))

(use-package flycheck-posframe
 :custom
 (flycheck-posframe-position 'window-bottom-left-corner)
 (flycheck-posframe-border-width 1)
 :hook
 flycheck-mode)

(use-package flycheck-hledger
 :demand t
 :after (flycheck hledger-mode)
 :custom
 ;; TODO Also add "accounts"
 (flycheck-hledger-checks '("commodities")))

(use-package company
 :delight " Co"
 :hook (prog-mode yaml-mode hledger-mode systemd-mode)
 :custom
 (company-tooltip-align-annotations t)
 (company-keywords-ignore-case t)
 (company-idle-delay 0.5)
 :bind (:map company-mode-map ("<tab>" . company-indent-or-complete-common)))

(use-package company-posframe
 :delight
 :custom
 (company-posframe-quickhelp-x-offset 2)
 :hook company-mode)

(use-package company-prescient
 :hook company-mode
 :custom
 (company-prescient-sort-length-enable nil))

(use-package lsp-mode
 :delight " Ls"
 :hook
 (prog-mode . lsp)
 (lsp-mode . lsp-enable-which-key-integration)
 :bind
 (:map lsp-mode-map
  ([remap er/expand-region] . lsp-extend-selection)
  ("C-c r" . lsp-rename)
  ("C-c h" . lsp-describe-thing-at-point)
  ("M-RET" . lsp-execute-code-action))
 :custom
 (lsp-semantic-tokens-enable t)
 (lsp-enable-relative-indentation t))

(use-package lsp-treemacs
 :hook
 (lsp-mode . lsp-treemacs-sync-mode)
 :bind
 (:map lsp-mode-map
  ("C-c e" . lsp-treemacs-errors-list)
  ("C-c s" . lsp-treemacs-symbols)
  ("C-c c" . lsp-treemacs-call-hierarchy)
  ("C-c t" . lsp-treemacs-type-hierarchy)))

(use-package c-ts-mode
 :ensure nil
 :after lsp-mode
 :bind (:map c-ts-base-mode-map ("<f2>" . lsp-clangd-find-other-file)))

(use-package ivy
 :delight
 :bind (:map ivy-minibuffer-map ("<RET>" . ivy-alt-done))
 :custom
 (ivy-use-selectable-prompt t)
 (ivy-use-virtual-buffers t)
 (ivy-count-format "(%d/%d) ")
 (ivy-virtual-abbreviate 'abbreviate)
 (ivy-extra-directories nil)
 :init
 (ivy-mode))

(use-package ivy-rich
 :custom
 (ivy-rich-path-style 'abbrev)
 :init
 (ivy-rich-mode))

(use-package ivy-xref
 :demand
 :custom
 (xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package lsp-ivy
 :after lsp-mode
 :bind
 (:map lsp-mode-map ("C-c x" . lsp-ivy-workspace-symbol)))

(use-package nerd-icons-ivy-rich
 :init
 (nerd-icons-ivy-rich-mode))

(use-package ivy-prescient
 :init
 (ivy-prescient-mode))

(use-package counsel
 :delight
 :init
 (counsel-mode))

(use-package swiper
 :bind
 ([remap isearch-forward] . swiper-isearch)
 ([remap isearch-backward] . swiper-isearch-backward)
 :custom
 (swiper-include-line-number-in-search t)
 (swiper-action-recenter t))

(use-package projectile
 :delight " Pr"
 :bind (:map projectile-mode-map ("C-x p" . projectile-command-map))
 :custom
 (projectile-project-search-path '("~/Workspace"))
 (projectile-sort-order 'recently-active)
 :init
 (projectile-mode))

(use-package counsel-projectile
 :custom
 (counsel-projectile-mode t))

(use-package treemacs-projectile)

(use-package flyspell-correct-ivy
 :bind (:map flyspell-mode-map ("C-M-;" . flyspell-correct-wrapper))
 :custom
 (flyspell-correct-interface #'flyspell-correct-ivy))

;; (use-package flycheck
;;  :hook
;;  (:map flycheck-mode-map
;;   ("C-c ! L" . consult-flycheck)))

;; (use-package consult-flycheck)

;; (use-package consult-lsp
;; :after lsp-mode
;; :bind
;; (:map lsp-mode-map
;;  ("C-c x" . consult-lsp-symbols)))

;; (use-package corfu-prescient
;;  :hook corfu-mode
;;  :custom
;;  (corfu-prescient-completion-styles '(prescient orderless hotfuzz)))

;; (use-package vertico-prescient
;;  :hook vertico-mode
;;  :custom
;;  (vertico-prescient-completion-styles '(prescient orderless hotfuzz)))

;; (use-package ctrlf
;;  :init
;;  (ctrlf-mode)
;;  :custom
;;  (ctrlf-default-search-style 'fuzzy)
;;  (ctrlf-auto-recenter t))

;; (use-package vertico
;;  :commands vertico-mode
;;  :init
;;  (vertico-mode)
;;  :custom
;;  (vertico-preselect 'first)
;;  :custom-face
;;  (vertico-current ((t (:weight normal)))))

;; (use-package consult
;;  :custom
;;  (consult-preview-key nil)
;;  :bind
;;  ([remap switch-to-buffer] . consult-buffer)
;;  ([remap isearch-forward] . consult-line)
;;  ([remap isearch-backward] . consult-line))

;; (use-package marginalia
;;  :commands marginalia-mode
;;  :init
;;  (marginalia-mode)
;;  :bind (:map minibuffer-local-map ("M-a" . marginalia-cycle))
;;  :custom
;;  (marginalia-separator " | "))

;; (use-package nerd-icons-completion
;;  :commands nerd-icons-completion-mode
;;  :after marginalia
;;  :init
;;  (nerd-icons-completion-mode)
;;  :hook
;;  (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; (use-package corfu
;;  :hook
;;  (prog-mode systemd-mode)
;;  :custom
;;  (corfu-cycle t))

;; (use-package corfu-echo
;;  :ensure nil
;;  :hook corfu-mode)

;; (use-package corfu-history
;;  :ensure nil
;;  :hook corfu-mode)

;; (use-package corfu-indexed
;;  :ensure nil
;;  :hook corfu-mode)

;; (use-package corfu-popupinfo
;;  :ensure nil
;;  :hook corfu-mode
;;  :custom
;;  (corfu-popupinfo-delay: '(1.0 . 0.5)))

;; (use-package nerd-icons-corfu
;;  :after corfu
;;  :config
;;  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; (use-package cape
;;  :hook
;;  (prog-mode .
;;   (lambda ()
;;    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;    (add-to-list 'completion-at-point-functions #'cape-file))))

;; (fm/pkg blamer
;;  (fm/after blamer
;;   (setq-default blamer-idle-time 0)
;;   (setq-default blamer-commit-formatter ": %s")
;;   (setq-default blamer-datetime-formatter "%s")
;;   (setq-default blamer-max-commit-message-length 60))
;;  (fm/after prog-mode
;;   (fm/key-local "C-c b" blamer-mode prog-mode-map)))

;; (fm/pkg sideline
;;  (fm/after sideline
;;   (fm/dim sideline-mode "Si")
;;   (setq-default sideline-delay 0.1)))

;; (fm/pkg sideline-blame
;;  (fm/after sideline
;;   (setq-default sideline-backends-right '(sideline-blame))
;;   (setq-default sideline-blame-commit-format "- %s")))

;; (fm/pkg ivy-yasnippet)

;; (fm/pkg yasnippet-snippets
;;  (fm/after yasnippet
;;   (yasnippet-snippets-initialize)))

;; (fm/pkg yasnippet
;;  (fm/after yasnippet
;;   (fm/dim yas-minor-mode "Ys")
;;   ;; (fm/after company
;;   ;;  (fm/hookn yas-minor-mode-hook (fm/company-add-backend 'company-yasnippet)))))
;;   ))

;; (fm/pkg scopeline
;;  (fm/after tree-sitter
;;   (fm/hook tree-sitter-mode-hook scopeline-mode))
;;  (fm/after scopeline
;;   (fm/dim scopeline-mode "Sl")
;;   (setq-default scopeline-min-lines 10)))

;; (fm/pkg rust-mode
;;  (fm/after rust-mode
;;   (fm/key-local "<f5>" rust-dbg-wrap-or-unwrap            rust-mode-map "rust-utils")
;;   (fm/key-local "<f6>" lsp-rust-analyzer-expand-macro     rust-mode-map "lsp-rust")
;;   (fm/key-local "<f7>" lsp-rust-analyzer-join-lines       rust-mode-map "lsp-rust")
;;   (fm/key-local "<f8>" lsp-rust-analyzer-inlay-hints-mode rust-mode-map "lsp-rust")
;;   (setq-default rust-indent-offset 2)
;;   (setq-default rust-load-optional-libraries nil)
;;   (setq-default rust-format-on-save t)
;;   (fm/hookn rust-mode-hook (electric-quote-local-mode -1))
;;   (fm/hook rust-mode-hook subword-mode)
;;   (fm/hook rust-mode-hook lsp)))

;; (fm/after rust-ts-mode
;;  (fm/key-local "<f5>" rust-dbg-wrap-or-unwrap            rust-ts-mode-map "rust-utils")
;;  (fm/key-local "<f6>" lsp-rust-analyzer-expand-macro     rust-ts-mode-map "lsp-rust")
;;  (fm/key-local "<f7>" lsp-rust-analyzer-join-lines       rust-ts-mode-map "lsp-rust")
;;  (fm/key-local "<f8>" lsp-rust-analyzer-inlay-hints-mode rust-ts-mode-map "lsp-rust")
;;  (setq-default rust-ts-mode-indent-offset 2)
;;  (fm/hookn rust-ts-mode-hook (electric-quote-local-mode -1))
;;  (fm/hook rust-ts-mode-hook subword-mode)
;;  (fm/hook rust-ts-mode-hook lsp))

;; (fm/pkg lsp-mode
;;  (fm/after lsp-mode
;;   (fm/dim lsp-mode "Ls")
;;   (fm/key-local "C-c f" lsp-format-buffer           lsp-mode-map "lsp-mode")
;;   (fm/key-local "C-c g" lsp-format-region           lsp-mode-map "lsp-mode")
;;   (fm/key-local "C-c r" lsp-rename                  lsp-mode-map "lsp-mode")
;;   (fm/key-local "C-c h" lsp-describe-thing-at-point lsp-mode-map "lsp-mode")
;;   (fm/key-local "C-="   lsp-extend-selection        lsp-mode-map "lsp-mode")
;;   (fm/key-local "M-RET" lsp-execute-code-action     lsp-mode-map "lsp-mode")
;;   (setq-default lsp-progress-prefix "  Progress: ")
;;   (setq-default lsp-completion-show-detail t)
;;   (setq-default lsp-completion-show-kind t)
;;   (setq-default lsp-completion-provider :none)
;;   (setq-default lsp-headerline-breadcrumb-enable t)
;;   (setq-default lsp-restart 'auto-restart)
;;   (setq-default lsp-enable-snippet t)
;;   (setq-default lsp-keymap-prefix "C-c")
;;   (setq-default lsp-idle-delay 0.1)
;;   (setq-default lsp-file-watch-threshold nil)
;;   (setq-default lsp-enable-semantic-highlighting t)
;;   (setq-default lsp-enable-indentation t)
;;   (setq-default lsp-enable-on-type-formatting nil)
;;   (setq-default lsp-before-save-edits nil)
;;   (setq-default lsp-auto-configure t)
;;   (setq-default lsp-signature-auto-activate t)
;;   (setq-default lsp-signature-render-documentation nil)
;;   (setq-default lsp-eldoc-enable-hover t)
;;   (setq-default lsp-eldoc-render-all nil)
;;   (setq-default lsp-modeline-code-actions-enable nil)
;;   (setq-default lsp-modeline-diagnostics-enable t)
;;   (setq-default lsp-log-io nil)
;;   (setq-default lsp-keep-workspace-alive nil)
;;   (setq-default lsp-enable-imenu nil)
;;   (fm/after which-key
;;    (fm/hook lsp-mode-hook lsp-enable-which-key-integration "lsp-mode")))
;;  (fm/after lsp-lens
;;   (fm/dim lsp-lens-mode)
;;   (setq-default lsp-lens-mode nil)
;;   (setq-default lsp-lens-enable nil))
;;  (fm/after lsp-headerline
;;   (setq-default lsp-headerline-breadcrumb-icons-enable nil))
;;  (fm/after lsp-semantic-tokens
;;   (setq-default lsp-semantic-tokens-apply-modifiers t))
;;  (fm/after lsp-rust
;;   ;; (setq-default lsp-rust-analyzer-max-inlay-hint-length 50)
;;   ;; (setq-default lsp-rust-unstable-features t)
;;   (setq-default lsp-rust-analyzer-checkonsave-features "all")
;;   (setq-default lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
;;   (setq-default lsp-rust-analyzer-proc-macro-enable t)
;;   (setq-default lsp-rust-racer-completion nil)
;;   (setq-default lsp-rust-build-bin t)
;;   (setq-default lsp-rust-build-lib t)
;;   (setq-default lsp-rust-clippy-preference "on")
;;   (setq-default lsp-rust-analyzer-server-display-inlay-hints t)
;;   (setq-default lsp-rust-analyzer-display-chaining-hints t)
;;   (setq-default lsp-rust-analyzer-display-parameter-hints t)
;;   (setq-default lsp-rust-analyzer-display-closure-return-type-hints t)
;;   (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-enable "always")
;;   (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
;;   (setq-default lsp-rust-analyzer-binding-mode-hints t)
;;   (setq-default lsp-rust-analyzer-display-reborrow-hints "mutable")
;;   (setq-default lsp-rust-all-features t)
;;   (setq-default lsp-rust-all-targets t)
;;   (setq-default lsp-rust-full-docs t)
;;   (setq-default lsp-rust-analyzer-cargo-watch-command "clippy"))
;;  (fm/after lsp-clangd
;;   (setq-default lsp-clients-clangd-args
;;    '("--header-insertion-decorators"
;;      "--all-scopes-completion"
;;      "--clang-tidy"
;;      "--completion-style=detailed"
;;      "--header-insertion=iwyu"
;;      ;; Breaks clangd-14
;;      ; "--header-insertion-decorators"
;;      "--inlay-hints"
;;      "-j=8"
;;      "--malloc-trim"
;;      "--pch-storage=memory"
;;      "--background-index"
;;      "--function-arg-placeholders"
;;      "--limit-references=0"
;;      "--limit-results=0"))
;;   (fm/after cc-mode
;;    (fm/autoload lsp-clangd-find-other-file "lsp-clangd")
;;    (fm/key-local "<f2>" lsp-clangd-find-other-file c-mode-base-map))
;;   (fm/after c-ts-mode
;;    (fm/autoload lsp-clangd-find-other-file "lsp-clangd")
;;    (fm/key-local "<f2>" lsp-clangd-find-other-file c-ts-base-mode-map))))

;; (fm/pkg treemacs
;;  (fm/key "<f9>" treemacs-select-window)
;;  (fm/after treemacs-customization
;;   (setq-default treemacs-width 40)
;;   (setq-default treemacs-indentation 1))
;;  (fm/after treemacs
;;   (setq-default treemacs-select-when-already-in-treemacs 'move-back))
;;   ;; (setq-default treemacs-indent-guide-mode t))
;;  (fm/after treemacs-interface
;;   (fm/key "<f12>" treemacs-delete-other-windows "treemacs-interface"))
;;  ;; (fm/after treemacs-header-line
;;  ;;  (setq-default treemacs-indicate-top-scroll-mode t))
;;  (fm/after treemacs-mode
;;   (fm/hook treemacs-mode-hook treemacs-tag-follow-mode "treemacs-tag-follow-mode")
;;   (fm/hook treemacs-mode-hook treemacs-fringe-indicator-mode "treemacs-fringe-indicator")
;;   (fm/hook treemacs-mode-hook treemacs-filewatch-mode "treemacs-filewatch-mode")
;;   ;; (fm/hook treemacs-mode-hook treemacs-indicate-top-scroll-mode "treemacs-header-line")
;;   ;; (fm/autoload treemacs-indent-guide-mode "treemacs-visuals")
;;   ;; (fm/hookn treemacs-mode-hook (treemacs-indent-guide-mode))
;;   (fm/autoload treemacs-git-mode "treemacs-async")
;;   (fm/hookn treemacs-mode-hook (treemacs-git-mode 'deferred))
;;   (fm/hook treemacs-mode-hook
;;    treemacs-git-commit-diff-mode
;;    "treemacs-git-commit-diff-mode")))

;; (fm/pkg lsp-treemacs
;;  (fm/after lsp-mode
;;   (fm/key-local "C-c e" lsp-treemacs-errors-list    lsp-mode-map)
;;   (fm/key-local "C-c s" lsp-treemacs-symbols        lsp-mode-map)
;;   (fm/key-local "C-c c" lsp-treemacs-call-hierarchy lsp-mode-map)
;;   (fm/key-local "C-c t" lsp-treemacs-type-hierarchy lsp-mode-map)
;;   (fm/hook lsp-mode-hook lsp-treemacs-sync-mode)))

;; (fm/pkg treemacs-projectile)

;; (fm/pkg lsp-ui
;;  (fm/after lsp-ui-doc
;;   (setq-default lsp-ui-doc-enable t)
;;   (setq-default lsp-ui-doc-show-with-cursor nil)
;;   (setq-default lsp-ui-doc-show-with-mouse t)
;;   (setq-default lsp-ui-doc-alignment 'frame)
;;   (setq-default lsp-ui-doc-header t)
;;   (setq-default lsp-ui-doc-include-signature t)
;;   (setq-default lsp-ui-doc-max-height 30)
;;   (setq-default lsp-ui-doc-use-webkit t))
;;  (fm/after lsp-ui-peek
;;   (setq-default lsp-ui-peek-list-width 40)
;;   (setq-default lsp-ui-peek-always-show t))
;;  (fm/after lsp-ui-sideline
;;   (setq-default lsp-ui-sideline-enable nil))
;;  (fm/after lsp-ui
;;   (fm/key-local "M-."   lsp-ui-peek-find-definitions    lsp-ui-mode-map "lsp-ui-peek")
;;   (fm/key-local "M-?"   lsp-ui-peek-find-references     lsp-ui-mode-map "lsp-ui-peek")
;;   (fm/key-local "M-I"   lsp-ui-peek-find-implementation lsp-ui-mode-map "lsp-ui-peek")
;;   (fm/key-local "C-c d" lsp-ui-doc-show                 lsp-ui-mode-map "lsp-ui-doc")
;;   ;; (fm/key-local "C-c l" lsp-ui-flycheck-list            lsp-ui-mode-map "lsp-ui-flycheck")))
;;   ))

;; (fm/pkg surround
;;  (require 'surround)
;;  (fm/key "M-'" surround-mark-inner)
;;  (fm/key "M-\"" surround-insert))

;; Print startup stats.
(message "Startup in %s (%d GC runs)" (emacs-init-time) gcs-done)

(provide 'init)
;;; init.el ends here

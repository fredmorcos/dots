;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; No Littering

;; Directories -- TODO REMOVE THESE
(defconst emacs-user-dir (expand-file-name user-emacs-directory))
(defconst emacs-var-dir (concat emacs-user-dir "var/"))
(defconst emacs-projectile-cache-file (concat emacs-var-dir "projectile-cache"))
(defconst emacs-projectile-projects-file (concat emacs-var-dir "projectile-projects"))
(make-directory emacs-var-dir t)

(use-package no-littering
 :ensure t
 :demand t

 :commands
 no-littering-theme-backups
 no-littering-expand-etc-file-name
 no-littering-expand-var-file-name

 :config
 (no-littering-theme-backups))

;;; Diminish

(use-package diminish
 :ensure t
 :defer t)

;;; Functions

(use-package emacs
 :ensure nil
 :defer t

 :config
 ;; Enable these functions.
 (put 'list-timers      'disabled nil)
 (put 'narrow-to-region 'disabled nil)
 (put 'narrow-to-page   'disabled nil)
 (put 'upcase-region    'disabled nil)
 (put 'downcase-region  'disabled nil)
 ;; Disable these functions.
 (put 'eshell           'disabled t)
 (put 'overwrite-mode   'disabled t)
 (put 'iconify-frame    'disabled t)
 (put 'suspend-frame    'disabled t)
 (put 'diary            'disabled t))

;;; Text Editing

;; TODO: Do I still need this?
;; (use-package elec-pair
;;  :ensure nil

;;  :custom
;;  (electric-pair-pairs '((?\[ . ?\]))))

(use-package qol
 :ensure nil
 :defer t
 :load-path "/home/fred/Workspace/dots/emacs/"

 :bind
 (("C-x j"    . qol/insert-buffer-name)
  ("C-x e"    . qol/replace-escapes)
  ("{"        . qol/insert-pair-curly)
  ("{"        . qol/insert-pair-curly)
  ("("        . qol/insert-pair-parens)
  ("'"        . qol/insert-pair-quote)
  ("\""       . qol/insert-pair-double-quotes)
  ("`"        . qol/insert-pair-backtick)))

(use-package misc
 :ensure nil
 :defer t

 :bind
 (("C-x c" . duplicate-dwim)))

(use-package cua-base
 :ensure nil
 :defer t

 :init
 (cua-selection-mode t))

(use-package move-text
 :ensure t
 :defer t

 :init
 (move-text-default-bindings))

(use-package mwim
 :ensure t
 :defer t

 :bind
 ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line-or-comment)
 ([remap move-end-of-line] . mwim-end-of-code-or-line))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 (tab-always-indent 'complete)
 (tab-first-completion 'word-or-paren-or-punct))

(use-package simple
 :ensure nil
 :defer t

 :custom
 (indent-tabs-mode nil))

(use-package unfill
 :ensure t
 :defer t

 :bind
 ([remap fill-paragraph] . unfill-toggle))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 (fill-column 90))

(use-package newcomment
 :ensure nil
 :defer t

 :custom
 (comment-fill-column 80))

(use-package files
 :ensure nil
 :defer t

 :custom
 (mode-require-final-newline 'visit-save)
 (require-final-newline 'visit-save)
 (coding-system-for-read 'utf-8-unix)
 (coding-system-for-write 'utf-8-unix))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 ;; Fill
 (colon-double-space t)
 (default-justification 'left))

(use-package expand-region
 :ensure t
 :defer t

 :bind
 ("C-=" . er/expand-region))

;;; Auto-save & backups

(use-package files
 :ensure nil
 :defer t

 :custom
 (auto-save-default t))

(use-package files
 :ensure nil
 :defer t

 :custom
 (backup-inhibited nil)
 (make-backup-files t)
 (load-prefer-newer t)
 (delete-old-versions t))

;;; UI

(use-package uniquify
 :ensure nil
 :defer t

 :custom
 (uniquify-buffer-name-style 'forward))

(use-package tooltip
 :ensure nil
 :defer t

 :custom
 (tooltip-use-echo-area t))

(use-package display-line-numbers
 :ensure nil
 :defer t

 :custom
 (display-line-numbers-grow-only t)
 (display-line-numbers-width-start t))

;;; UX

(use-package windmove
 :ensure nil
 :defer t

 :init
 (windmove-default-keybindings)
 (windmove-delete-default-keybindings))

(use-package warnings
 :ensure nil
 :defer t

 :config
 ;; Suppress certain annoying warnings.
 (add-to-list 'warning-suppress-types 'defvaralias))

(use-package files
 :ensure nil
 :defer t

 :custom
 (confirm-kill-processes nil))

(use-package help
 :ensure nil
 :defer t

 :custom
 (help-window-select t))

(use-package mouse
 :ensure nil
 :defer t

 :custom
 (mouse-yank-at-point t)
 (mouse-1-click-follows-link 'double))

(use-package simple
 :ensure nil
 :defer t

 :custom
 ;; Hide commands in M-x that do not work in the current mode
 (read-extended-command-predicate #'command-completion-default-include-p)
 (undo-limit (* 1024 1024))
 (suggest-key-bindings 10)
 (save-interprogram-paste-before-kill t)
 (backward-delete-char-untabify-method 'hungry)
 (next-error-message-highlight t))

(use-package simple
 :ensure nil
 :defer t
 :after files

 :hook
 (before-save . delete-trailing-whitespace))

;;; Scrolling

(use-package emacs
 :ensure nil
 :defer t

 :custom
 (scroll-conservatively 104)
 (scroll-margin 1)
 (hscroll-margin 1)
 (hscroll-step 1)
 (auto-hscroll-mode 'current-line)
 (fast-but-imprecise-scrolling t)

 :preface
 (defun init/scroll-other-window ()
  "Scroll up the other window in a split frame."
  (interactive)
  (scroll-other-window 1))

 (defun init/scroll-other-window-down ()
  "Scroll down the other window in a split frame."
  (interactive)
  (scroll-other-window-down 1))

 :bind
 (("<f10>" . init/scroll-other-window)
  ("<f11>" . init/scroll-other-window-down)))

(use-package simple
 :ensure nil
 :defer t

 :bind
 ("<mouse-4>" . previous-line)
 ("<mouse-5>" . next-line))

;;; Dynamic Expansion

(use-package hippie-exp
 :ensure nil
 :defer t

 :bind
 ;; Replace dabbrev-expand with hippie-expand
 ([remap dabbrev-expand] . hippie-expand)

 :custom
 (hippie-expand-try-functions-list
  '(try-expand-dabbrev-visible
    try-expand-line
    try-expand-dabbrev
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

(use-package abbrev
 :ensure nil
 :defer t
 :diminish "Ab")

;;; Completion

(use-package emacs
 :ensure nil
 :defer t

 :custom
 (completion-ignore-case t)
 (read-buffer-completion-ignore-case t))

;;; History and save-hist

(use-package emacs
 :ensure nil
 :defer t

 :custom
 (history-delete-duplicates t)
 (history-length 150))

(use-package saveplace
 :ensure nil
 :defer t

 :init
 (save-place-mode))

(use-package savehist
 :ensure nil
 :defer t

 :init
 (savehist-mode))

(use-package recentf
 :ensure nil
 :defer t

 :custom
 (recentf-max-menu-items 50)
 (recentf-max-saved-items 100)
 (recentf-exclude `(,(concat (expand-file-name user-emacs-directory) "elpa")
                    ,(no-littering-expand-var-file-name "")
                    ,(no-littering-expand-etc-file-name "")))

 :init
 (recentf-mode))

;;; Windows

(use-package window
 :ensure nil
 :defer t

 :custom
 (switch-to-buffer-in-dedicated-window 'pop)
 (switch-to-buffer-obey-display-actions t)
 (split-height-threshold 160)
 (even-window-sizes 'width-only)
 (switch-to-prev-buffer-skip-regexp '("\\`\\*.*\\'"))

 :preface
 (defmacro init/disable-popup (regexp)
  "Stop buffers that match REGEXP from popping up."
  `(push (cons ,regexp (cons #'display-buffer-no-window nil)) display-buffer-alist))

 :config
 (init/disable-popup "\\`\\*Compile-Log\\*.*\\'")
 (init/disable-popup "\\`\\*Native-compile-Log\\*.*\\'")
 (init/disable-popup "\\`\\*Async-native-compile-log\\*.*\\'")
 (init/disable-popup "\\`\\*Warnings\\*.*\\'")

 :bind
 (("<f12>"       . delete-other-windows)
  ("<M-S-right>" . next-buffer)
  ("<M-S-left>"  . previous-buffer)))

;;; General Programming

(use-package eldoc
 :ensure nil
 :defer t
 :diminish "Ed"

 :custom
 (eldoc-documentation-strategy 'eldoc-documentation-compose))

(use-package subword
 :ensure nil
 :defer t
 :diminish "Sw")

(use-package prog-mode
 :ensure nil
 :defer t

 :hook
 (prog-mode . display-fill-column-indicator-mode)
 (prog-mode . goto-address-prog-mode))

(use-package deadgrep
 :ensure t
 :defer t

 :bind
 (:map prog-mode-map
  ("M-F" . deadgrep)))

(use-package wgrep
 :ensure t
 :defer t)

(use-package wgrep-deadgrep
 :ensure nil
 :defer t
 :after deadgrep)

;;; Version Control

(use-package vc
 :ensure nil
 :defer t

 :custom
 (vc-make-backup-files t))

(use-package ediff-wind
 :ensure nil
 :defer t

 :custom
 (ediff-split-window-function #'split-window-right)
 (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package blamer
 :ensure t
 :defer t

 :custom
  (blamer-idle-time 0)
  (blamer-commit-formatter ": %s")
  (blamer-datetime-formatter "%s")
 (blamer-max-commit-message-length 60)

 :bind
 (:map prog-mode-map
  ("C-c b" . blamer-mode)))

(use-package magit-process
 :ensure magit
 :defer t

 :hook
 (magit-process-mode . goto-address-mode))

(use-package magit
 :ensure t
 :defer t

 :bind
 ("C-x g" . magit-status)

 :custom
 (magit-log-section-commit-count 20)
 (magit-auto-revert-tracked-only nil)
 ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
 (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
 (magit-bury-buffer-function #'magit-restore-window-configuration)
 (magit-repository-directories '(("~/Workspace" . 3)))

 :hook
 (after-save . magit-after-save-refresh-status))

(use-package magit-diff
 :ensure magit
 :defer t

 :custom
 (magit-revision-show-gravatars t)
 (magit-revision-fill-summary-line fill-column))

(use-package diff-hl
 :ensure t
 :defer t

 :custom
 (diff-hl-draw-borders nil)
 (diff-hl-flydiff-delay 0.1))

(use-package diff-hl
 :ensure t
 :defer t
 :after magit-mode

 :hook
 (magit-pre-refresh . diff-hl-magit-pre-refresh)
 (magit-post-refresh . diff-hl-magit-post-refresh))

;;; General Features

(use-package symbol-overlay
 :ensure t
 :defer t
 :diminish "Sy"

 :bind
 (:map symbol-overlay-mode-map
  ("M->" . symbol-overlay-jump-next)
  ("M-<" . symbol-overlay-jump-prev))

 :custom
 (symbol-overlay-idle-time 0.1))

(use-package paren
 :ensure nil
 :defer t

 :custom
 (show-paren-when-point-inside-paren t)
 (show-paren-style 'mixed)
 (show-paren-highlight-openparen t)
 (show-paren-context-when-offscreen 'overlay))

(use-package autorevert
 :ensure nil
 :defer t
 :diminish "Ar"

 :custom
 (auto-revert-interval 1)
 (auto-revert-avoid-polling t)
 (buffer-auto-revert-by-notification t)
 (auto-revert-mode-text " Ar"))

(use-package indent-guide
 :ensure t
 :defer t)

(use-package crux
 :ensure t
 :defer t)

;;; Various

(use-package dictionary
 :ensure nil
 :defer t

 :custom
 (dictionary-server "dict.org")
 (dictionary-use-single-buffer t))

(use-package woman
 :ensure nil
 :defer t

 :custom
 (woman-fill-column 100))

;;; Whitespace

(use-package whitespace
 :ensure nil
 :defer t
 :diminish "Ws"

 :custom
 (whitespace-line-column 90)
 (show-trailing-whitespace nil)
 (whitespace-action '(cleanup))
 (whitespace-style
  '(face tabs lines-tail empty tab-mark indentation indentation::tab indentation::space
    space-after-tab space-after-tab::tab space-after-tab::space space-before-tab
    space-before-tab::tab space-before-tab::space whitespace-missing-newline-at-eof)))

;;; Makefiles

(use-package make-mode
 :ensure nil
 :defer t

 :hook
 (makefile-mode . whitespace-mode))

;;; Emacs Lisp

(use-package elisp-mode
 :ensure nil
 :defer t

 :custom
 (lisp-indent-offset 1)
 (lisp-indent-function #'common-lisp-indent-function)

 :hook
 (emacs-lisp-mode . symbol-overlay-mode)
 (emacs-lisp-mode . whitespace-mode))

(use-package highlight-defined
 :ensure t
 :defer t

 :hook
 (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
 :ensure t
 :defer t

 :hook
 (emacs-lisp-mode . highlight-quoted-mode))

(use-package eros
 :ensure t
 :defer t

 :hook
 (emacs-lisp-mode . eros-mode))

(use-package suggest
 :ensure t
 :defer t)

(use-package ipretty
 :ensure t
 :defer t

 :hook
 (emacs-lisp-mode . (lambda () (ipretty-mode t))))

(use-package elsa
 :ensure t
 :defer t)

(use-package flycheck-elsa
 :ensure t
 :defer t

 :hook
 (emacs-lisp-mode . flycheck-elsa-setup))

;;; Shell Scripting

(use-package sh-script
 :ensure nil
 :defer t

 :custom
 (sh-basic-offset 2)
 (sh-indentation 2)

 :preface
 (defun init/make-file-executable ()
  "Makes the file executable on save."
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p 0 t))

 :hook
 (sh-mode . init/make-file-executable))

;;; Dired

(use-package casual-dired
 :ensure t
 :defer t)

(use-package dired-async
 :ensure nil
 :defer t
 :diminish "As")

(use-package dired
 :ensure nil
 :defer t

 :custom
 (dired-mouse-drag-files t)
 (dired-listing-switches "-l --group-directories-first")
 (dired-hide-details-hide-symlink-targets nil)

 :bind
 (:map dired-mode-map
  ("C-o" . casual-dired-tmenu))

 :preface
 (defun init/dired-setup ()
  "Setup dired requires."
  (require 'dired-x)
  (require 'wdired)
  (require 'image-dired)
  (require 'casual-dired))

 :hook
 (dired-mode . init/dired-setup)
 (dired-mode . dired-hide-details-mode)
 (dired-mode . auto-revert-mode)
 (dired-mode . hl-line-mode)
 (dired-mode . context-menu-mode)
 (dired-mode . dired-async-mode))

;;; Search

(use-package isearch
 :ensure nil
 :defer t

 :custom
 (isearch-lazy-count t)
 (isearch-lazy-highlight t))

;;; CMake

(use-package cmake-mode
 :ensure t
 :defer t

 :hook
 (cmake-mode . eldoc-cmake-enable))

(use-package eldoc-cmake
 :ensure t
 :defer t)

;;; Markdown

(use-package markdown-mode
 :ensure t
 :defer t)

;;; Sed

(use-package sed-mode
 :ensure t
 :defer t)

;;; Po Translations

(use-package po-mode
 :ensure t
 :defer t)

;;; TOML

(use-package toml-mode
 :ensure t
 :defer t

 :hook
 (toml-mode . eldoc-toml-mode))

(use-package eldoc-toml
 :ensure t
 :defer t
 :diminish)

;;; JSON

(use-package json-mode
 :ensure t
 :defer t

 :hook
 (json-mode . indent-guide-mode)
 (json-mode . tree-sitter-mode))

;;; Systemd

(use-package systemd
 :ensure t
 :defer t

 :hook
 (systemd-mode . company-mode))

;;; Spell Checking

(use-package flyspell
 :ensure nil
 :defer t
 :diminish "Fs"

 :custom
 (ispell-program-name "aspell")
 (ispell-extra-args '("--sug-mode=ultra"))
 (ispell-local-dictionary "en_US"))

(use-package flyspell-correct
 :ensure nil
 :defer t

 :bind
 (:map flyspell-mode-map
  ("C-;" . flyspell-correct-wrapper))

 :custom
 (flyspell-correct-interface #'flyspell-correct-ivy))

(use-package flyspell-correct-ivy
 :ensure t
 :defer t)

(use-package spell-fu
 :ensure t
 :defer t

 :preface
 (defun init/spell-fu-setup ()
  "Setup spell-fu."
  (defconst user-dict-en (expand-file-name "~/.aspell.en.pws"))
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
  (spell-fu-dictionary-add (spell-fu-get-personal-dictionary "en-personal" user-dict-en))
  (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "de")))

 :hook
 (spell-fu-mode . init/spell-fu-setup)

 :custom
 spell-fu-faces-exclude '(link org-link))

;;; Emacs Tools

(use-package which-key
 :ensure t
 :defer t
 :diminish

 :custom
 (which-key-idle-delay 0.5)
 (which-key-show-docstrings nil)
 (which-key-add-column-padding 3)
 (which-key-max-description-length nil)
 (which-key-max-display-columns nil)

 :init
 (which-key-mode))

(use-package ivy
 :ensure t
 :defer t
 :diminish

 :bind
 (:map ivy-minibuffer-map
  ("<RET>" . ivy-alt-done))

 :custom
 (ivy-wrap t)
 (ivy-use-selectable-prompt t)
 (ivy-use-virtual-buffers t)
 (ivy-count-format "(%d/%d) ")
 (ivy-virtual-abbreviate 'abbreviate)
 (ivy-initial-inputs-alist nil)
 (ivy-extra-directories nil)
 (ivy-re-builders-alist '((t . ivy--regex-ignore-order) (t . ivy--regex-plus)))

 :init
 (ivy-mode))

(use-package ivy-rich
 :ensure t
 :defer t

 :custom
 (ivy-rich-path-style 'abbrev)

 :init
 (ivy-rich-mode))

(use-package counsel
 :ensure t
 :defer t
 :diminish

 :bind
 (:map counsel-mode-map
  ("M-Y" . counsel-yank-pop))

 :config
 (put 'counsel-find-symbol 'no-counsel-M-x t)

 :init
 (counsel-mode))

(use-package marginalia
 :ensure t
 :defer t

 :init
 (marginalia-mode))

(use-package hotfuzz
 :ensure t
 :defer t

 :init
 (push 'hotfuzz completion-styles))

(use-package prescient
 :ensure t
 :defer t

 :custom
 (prescient-sort-full-matches-first t)

 :config
 (push 'literal-prefix prescient-filter-method)
 (push 'prefix prescient-filter-method)
 (push 'anchored prescient-filter-method)

 :init
 (push 'prescient completion-styles)

 :commands
 prescient-persist-mode

 :init
 (prescient-persist-mode))

(use-package ivy-prescient
 :ensure t
 :defer t

 :init
 (ivy-prescient-mode))

(use-package orderless
 :ensure t
 :defer t

 :config
 (push 'orderless-initialism orderless-matching-styles)
 (push 'orderless-prefixes orderless-matching-styles)

 :init
 (push 'orderless completion-styles))

(use-package minibuffer
 :ensure nil
 :defer t

 :config
 (push 'substring completion-styles)
 (push 'flex completion-styles)

 :custom
 (read-file-name-completion-ignore-case t)
 (completions-format 'one-column)
 (completions-detailed t))

(use-package ctrlf
 :ensure t
 :defer t

 :custom
 (ctrlf-default-search-style 'fuzzy)
 (ctrlf-auto-recenter t)

 :init
 (ctrlf-mode))

(use-package consult
 :ensure t
 :defer t

 :custom
 (completion-in-region-function #'consult-completion-in-region))

(use-package transient
 :ensure t
 :defer t

 :custom
 (transient-default-level 7))

(use-package multiple-cursors
 :ensure t
 :defer t

 :bind
 ("C-c C-v"       . mc/edit-lines)
 ("C->"           . mc/mark-next-like-this)
 ("C-<"           . mc/mark-previous-like-this)
 ("C-S-<mouse-1>" . mc/add-cursor-on-click))

(use-package multiple-cursors-core
 :ensure nil
 :defer t

 :custom
 (mc/always-run-for-all t))

(use-package volatile-highlights
 :ensure t
 :defer t
 :diminish "Vh"

 :init
 (volatile-highlights-mode))

;;; Debuggers

(use-package gdb-mi
 :ensure nil
 :defer t

 :custom
 (gdb-many-windows t)
 (gdb-use-separate-io-buffer t)

 :config
 (advice-add 'gdb-setup-windows :after
  (lambda () (set-window-dedicated-p (selected-window) t))))

(use-package gud
 :ensure nil
 :defer t

 :hook
 (gud-mode-hook . gud-tooltip-mode)

 :custom
 (gdb-restore-window-configuration-after-quit t))

;;; YAML

(use-package yaml-mode
 :ensure t
 :defer t

 :bind
 (:map yaml-mode-map
  ("C-c p" . qol/generate-password))

 :hook
 (yaml-mode . flycheck-mode))

;;; LLVM

(use-package llvm-ts-mode
 :ensure t
 :defer t
 :mode "\\.ll\\'"

 :hook
 (llvm-mode . demangle-mode))

(use-package autodisass-llvm-bitcode
 :ensure t
 :defer
 :mode "\\.bc\\'")

(use-package demangle-mode
 :ensure t
 :defer t)

(use-package yaml-mode
 :ensure t
 :defer t

 :mode "\\.clang-format"
 :mode "\\.clang-tidy")

;;; Other

;; (im/after files
;;  (setq-default major-mode-remap-alist
;;   '((c-mode . c-ts-mode)
;;     (c++-mode . c++-ts-mode)
;;     (c-or-c++-mode . c-or-c++-ts-mode)
;;     (rust-mode . rust-ts-mode)
;;     (sh-mode . bash-ts-mode)
;;     (toml-mode . toml-ts-mode)
;;     (json-mode . json-ts-mode)
;;     (cmake-mode . cmake-ts-mode)
;;     (python-mode . python-ts-mode)
;;     (dockerfile-mode . dockerfile-ts-mode))))

(eval-when-compile
 (defconst emacs-dots-dir "/home/fred/Workspace/dots/emacs/")
 (push emacs-dots-dir load-path))
(require 'init-macros)

(im/after text-mode
 (im/hook text-mode-hook spell-fu-mode))

(defmacro im/setup-c-style-comments ()
 "Setup C-style /* ... */ comments."
 `(im/after newcomment
   (setq-local comment-style 'extra-line)))

(im/after css-mode
 (im/hookn css-mode-hook (im/setup-c-style-comments)))

(im/after cc-mode
 (im/key-disable "(" c-mode-base-map)
 (setq-default c-doc-comment-style
  '((java-mode . javadoc)
    (c-mode    . gtkdoc)
    (c++-mode  . doxygen)))
 (im/hook c-mode-common-hook lsp))

(im/after c-ts-mode
 (im/hook c-ts-base-mode-hook lsp))

(im/after cc-vars
 (setq-default c-mark-wrong-style-of-comment t)
 (setq-default c-default-style '((other . "user")))
 (setq-default c-basic-offset 2)
 (im/hookn c-mode-common-hook (im/setup-c-style-comments)))

(im/after python
 (im/hook python-mode-hook lsp)
 (im/hook python-ts-mode-hook lsp)
 (im/hookn python-ts-mode-hook
  (setq-local fill-column 79)))

(im/after jit-lock
 (setq-default jit-lock-stealth-time 1)
 (setq-default jit-lock-chunk-size 5000)
 (setq-default jit-lock-antiblink-grace 1))

(im/pkg projectile
 (im/after projectile
  (im/key-local "C-x p" projectile-command-map projectile-mode-map "projectile")
  (im/dim projectile-mode "Pr")
  (setq-default projectile-cache-file emacs-projectile-cache-file)
  (setq-default projectile-known-projects-file emacs-projectile-projects-file)
  (setq-default projectile-project-search-path '("~/Workspace"))
  (setq-default projectile-sort-order 'recently-active)
  (setq-default projectile-enable-caching nil)
  (setq-default projectile-require-project-root nil)))

(im/pkg counsel-projectile
 (im/after counsel-projectile
  (im/key-local "M-G" counsel-projectile-git-grep projectile-mode-map))
 (counsel-projectile-mode))

(im/pkg yasnippet-snippets
 (im/after yasnippet
  (yasnippet-snippets-initialize)))

(im/pkg yasnippet
 (im/after yasnippet
  (im/dim yas-minor-mode "Ys")
  (im/after company
   (im/hookn yas-minor-mode-hook (im/company-add-backend 'company-yasnippet)))))

(im/pkg hledger-mode
 (im/after hledger-mode
  (setq-default hledger-currency-string "EUR")
  (setq-default hledger-current-overlay t)
  (setq-default hledger-comments-column 1)
  (im/hookn hledger-mode-hook
   (setq-local tab-width 1)
   (im/after flycheck
    (eval-when-compile (require 'flycheck-hledger))))
  (im/hook hledger-mode-hook whitespace-mode)
  (im/hook hledger-mode-hook symbol-overlay-mode)
  (im/hook hledger-mode-hook flycheck-mode))
 (im/mode ".journal" hledger-mode)
 (im/mode ".ledger"  hledger-mode))

(im/pkg flycheck-hledger)

(im/pkg flycheck
 (im/after flycheck
  (im/autoload flycheck-next-error "flycheck")
  (im/autoload flycheck-previous-error "flycheck")
  (im/key-local "M-n" flycheck-next-error     flycheck-mode-map "flycheck")
  (im/key-local "M-p" flycheck-previous-error flycheck-mode-map "flycheck")
  (setq-default flycheck-checker-error-threshold nil)
  (setq-default flycheck-mode-line-prefix "Fc")
  (setq-default flycheck-check-syntax-automatically
   '(idle-change new-line mode-enabled idle-buffer-switch))
  (setq-default flycheck-idle-change-delay 0.25)
  (setq-default flycheck-idle-buffer-switch-delay 0.25)
  (im/hook flycheck-mode-hook flycheck-posframe-mode)))

(im/pkg flycheck-posframe
 (im/after flycheck-posframe
  ;; (flycheck-posframe-configure-pretty-defaults)
  (setq-default flycheck-posframe-position 'window-bottom-left-corner)
  (setq-default flycheck-posframe-border-width 1)
  (setq-default flycheck-posframe-prefix
   (concat " " (char-to-string 8618) " Info: "))
  (setq-default flycheck-posframe-warnings-prefix
   (concat " " (char-to-string 9888) " Warning: "))
  (setq-default flycheck-posframe-error-prefix
   (concat " " (char-to-string 10540) " Error: "))
  (im/after company
   (im/hook flycheck-posframe-inhibit-functions company--active-p "company")
   (im/hook flycheck-posframe-inhibit-functions
    (lambda (&rest _) (bound-and-true-p company-backend))))))

(im/pkg consult-flycheck
 (im/after flycheck
  (im/key-local "C-c ! a" consult-flycheck flycheck-mode-map)))

(im/pkg company
 (im/after company
  (im/dim company-mode "Co")
  (setq-default company-backends '((company-capf)))
  (setq-default company-idle-delay 0.5)
  (setq-default company-keywords-ignore-case t)
  (setq-default company-minimum-prefix-length 2)
  (setq-default company-selection-wrap-around t)
  (setq-default company-tooltip-align-annotations t)
  (im/key-local "<tab>" company-indent-or-complete-common company-mode-map "company")))

(defun im/company-add-backend (backend)
 "Add BACKEND to local copy of `company-backends'."
 (eval-when-compile (defvar company-backends))
 (im/autoload qol/append "qol")
 (qol/append (car company-backends) backend))

(im/pkg company-posframe
 (im/after company-posframe
  (im/dim company-posframe-mode)
  (setq-default company-posframe-quickhelp-x-offset 2))
 (im/after company
  (im/hook company-mode-hook company-posframe-mode "company-posframe")))

(im/pkg company-prescient
 (im/after company
  (im/hook company-mode-hook company-prescient-mode)))

(im/pkg tree-sitter-langs
 (im/after tree-sitter-mode
  (im/hook tree-sitter-mode-hook tree-sitter-langs-install-grammars)))

(im/pkg tree-sitter
 (im/after tree-sitter
  (im/dim tree-sitter-mode "Ts")
  (im/hook tree-sitter-mode-hook tree-sitter-hl-mode)))

;; (im/pkg scopeline
;;  (im/after tree-sitter
;;   (im/hook tree-sitter-mode-hook scopeline-mode))
;;  (im/after scopeline
;;   (im/dim scopeline-mode "Sl")
;;   (setq-default scopeline-min-lines 10)))

(im/after prog-mode
 (im/hook prog-mode-hook diff-hl-mode)
 (im/hook prog-mode-hook eldoc-mode)
 (im/hook prog-mode-hook show-paren-mode)
 (im/hook prog-mode-hook flyspell-prog-mode)
 (im/hook prog-mode-hook flycheck-mode)
 (im/hook prog-mode-hook yas-minor-mode)
 (im/hook prog-mode-hook company-mode)
 (im/hook prog-mode-hook electric-pair-mode)
 (im/hook prog-mode-hook electric-layout-mode)
 (im/hook prog-mode-hook display-line-numbers-mode)
 (im/hook prog-mode-hook hl-line-mode)
 (im/hook prog-mode-hook bug-reference-prog-mode))

(im/after conf-mode
 (im/hook conf-desktop-mode-hook diff-hl-mode)
 (im/hook conf-desktop-mode-hook show-paren-mode)
 (im/hook conf-desktop-mode-hook flyspell-prog-mode)
 (im/hook conf-desktop-mode-hook electric-pair-mode)
 (im/hook conf-desktop-mode-hook electric-layout-mode)
 (im/hook conf-desktop-mode-hook display-line-numbers-mode)
 (im/hook conf-desktop-mode-hook hl-line-mode))

(im/pkg meson-mode
 (im/after meson-mode
  (im/after company
   (im/hookn meson-mode-hook
    (im/company-add-backend 'company-dabbrev-code)))
  (im/hook meson-mode-hook symbol-overlay-mode)
  (im/hook meson-mode-hook company-mode)))

(im/pkg rust-mode
 (im/after rust-mode
  (im/key-local "<f5>" rust-dbg-wrap-or-unwrap            rust-mode-map "rust-utils")
  (im/key-local "<f6>" lsp-rust-analyzer-expand-macro     rust-mode-map "lsp-rust")
  (im/key-local "<f7>" lsp-rust-analyzer-join-lines       rust-mode-map "lsp-rust")
  (im/key-local "<f8>" lsp-rust-analyzer-inlay-hints-mode rust-mode-map "lsp-rust")
  (setq-default rust-indent-offset 2)
  (setq-default rust-load-optional-libraries nil)
  (setq-default rust-format-on-save t)
  (im/hookn rust-mode-hook (electric-quote-local-mode -1))
  (im/hook rust-mode-hook subword-mode)
  (im/hook rust-mode-hook lsp)))

(im/after rust-ts-mode
 (im/key-local "<f5>" rust-dbg-wrap-or-unwrap            rust-ts-mode-map "rust-utils")
 (im/key-local "<f6>" lsp-rust-analyzer-expand-macro     rust-ts-mode-map "lsp-rust")
 (im/key-local "<f7>" lsp-rust-analyzer-join-lines       rust-ts-mode-map "lsp-rust")
 (im/key-local "<f8>" lsp-rust-analyzer-inlay-hints-mode rust-ts-mode-map "lsp-rust")
 (setq-default rust-ts-mode-indent-offset 2)
 (im/hookn rust-ts-mode-hook (electric-quote-local-mode -1))
 (im/hook rust-ts-mode-hook subword-mode)
 (im/hook rust-ts-mode-hook lsp))

(im/pkg lsp-mode
 (im/after lsp-mode
  (im/dim lsp-mode "Ls")
  (im/key-local "C-c f" lsp-format-buffer           lsp-mode-map "lsp-mode")
  (im/key-local "C-c g" lsp-format-region           lsp-mode-map "lsp-mode")
  (im/key-local "C-c r" lsp-rename                  lsp-mode-map "lsp-mode")
  (im/key-local "C-c h" lsp-describe-thing-at-point lsp-mode-map "lsp-mode")
  (im/key-local "C-="   lsp-extend-selection        lsp-mode-map "lsp-mode")
  (im/key-local "M-RET" lsp-execute-code-action     lsp-mode-map "lsp-mode")
  (setq-default lsp-progress-prefix "  Progress: ")
  (setq-default lsp-completion-show-detail t)
  (setq-default lsp-completion-show-kind t)
  (setq-default lsp-completion-provider :none)
  (setq-default lsp-headerline-breadcrumb-enable t)
  (setq-default lsp-restart 'auto-restart)
  (setq-default lsp-enable-snippet t)
  (setq-default lsp-keymap-prefix "C-c")
  (setq-default lsp-idle-delay 0.1)
  (setq-default lsp-file-watch-threshold nil)
  (setq-default lsp-enable-semantic-highlighting t)
  (setq-default lsp-enable-indentation t)
  (setq-default lsp-enable-on-type-formatting nil)
  (setq-default lsp-before-save-edits nil)
  (setq-default lsp-auto-configure t)
  (setq-default lsp-signature-auto-activate t)
  (setq-default lsp-signature-render-documentation nil)
  (setq-default lsp-eldoc-enable-hover t)
  (setq-default lsp-eldoc-render-all nil)
  (setq-default lsp-modeline-code-actions-enable nil)
  (setq-default lsp-modeline-diagnostics-enable t)
  (setq-default lsp-log-io nil)
  (setq-default lsp-keep-workspace-alive nil)
  (setq-default lsp-enable-imenu nil)
  (im/after which-key
   (im/hook lsp-mode-hook lsp-enable-which-key-integration "lsp-mode")))
 (im/after lsp-lens
  (im/dim lsp-lens-mode)
  (setq-default lsp-lens-mode nil)
  (setq-default lsp-lens-enable nil))
 (im/after lsp-headerline
  (setq-default lsp-headerline-breadcrumb-icons-enable nil))
 (im/after lsp-semantic-tokens
  (setq-default lsp-semantic-tokens-apply-modifiers t))
 (im/after lsp-rust
  ;; (setq-default lsp-rust-analyzer-max-inlay-hint-length 50)
  ;; (setq-default lsp-rust-unstable-features t)
  (setq-default lsp-rust-analyzer-checkonsave-features "all")
  (setq-default lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (setq-default lsp-rust-analyzer-proc-macro-enable t)
  (setq-default lsp-rust-racer-completion nil)
  (setq-default lsp-rust-build-bin t)
  (setq-default lsp-rust-build-lib t)
  (setq-default lsp-rust-clippy-preference "on")
  (setq-default lsp-rust-analyzer-server-display-inlay-hints t)
  (setq-default lsp-rust-analyzer-display-chaining-hints t)
  (setq-default lsp-rust-analyzer-display-parameter-hints t)
  (setq-default lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-enable "always")
  (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (setq-default lsp-rust-analyzer-binding-mode-hints t)
  (setq-default lsp-rust-analyzer-display-reborrow-hints "mutable")
  (setq-default lsp-rust-all-features t)
  (setq-default lsp-rust-all-targets t)
  (setq-default lsp-rust-full-docs t)
  (setq-default lsp-rust-analyzer-cargo-watch-command "clippy"))
 (im/after lsp-clangd
  (setq-default lsp-clients-clangd-args
   '("--header-insertion-decorators"
     "--all-scopes-completion"
     "--clang-tidy"
     "--completion-style=detailed"
     "--header-insertion=iwyu"
     ;; Breaks clangd-14
     ; "--header-insertion-decorators"
     "--inlay-hints"
     "-j=8"
     "--malloc-trim"
     "--pch-storage=memory"
     "--background-index"
     "--function-arg-placeholders"
     "--limit-references=0"
     "--limit-results=0"))
  (im/after cc-mode
   (im/autoload lsp-clangd-find-other-file "lsp-clangd")
   (im/key-local "<f2>" lsp-clangd-find-other-file c-mode-base-map))
  (im/after c-ts-mode
   (im/autoload lsp-clangd-find-other-file "lsp-clangd")
   (im/key-local "<f2>" lsp-clangd-find-other-file c-ts-base-mode-map))))

(im/pkg lsp-ivy
 (im/after lsp-mode
  (im/key-local "C-c x" lsp-ivy-workspace-symbol lsp-mode-map)))

(im/pkg treemacs
 (im/key "<f9>" treemacs-select-window)
 (im/after treemacs-customization
  (setq-default treemacs-width 40)
  (setq-default treemacs-indentation 1))
 (im/after treemacs
  (setq-default treemacs-select-when-already-in-treemacs 'move-back))
  ;; (setq-default treemacs-indent-guide-mode t))
 (im/after treemacs-interface
  (im/key "<f12>" treemacs-delete-other-windows "treemacs-interface"))
 ;; (im/after treemacs-header-line
 ;;  (setq-default treemacs-indicate-top-scroll-mode t))
 (im/after treemacs-mode
  (im/hook treemacs-mode-hook treemacs-tag-follow-mode "treemacs-tag-follow-mode")
  (im/hook treemacs-mode-hook treemacs-fringe-indicator-mode "treemacs-fringe-indicator")
  (im/hook treemacs-mode-hook treemacs-filewatch-mode "treemacs-filewatch-mode")
  ;; (im/hook treemacs-mode-hook treemacs-indicate-top-scroll-mode "treemacs-header-line")
  ;; (im/autoload treemacs-indent-guide-mode "treemacs-visuals")
  ;; (im/hookn treemacs-mode-hook (treemacs-indent-guide-mode))
  (im/autoload treemacs-git-mode "treemacs-async")
  (im/hookn treemacs-mode-hook (treemacs-git-mode 'deferred))
  (im/hook treemacs-mode-hook
   treemacs-git-commit-diff-mode
   "treemacs-git-commit-diff-mode")))

(im/pkg lsp-treemacs
 (im/after lsp-mode
  (im/key-local "C-c e" lsp-treemacs-errors-list    lsp-mode-map)
  (im/key-local "C-c s" lsp-treemacs-symbols        lsp-mode-map)
  (im/key-local "C-c c" lsp-treemacs-call-hierarchy lsp-mode-map)
  (im/key-local "C-c t" lsp-treemacs-type-hierarchy lsp-mode-map)
  (im/hook lsp-mode-hook lsp-treemacs-sync-mode)))

(im/pkg treemacs-projectile)

(im/pkg lsp-ui
 (im/after lsp-ui-doc
  (setq-default lsp-ui-doc-enable t)
  (setq-default lsp-ui-doc-show-with-cursor nil)
  (setq-default lsp-ui-doc-show-with-mouse t)
  (setq-default lsp-ui-doc-alignment 'frame)
  (setq-default lsp-ui-doc-header t)
  (setq-default lsp-ui-doc-include-signature t)
  (setq-default lsp-ui-doc-max-height 30)
  (setq-default lsp-ui-doc-use-webkit t))
 (im/after lsp-ui-peek
  (setq-default lsp-ui-peek-list-width 40)
  (setq-default lsp-ui-peek-always-show t))
 (im/after lsp-ui-sideline
  (setq-default lsp-ui-sideline-enable nil))
 (im/after lsp-ui
  (im/key-local "M-."   lsp-ui-peek-find-definitions    lsp-ui-mode-map "lsp-ui-peek")
  (im/key-local "M-?"   lsp-ui-peek-find-references     lsp-ui-mode-map "lsp-ui-peek")
  (im/key-local "M-I"   lsp-ui-peek-find-implementation lsp-ui-mode-map "lsp-ui-peek")
  (im/key-local "C-c d" lsp-ui-doc-show                 lsp-ui-mode-map "lsp-ui-doc")
  (im/key-local "C-c l" lsp-ui-flycheck-list            lsp-ui-mode-map "lsp-ui-flycheck")))

(im/pkg web-mode
 (im/mode ".html" web-mode)
 (im/mode ".css" web-mode)
 (im/mode ".js" web-mode)
 (im/after web-mode
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-enable-current-column-highlight t)
  (setq-default web-mode-enable-current-element-highlight t)
  (setq-default web-mode-auto-close-style 3)
  (setq-default web-mode-enable-auto-expanding t)
  (im/hook web-mode-hook lsp)
  (im/hookn web-mode-hook (setq-local tab-width 2))))

(im/pkg company-web
 (im/after web-mode
  (im/after company
   (im/hookn web-mode-hook
    (im/company-add-backend 'company-css)
    (im/company-add-backend 'company-web-html)))))

(im/pkg emmet-mode
 (setq-default emmet-indentation 2)
 (im/after web-mode
  (im/hook web-mode-hook emmet-mode)))

(im/pkg dockerfile-mode)
(im/pkg pkgbuild-mode)

(im/pkg vterm
 (im/after vterm
  (setq-default vterm-max-scrollback 100000)))

(im/pkg sideline
 (im/after sideline
  (im/dim sideline-mode "Si")
  (setq-default sideline-delay 0.1)))

(im/pkg sideline-blame
 (im/after sideline
  (setq-default sideline-backends-right '(sideline-blame))
  (setq-default sideline-blame-commit-format "- %s")))

(im/pkg buffer-move
 (im/key "C-x m" buf-move))

;; (im/pkg popper
;;  (im/after popper
;;   (setq-default popper-reference-buffers '("\\*deadgrep.*$" "\\*Occur\\*$"))
;;   (im/autoload popper-group-by-projectile "popper")
;;   (setq-default popper-group-function #'popper-group-by-projectile)
;;   (im/key "C-`" popper-cycle "popper")
;;   (im/key-local "C-~" popper-toggle-type popper-mode-map "popper"))
;;  (popper-mode)
;;  (popper-echo-mode))

(im/pkg surround
 (require 'surround)
 (im/key "M-'" surround-mark-inner)
 (im/key "M-\"" surround-insert))

;; Print startup stats.
(message "Startup in %s (%d GC runs)" (emacs-init-time) gcs-done)

(provide 'init)
;;; init.el ends here

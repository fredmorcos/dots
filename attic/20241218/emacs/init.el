;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Quality of Life

(use-package qol
 :ensure nil
 :defer t
 :load-path "/home/fred/Workspace/dots/emacs/"
 :commands qol/select-package qol/append qol/remove)

;;; No Littering

(use-package no-littering
 :ensure t
 :demand
 :commands
 no-littering-theme-backups
 no-littering-expand-etc-file-name
 no-littering-expand-var-file-name

 :init
 (qol/select-package 'no-littering)

 :config
 (no-littering-theme-backups))

;;; Diminish

(use-package diminish
 :ensure t
 :defer t

 :init
 (qol/select-package 'diminish))

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

(use-package qol
 :ensure nil
 :defer t
 :load-path "/home/fred/Workspace/dots/emacs/"
 :commands qol/generate-password

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
 (qol/select-package 'move-text)
 (move-text-default-bindings))

(use-package mwim
 :ensure t
 :defer t

 :init
 (qol/select-package 'mwim)

 :bind
 ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line-or-comment)
 ([remap move-end-of-line] . mwim-end-of-code-or-line))

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

(use-package unfill
 :ensure t
 :defer t

 :init
 (qol/select-package 'unfill)

 :bind
 ([remap fill-paragraph] . unfill-toggle))

(use-package register
 :ensure nil
 :defer t
 :after counsel

 :bind
 ([remap jump-to-register] . counsel-register))

(use-package indent
 :ensure nil
 :defer t

 :custom
 (tab-always-indent 'complete)
 (tab-first-completion 'word))

(use-package simple
 :ensure nil
 :defer t
 :custom
 (indent-tabs-mode nil))

(use-package files
 :ensure nil
 :defer t

 :custom
 (mode-require-final-newline 'visit-save)
 (require-final-newline 'visit-save)
 ;; File contents.
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

 :init
 (qol/select-package 'expand-region)

 :bind
 ("C-=" . er/expand-region))

(use-package jinx
 :ensure t
 :defer t
 :after text-mode

 :init
 (qol/select-package 'jinx)

 :hook text-mode-hook)

(use-package surround
 :ensure t
 :defer t

 :init
 (qol/select-package 'surround)

 :bind
 ("M-'" . surround-mark-inner)
 ("M-\"" . surround-insert))

;;; Window Management

(use-package windmove
 :ensure nil
 :defer t

 :init
 (windmove-default-keybindings)
 (windmove-delete-default-keybindings))

(use-package buffer-move
 :ensure t
 :defer t

 :init
 (qol/select-package 'buffer-move)

 :bind
 ("C-x m" . buf-move))

;;; Auto-save & backups

(use-package files
 :ensure nil
 :defer t

 :custom
 (auto-save-default t)
 (backup-inhibited nil)
 (make-backup-files t)
 ;; Prefer the newest version of a file.
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

;;; Warnings

(use-package warnings
 :ensure nil
 :defer t

 :config
 ;; Suppress certain annoying warnings.
 (add-to-list 'warning-suppress-types 'defvaralias)

 :custom
 ;; Stop the warnings buffer from popping up, but still log warnings.
 (warning-minimum-level :emergency))

;;; Help Windows

(use-package help
 :ensure nil
 :defer t

 :custom
 (help-window-select t))

(use-package help-mode
 :ensure nil
 :defer t

 :config
 ;; Recenter after pressing on links to emacs source code.
 (defadvice help-button-action
  (after recenter-after-help-button-action activate)
  (recenter))
 (defadvice help-function-def--button-function
  (after recenter-after-help-function-def--button-function activate)
  (recenter)))

(use-package info
 :ensure nil
 :defer t

 :bind
 (:map Info-mode-map ("C-p" . casual-info-tmenu)))

;;; UX

(use-package emacs
 :ensure nil
 :defer t

 :custom
 (delete-by-moving-to-trash t))

(use-package files
 :ensure nil
 :defer t

 :config
 (defadvice find-file
  (after recenter-after-find-file activate)
  (recenter))

 :custom
 (confirm-kill-processes nil))

(use-package mouse
 :ensure nil
 :defer t

 :custom
 (mouse-yank-at-point t)
 (mouse-1-click-follows-link 'double))

(use-package simple
 :ensure nil
 :defer t

 :config
 ;; Recenter after using goto-line.
 (defadvice goto-line
  (after recenter-after-goto-line activate)
  (recenter))

 :custom
 ;; Hide commands in M-x that do not work in the current mode
 (read-extended-command-predicate #'command-completion-default-include-p)
 (undo-limit (* 1024 1024))
 (suggest-key-bindings 10)
 (save-interprogram-paste-before-kill t)
 (backward-delete-char-untabify-method 'hungry)
 ;; Recenter after jump to next error.
 (next-error-recenter '(4))
 (next-error-message-highlight t)
 (completion-auto-select 'second-tab)

 :preface
 (defun init/keyboard-quit-dwim ()
  "Reasonable keyboard-quit behavior.
     - When a minibuffer is open but not focused, close it.
     - When a completions buffer is selected, close it.
     - Otherwise (e.g. region is active) do `keyboard-quit`."
  (interactive)
  (cond
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   (t
    (keyboard-quit))))

 :bind
 ([remap keyboard-quit] . init/keyboard-quit-dwim))

(use-package simple
 :ensure nil
 :defer t
 :after eldoc

 :config
 (eldoc-add-command #'init/keyboard-quit-dwim))

(use-package simple
 :ensure nil
 :defer t
 :after files

 :hook
 (before-save-hook . delete-trailing-whitespace))

(use-package nerd-icons
 :ensure t
 :defer t

 :init
 (qol/select-package 'nerd-icons))

(use-package casual-suite
 :ensure t
 :defer t

 :init
 (qol/select-package 'casual-suite))

(use-package isearch
 :ensure nil
 :defer t

 :bind
 (:map isearch-mode-map ("C-p" . casual-isearch-tmenu)))

(use-package ibuffer
 :ensure nil
 :defer t

 :bind
 (:map ibuffer-mode-map
  (("C-p" . casual-ibuffer-tmenu)
   ("F"   . casual-ibuffer-filter-tmenu)
   ("s"   . casual-ibuffer-sortby-tmenu))))

(use-package re-builder
 :ensure nil
 :defer t

 :bind
 (:map reb-mode-map ("C-p" . casual-re-builder-tmenu))
 (:map reb-lisp-mode-map ("C-p" . casual-re-builder-tmenu)))

(use-package bookmark
 :ensure nil
 :defer t

 :bind
 (:map bookmark-bmenu-mode-map ("C-p" . casual-bookmarks-tmenu)))

(use-package symbol-overlay
 :ensure t
 :defer t

 :init
 (qol/select-package 'symbol-overlay)

 :bind
 (:map symbol-overlay-map ("C-p" . casual-symbol-overlay-tmenu)))

(use-package emacs
 :ensure nil
 :defer t

 :bind
 ("C-p" . casual-editkit-main-tmenu))

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
 (("C-<f11>" . init/scroll-other-window)
  ("C-<f12>" . init/scroll-other-window-down)))

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

(use-package company
 :ensure t
 :defer t
 :diminish "Co"
 :commands company--active-p

 :init
 (qol/select-package 'company)

 :preface
 (defun init/setup-company (&optional main-backends secondary-backends)
  "Setup company completion system with common backends."
  (setq-local company-backends
   `((,@main-backends
      company-capf
      ;; company-dabbrev-code
      ;; company-keywords
      company-yasnippet
      ;; company-files
      ,@secondary-backends
      :separate))))

 ;; :bind
 ;; (:map company-mode-map ("<tab>" . company-indent-or-complete-common))

 :custom
 ;; (company-idle-delay 0.7)
 ;; (company-minimum-prefix-length 2)
 (company-keywords-ignore-case t)
 (company-selection-wrap-around t)
 (company-tooltip-align-annotations t)
 (company-tooltip-minimum-width 40)
 (company-tooltip-width-grow-only t))

(use-package company-posframe
 :ensure t
 :defer t
 :diminish
 :after company
 :hook company-mode-hook

 :init
 (qol/select-package 'company-posframe)

 :config
 (qol/append company-posframe-show-params :border-width 1)
 (qol/append company-posframe-quickhelp-show-params :border-width 1)

 :custom
 (company-posframe-quickhelp-x-offset 2))

(use-package company-prescient
 :ensure t
 :defer t
 :after company
 :hook company-mode-hook

 :init
 (qol/select-package 'company-prescient))

;;; Syntax Checking

(use-package flycheck
 :ensure t
 :defer t

 :init
 (qol/select-package 'flycheck)

 :commands
 (flycheck-next-error
  flycheck-previous-error
  flycheck-add-next-checker)

 :bind
 (:map flycheck-mode-map
  ("M-n" . flycheck-next-error)
  ("M-p" . flycheck-previous-error))

 :custom
 (flycheck-checker-error-threshold nil)
 (flycheck-mode-line-prefix "Fc")
 (flycheck-check-syntax-automatically
  '(idle-change new-line mode-enabled idle-buffer-switch))
 ;; (flycheck-idle-change-delay 0.5)
 ;; (flycheck-idle-buffer-switch-delay 0.2)
 ;; (flycheck-display-errors-delay 0.2)

 :config
 (defadvice flycheck-next-error
  (after recenter-after-flycheck-next-error activate)
  (recenter))
 (defadvice flycheck-previous-error
  (after recenter-after-flycheck-previous-error activate)
  (recenter))
 (defadvice flycheck-error-list-goto-error
  (after recenter-after-flycheck-error-list-goto-error activate)
  (recenter)))

(use-package flycheck-posframe
 :ensure t
 :defer t

 :init
 (qol/select-package 'flycheck-posframe)

 :custom
 (flycheck-posframe-prefix (concat " " (char-to-string 8618)  " Info: "))
 (flycheck-posframe-warning-prefix (concat " " (char-to-string 9888)  " Warning: "))
 (flycheck-posframe-error-prefix (concat " " (char-to-string 10540) " Error: "))
 (flycheck-posframe-position 'window-bottom-left-corner)
 (flycheck-posframe-border-width 1))

(use-package flycheck-posframe
 :ensure t
 :defer t
 :after flycheck
 :hook flycheck-mode-hook

 :init
 (qol/select-package 'flycheck-posframe))

(use-package company
 :ensure t
 :defer t
 :after flycheck-posframe

 :init
 (qol/select-package 'company)

 :preface
 (defun init/company-is-active (&rest _)
  (or (company--active-p) (bound-and-true-p company-backend)))

 :hook
 (flycheck-posframe-inhibit-functions . init/company-is-active))

(use-package consult-flycheck
 :ensure t
 :defer t
 :after flycheck

 :init
 (qol/select-package 'consult-flycheck)

 :bind
 (:map flycheck-mode-map
  ("C-c ! a" . consult-flycheck)))

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
                    ,(no-littering-expand-etc-file-name "")
                    ,@native-comp-eln-load-path
                    "/usr/share/emacs"))

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
 ;; Skip *SPECIALS* when switching buffers.
 (switch-to-prev-buffer-skip-regexp '("\\`\\*.*\\'"))

 :preface
 (defun init/disable-popup (regexp)
  "Stop buffers that match REGEXP from popping up."
  (add-to-list 'display-buffer-alist
   `(,regexp (display-buffer-no-window) (allow-no-window . t))))

 :config
 (init/disable-popup "\\`\\*Compile-Log\\*.*\\'")
 (init/disable-popup "\\`\\*Native-compile-Log\\*.*\\'")
 (init/disable-popup "\\`\\*Async-native-compile-log\\*.*\\'")
 (init/disable-popup "\\`\\*Warnings\\*.*\\'")

 :bind
 (("<f12>"       . delete-other-windows)
  ("M-S-<right>" . next-buffer)
  ("M-S-<left>"  . previous-buffer)))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 (resize-mini-windows t))

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

(use-package display-fill-column-indicator
 :ensure nil
 :defer t
 :after prog-mode
 :hook prog-mode-hook)

(use-package goto-addr
 :ensure nil
 :defer t
 :after prog-mode
 :hook (prog-mode-hook . goto-address-prog-mode))

(use-package diff-hl
 :ensure t
 :defer t
 :after prog-mode
 :hook prog-mode-hook

 :init
 (qol/select-package 'diff-hl))

(use-package eldoc
 :ensure nil
 :defer t
 :after prog-mode
 :hook prog-mode-hook)

(use-package paren
 :ensure nil
 :defer t
 :after prog-mode
 :hook (prog-mode-hook . show-paren-mode))

(use-package flycheck
 :ensure t
 :defer t
 :after prog-mode
 :hook prog-mode-hook

 :init
 (qol/select-package 'flycheck))

(use-package yasnippet
 :ensure t
 :defer t
 :after prog-mode
 :hook (prog-mode-hook . yas-minor-mode-on)

 :init
 (qol/select-package 'yasnippet))

(use-package company
 :ensure t
 :defer t
 :after prog-mode
 :hook
 (prog-mode-hook . init/setup-company)

 :init
 (qol/select-package 'company))

(use-package elec-pair
 :ensure nil
 :defer t
 :after prog-mode
 :hook (prog-mode-hook . electric-pair-local-mode))

(use-package electric
 :ensure nil
 :defer t
 :after prog-mode
 :hook (prog-mode-hook . electric-layout-local-mode))

(use-package display-line-numbers
 :ensure nil
 :defer t
 :after prog-mode
 :hook prog-mode-hook)

(use-package hl-line
 :ensure nil
 :defer t
 :after prog-mode
 :hook prog-mode-hook)

(use-package bug-reference
 :ensure nil
 :defer t
 :after prog-mode
 :hook (prog-mode-hook . bug-reference-prog-mode))

(use-package jinx
 :ensure t
 :defer t
 :after prog-mode
 :hook prog-mode-hook

 :init
 (qol/select-package 'jinx))

(use-package whitespace
 :ensure nil
 :defer t
 :after prog-mode
 :hook prog-mode-hook)

(use-package deadgrep
 :ensure t
 :defer t
 :after prog-mode

 :init
 (qol/select-package 'deadgrep)

 :bind
 (:map prog-mode-map ("M-F" . deadgrep)))

(use-package wgrep
 :ensure t
 :defer t

 :init
 (qol/select-package 'wgrep))

(use-package wgrep-deadgrep
 :ensure t
 :defer t
 :after deadgrep

 :init
 (qol/select-package 'wgrep-deadgrep))

(use-package sideline
 :ensure t
 :defer t
 :diminish "Si"

 :init
 (qol/select-package 'sideline))

(use-package sideline-blame
 :ensure t
 :defer t
 :after sideline

 :init
 (qol/select-package 'sideline-blame)

 :custom
 (sideline-backends-right '(sideline-blame))
 (sideline-blame-commit-format "- %s"))

(use-package xref
 :ensure nil
 :defer t

 :commands
 xref-push-marker-stack

 :hook
 (xref-after-return-hook . recenter)
 (xref-after-jump-hook . recenter))

(use-package ivy-xref
 :ensure t
 :demand

 :init
 (qol/select-package 'ivy-xref)

 :custom
 (xref-show-xrefs-function 'ivy-xref-show-xrefs))

(use-package editorconfig
 :ensure t
 :defer t
 :diminish "Ec"

 :init
 (qol/select-package 'editorconfig))

;;; Configuration Files

(use-package diff-hl
 :ensure t
 :defer t
 :after conf-mode
 :hook (conf-mode-hook conf-desktop-mode-hook))

(use-package paren
 :ensure nil
 :defer t
 :after conf-mode
 :hook ((conf-mode-hook conf-desktop-mode-hook) . show-paren-mode))

(use-package elec-pair
 :ensure nil
 :defer t
 :after conf-mode
 :hook ((conf-mode-hook conf-desktop-mode-hook) . electric-pair-local-mode))

(use-package electric
 :ensure nil
 :defer t
 :after conf-mode
 :hook ((conf-mode-hook conf-desktop-mode-hook) . electric-layout-local-mode))

(use-package display-line-numbers
 :ensure nil
 :defer t
 :after conf-mode
 :hook (conf-mode-hook conf-desktop-mode-hook))

(use-package hl-line
 :ensure nil
 :defer t
 :after conf-mode
 :hook (conf-mode-hook conf-desktop-mode-hook))

(use-package jinx
 :ensure t
 :defer t
 :after conf-mode
 :hook (conf-mode-hook conf-desktop-mode-hook))

(use-package whitespace
 :ensure nil
 :defer t
 :after conf-mode
 :hook (conf-mode-hook conf-desktop-mode-hook))

;;; Meson

(use-package meson-mode
 :ensure t
 :defer t)

(use-package symbol-overlay
 :ensure t
 :defer t
 :after meson-mode
 :hook meson-mode-hook)

(use-package company
 :ensure t
 :defer t
 :after meson-mode
 :hook
 (meson-mode-hook . init/setup-company))

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
 ;; (ediff-split-window-function #'split-window-right)
 (ediff-split-window-function #'split-window-horizontally)
 (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package blamer
 :ensure t
 :defer t

 :custom
 ;; (blamer-idle-time 0)
 (blamer-commit-formatter ": %s")
 (blamer-datetime-formatter "%s")
 (blamer-max-commit-message-length 60)

 :bind
 (:map prog-mode-map
  ("C-c b" . blamer-mode)))

(use-package goto-addr
 :ensure nil
 :defer t
 :after magit-process
 :hook (magit-process-mode-hook . goto-address-mode))

(use-package magit
 :ensure t
 :defer t

 :bind
 ("C-x g" . magit-status)

 :custom
 (magit-log-section-commit-count 20)
 ;; (magit-auto-revert-tracked-only nil)
 ;; (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
 (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
 (magit-bury-buffer-function #'magit-restore-window-configuration)
 (magit-repository-directories '(("~/Workspace" . 3))))

(use-package magit
 :ensure t
 :defer t
 :after files
 :hook (after-save-hook . magit-after-save-refresh-status))

(use-package magit-diff
 :ensure magit
 :defer t

 :custom
 (magit-revision-show-gravatars t)
 (magit-revision-fill-summary-line fill-column)

 :config
 (defadvice magit-diff-visit-file
  (after recenter-after-magit-diff-visit-file activate)
  (recenter)))

(use-package diff-hl
 :ensure t
 :defer t

 :custom
 ;; (diff-hl-flydiff-delay 0.1)
 (diff-hl-draw-borders nil)
 (diff-hl-update-async t))

(use-package diff-hl
 :ensure t
 :defer t
 :after magit-mode
 :hook
 (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(use-package diff-hl-flydiff
 :ensure diff-hl
 :defer t
 :after diff-hl
 :hook
 (diff-hl-mode-hook . diff-hl-flydiff-mode))

(use-package diff-hl-show-hunk
 :ensure diff-hl
 :defer t
 :after diff-hl
 :hook
 (diff-hl-mode-hook . diff-hl-show-hunk-mouse-mode))

(use-package diff-hl-dired
 :ensure diff-hl
 :defer t
 :after dired
 :hook
 (dired-mode-hook . diff-hl-dired-mode))

;;; General Features

(use-package symbol-overlay
 :ensure t
 :defer t
 :diminish "So"

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
 (show-paren-when-point-in-periphery t)
 (show-paren-when-point-inside-paren t)
 (show-paren-style 'mixed)
 (show-paren-highlight-openparen t)
 (show-paren-context-when-offscreen 'overlay))

(use-package autorevert
 :ensure nil
 :defer t
 :diminish "Ar"

 :custom
 (auto-revert-mode-text " Ar")
 (auto-revert-interval 1)
 (auto-revert-avoid-polling t)
 (buffer-auto-revert-by-notification t))

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
 (whitespace-line-column fill-column)
 (show-trailing-whitespace nil)
 (whitespace-action '(cleanup auto-cleanup))
 (whitespace-style
  '(face tabs lines-tail empty tab-mark indentation indentation::tab indentation::space
    space-after-tab space-after-tab::tab space-after-tab::space space-before-tab
    space-before-tab::tab space-before-tab::space whitespace-missing-newline-at-eof))

 :custom-face
 (whitespace-tab ((t (:foreground "lavender" :background "white smoke")))))

;;; Makefiles

(use-package whitespace
 :ensure t
 :defer t
 :after make-mode
 :hook makefile-mode-hook)

;;; Emacs Lisp

(use-package elisp-mode
 :ensure nil
 :defer t

 :custom
 (lisp-indent-offset 1)
 (lisp-indent-function #'common-lisp-indent-function)

 :preface
 (defun init/expand-current-macro ()
  "Expand the current macro expression."
  (interactive)
  (beginning-of-defun)
  (emacs-lisp-macroexpand))

 :bind
 (:map emacs-lisp-mode-map
  ("<f6>" . init/expand-current-macro)))

(use-package symbol-overlay
 :ensure t
 :defer t
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package whitespace
 :ensure t
 :defer t
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package highlight-defined
 :ensure t
 :defer t
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package highlight-quoted
 :ensure t
 :defer t
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package eros
 :ensure t
 :defer t
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package suggest
 :ensure t
 :defer t)

(use-package ipretty
 :ensure t
 :defer t
 :after elisp-mode
 :hook (emacs-lisp-mode-hook . (lambda () (ipretty-mode t))))

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
 (sh-mode-hook . init/make-file-executable)
 (bash-ts-mode-hook . init/make-file-executable))

(use-package lsp-mode
 :ensure t
 :defer t
 :after sh-script
 :hook (sh-mode-hook . lsp))

;;; Dired

(use-package dired-async
 :ensure nil
 :defer t
 :diminish "As")

(use-package dired
 :ensure nil
 :defer t

 :custom
 (dired-mouse-drag-files t)
 (dired-listing-switches "-l -h --group-directories-first")
 (dired-hide-details-hide-symlink-targets nil)
 (dired-recursive-copies 'always)
 (dired-recursive-deletes 'always)
 (dired-dwim-target t)

 :bind
 (:map dired-mode-map
  ("C-p" . casual-dired-tmenu))

 :preface
 (defun init/dired-setup ()
  "Setup dired requires."
  (require 'dired-x)
  (require 'wdired)
  (require 'image-dired))

 :hook
 (dired-mode-hook . init/dired-setup)
 (dired-mode-hook . dired-hide-details-mode))

(use-package hl-line
 :ensure t
 :defer t
 :after dired
 :hook dired-mode-hook)

(use-package mouse
 :ensure nil
 :defer t
 :after dired
 :hook (dired-mode-hook . context-menu-mode))

(use-package dired-async
 :ensure nil
 :defer t
 :after dired
 :hook dired-mode-hook)

(use-package autorevert
 :ensure nil
 :defer t
 :after dired
 :hook (dired-mode-hook . auto-revert-mode))

(use-package nerd-icons-dired
 :ensure t
 :defer t

 :hook (dired-mode-hook . nerd-icons-dired-mode))

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
 :defer t)

(use-package eldoc-cmake
 :ensure t
 :defer t
 :after cmake-mode
 :hook (cmake-mode-hook . eldoc-cmake-enable))

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
 :defer t)

(use-package eldoc-toml
 :ensure t
 :defer t
 :diminish
 :after (eldoc toml-mode)
 :hook toml-mode-hook)

;;; JSON

(use-package json-mode
 :ensure t
 :defer t)

(use-package indent-guide
 :ensure t
 :defer t
 :after json-mode
 :hook json-mode-hook)

(use-package indent-guide
 :ensure t
 :defer t
 :after json-ts-mode
 :hook json-ts-mode-hook)

(use-package tree-sitter
 :ensure t
 :defer t
 :diminish "Ts"
 :after json-mode
 :hook json-mode-hook)

;;; Spell Checking

(use-package jinx
 :ensure t
 :defer t
 :diminish "Jx"

 :bind
 (:map jinx-mode-map
  ("M-$"   . jinx-correct)
  ("C-M-$" . jinx-languages)))

;;; Emacs Tools

(use-package which-key
 :ensure t
 :defer t
 :diminish

 :custom
 ;; (which-key-idle-delay 0.5)
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
 ;; (ivy-initial-inputs-alist nil)
 ;; (ivy-re-builders-alist '((t . ivy--regex-ignore-order) (t . ivy--regex-plus)))
 (ivy-wrap t)
 (ivy-use-selectable-prompt t)
 (ivy-use-virtual-buffers t)
 (ivy-count-format "(%d/%d) ")
 (ivy-virtual-abbreviate 'abbreviate)
 (ivy-extra-directories nil)

 :init
 (ivy-mode))

(use-package ivy-rich
 :ensure t
 :defer t
 :after counsel

 :custom
 (ivy-rich-path-style 'abbrev)

 :init
 (ivy-rich-mode))

(use-package nerd-icons-ivy-rich
 :ensure t
 :defer t

 :init
 (nerd-icons-ivy-rich-mode))

(use-package nerd-icons-completion
 :ensure t
 :defer t

 :init
 (nerd-icons-completion-mode))

(use-package counsel
 :ensure t
 :defer t
 :diminish

 :bind
 (:map counsel-mode-map
  ("M-Y" . counsel-yank-pop))

 :config
 ;; (put 'counsel-find-symbol 'no-counsel-M-x t)
 (defadvice counsel-register
  (after recenter-after-counsel-register activate)
  (recenter))
 (defadvice counsel-outline
  (after recenter-after-counsel-outline activate)
  (recenter))

 :init
 (counsel-mode))

(use-package marginalia
 :ensure t
 :defer t

 :bind
 (:map minibuffer-local-map
  ("M-A" . marginalia-cycle))
 (:map completion-list-mode-map
  ("M-A" . marginalia-cycle))

 :custom
 (marginalia-field-width 10)

 :hook
 (marginalia-mode-hook . nerd-icons-completion-marginalia-setup)

 :init
 (marginalia-mode))

(use-package hotfuzz
 :ensure t
 :defer t
 :after minibuffer

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

 :commands
 prescient-persist-mode

 :init
 (prescient-persist-mode))

(use-package prescient
 :ensure t
 :defer t
 :after minibuffer

 :init
 (push 'prescient completion-styles))

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
 (push 'orderless-prefixes orderless-matching-styles))

(use-package orderless
 :ensure t
 :defer t
 :after minibuffer

 :init
 (push 'orderless completion-styles))

(use-package minibuffer
 :ensure nil
 :defer t

 :config
 (qol/remove completion-styles 'emacs22)
 (qol/remove completion-styles 'partial-completion)

 :custom
 (completion-category-defaults nil)
 (completion-category-overrides nil)
 (minibuffer-electric-default-mode t)
 (minibuffer-message-clear-timeout 4)
 (completions-sort #'prescient-sort)
 (completions-max-height 20)
 (read-file-name-completion-ignore-case t)
 (completions-format 'one-column)
 (completions-detailed t)
 (completions-group t))

(use-package map-ynp
 :ensure nil
 :defer t

 :custom
 (read-answer-short t))

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
 :diminish

 :init
 (volatile-highlights-mode))

;;; Syntax Highlighting

(use-package jit-lock
 :ensure nil
 :defer t

 :custom
 (jit-lock-stealth-time 0.1)
 ;; A little more than what can fit on the screen.
 (jit-lock-chunk-size 4000)
 (jit-lock-antiblink-grace nil))

(use-package treesit
 :ensure nil
 :defer t

 :custom
 (treesit-language-source-alist
  '((bash   . ("https://github.com/tree-sitter/tree-sitter-bash"))
    (c      . ("https://github.com/tree-sitter/tree-sitter-c"))
    (cpp    . ("https://github.com/tree-sitter/tree-sitter-cpp"))
    (json   . ("https://github.com/tree-sitter/tree-sitter-json.git"))
    (python . ("https://github.com/tree-sitter/tree-sitter-python.git"))
    (toml   . ("https://github.com/ikatyang/tree-sitter-toml.git"))
    (yaml   . ("https://github.com/ikatyang/tree-sitter-yaml.git")))))

(use-package tree-sitter
 :ensure t
 :defer t
 :diminish "Ts")

(use-package tree-sitter-hl
 :ensure tree-sitter
 :defer t
 :hook tree-sitter-mode-hook
 :custom-face
 (tree-sitter-hl-face:property ((t (:inherit font-lock-keyword-face)))))

(use-package tree-sitter-langs
 :ensure t
 :defer t
 :hook
 (tree-sitter-mode-hook .
  (lambda ()
   (tree-sitter-langs-install-grammars t)))

 :custom
 (tree-sitter-langs-git-dir (file-name-concat tree-sitter-langs-grammar-dir "git")))

;;; Debuggers

(use-package debug
 :ensure nil
 :defer t

 :bind
 (:map debugger-mode-map ("C-g" . debugger-quit)))

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

(use-package dape
 :ensure t
 :defer t

 :preface
 (defun init/save-some-buffers ()
  "Save some buffers before starting dape."
  (save-some-buffers t t))

 :hook
 (kill-emacs-hook . dape-breakpoint-save)
 (after-init-hook . dape-breakpoint-load)
 (lsp-mode-hook . dape-breakpoint-global-mode)
 (dape-start-hook . init/save-some-buffers)
 (dape-start-hook . dape-info)

 :custom
 (dape-buffer-window-arrangement 'right)
 (dape-inlay-hints t)
 (dape-cwd-fn 'projectile-project-root))

(use-package pulse
 :ensure nil
 :defer t
 :after dape

 :hook
 (dape-display-source-hook . pulse-momentary-highlight-one-line))

;;; YAML

(use-package yaml-mode
 :ensure t
 :defer t

 :bind
 (:map yaml-mode-map
  ("C-c p" . qol/generate-password)))

(use-package flycheck
 :ensure t
 :defer t
 :after yaml-mode
 :hook yaml-mode-hook)

(use-package tree-sitter
 :ensure t
 :defer t
 :after yaml-mode
 :hook yaml-mode-hook)

;;; LLVM

(use-package llvm-ts-mode
 :ensure t
 :defer t
 :mode "\\.ll\\'")

(use-package demangle-mode
 :ensure t
 :defer t
 :after llvm-ts-mode
 :hook llvm-ts-mode-hook)

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

;;; CSS

(use-package css-mode
 :ensure nil
 :defer t

 :preface
 (defun init/css-setup-comments ()
  "Setup C-style /* ... */ comments."
  (with-eval-after-load 'newcomment
   (setq-local comment-style 'extra-line)))

 :hook
 (css-mode-hook . init/css-setup-comments))

;;; C and C++ Programming

(use-package files
 :ensure nil
 :defer t

 :custom
 (safe-local-variable-values '((comment-style . multi-line)
                               (backward-delete-char-untabify-method . nil))))

(use-package cc-mode
 :ensure nil
 :defer t

 :bind
 (:map c-mode-base-map
  ("(" . nil))

 :custom
 (c-doc-comment-style
  '((java-mode . javadoc)
    (c-mode    . gtkdoc)
    (c++-mode  . doxygen))))

(use-package editorconfig
 :ensure t
 :defer t
 :diminish "Ec"
 :after cc-mode
 :hook
 (c-mode-hook . editorconfig-mode))

(use-package cc-vars
 :ensure nil
 :defer t

 :custom
 (c-mark-wrong-style-of-comment t)
 (c-default-style '((other . "user")))
 (c-basic-offset 2)
 (c-tab-always-indent 'complete)

 :preface
 (defun init/cc-setup-comments ()
  "Setup C-style /* ... */ comments."
  (with-eval-after-load 'newcomment
   (setq-local comment-style 'extra-line)))

 :hook
 (c-mode-common-hook . init/cc-setup-comments))

(use-package lsp-mode
 :ensure t
 :defer t
 :after cc-mode
 :hook (c-mode-common-hook . lsp))

(use-package lsp-clangd
 :ensure lsp-mode
 :defer t

 :config
 (add-to-list 'lsp-clients-clangd-args "--enable-config")
 (add-to-list 'lsp-clients-clangd-args "--all-scopes-completion")
 (add-to-list 'lsp-clients-clangd-args "--background-index")
 (add-to-list 'lsp-clients-clangd-args "--clang-tidy")
 (add-to-list 'lsp-clients-clangd-args "--completion-style=detailed")
 (add-to-list 'lsp-clients-clangd-args "--function-arg-placeholders")
 (add-to-list 'lsp-clients-clangd-args "--header-insertion=iwyu")
 ;; This sometimes breaks LSP completion.
 (add-to-list 'lsp-clients-clangd-args "--header-insertion-decorators")
 (add-to-list 'lsp-clients-clangd-args "--limit-references=0")
 (add-to-list 'lsp-clients-clangd-args "--limit-results=0")
 (add-to-list 'lsp-clients-clangd-args "--rename-file-limit=0")
 (add-to-list 'lsp-clients-clangd-args "-j=16")
 (add-to-list 'lsp-clients-clangd-args "--malloc-trim")
 (add-to-list 'lsp-clients-clangd-args "--pch-storage=memory"))

(use-package lsp-clangd
 :ensure lsp-mode
 :defer t
 :after cc-mode

 :commands
 lsp-clangd-find-other-file

 :bind
 (:map c-mode-base-map ("<f2>" . lsp-clangd-find-other-file)))

;;; Python

(use-package python
 :ensure nil
 :defer t

 :preface
 (defun init/python-setup-fill-column ()
  "Set fill column in python-mode."
  (setq-local fill-column 79))

 :hook
 (python-mode-hook . init/python-setup-fill-column)
 (python-ts-mode-hook . init/python-setup-fill-column))

(use-package lsp-mode
 :ensure t
 :defer t
 :after python
 :hook ((python-mode-hook python-ts-mode-hook) . lsp))

;;; Project Management

(use-package projectile
 :ensure t
 :defer t
 :diminish "Pr"

 :bind
 (:map projectile-mode-map
  ("C-x p" . projectile-command-map))

 :custom
 (projectile-project-search-path '("~/Workspace"))
 (projectile-sort-order 'recently-active)
 (projectile-indexing-method 'hybrid)
 (projectile-enable-caching nil)
 (projectile-require-project-root nil))

(use-package counsel-projectile
 :ensure t
 :defer t

 :config
 (defadvice counsel-projectile-git-grep
  (after recenter-after-counsel-projectile-git-grep activate)
  (recenter))

 :init
 (counsel-projectile-mode))

(use-package treemacs-projectile
 :ensure t
 :defer t)

;;; Snippets

(use-package yasnippet
 :ensure t
 :defer t
 :diminish (yas-minor-mode . "Ys")

 :init
 (add-to-list 'yas-snippet-dirs "~/Workspace/dots/emacs/snippets")

 :preface
 (defun init/start-ivy-yasnippet ()
  "Start Ivy Yasnippet."
  (interactive)
  (yas-minor-mode-on)
  (ivy-yasnippet))

 :bind
 ("C-c Y" . init/start-ivy-yasnippet)
 ("C-c y s" . yas-expand-from-trigger-key))

(use-package yasnippet-snippets
 :ensure t
 :defer t
 :after yasnippet

 :init
 (yasnippet-snippets-initialize))

(use-package ivy-yasnippet
 :ensure t
 :defer t)

;;; Ledger

(use-package hledger-mode
 :ensure t
 :defer t
 :mode ("\\.journal\\'" "\\.ledger\\'")

 :custom
 (hledger-currency-string "EUR")
 (hledger-comments-column 1)
 (hledger-invalidate-completions '(on-save))

 :hook
 (hledger-mode-hook . (lambda () (setq-local tab-width 1))))

(use-package whitespace
 :ensure t
 :defer t
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package symbol-overlay
 :ensure t
 :defer t
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package yasnippet
 :ensure t
 :defer t
 :after hledger-mode
 :hook (hledger-mode-hook . yas-minor-mode-on))

(use-package company
 :ensure t
 :defer t
 :after hledger-mode
 :hook
 (hledger-mode-hook
  (hledger-mode-hook .
   (lambda ()
    (init/setup-company
     '(hledger-company))))))

(use-package flycheck
 :ensure t
 :defer t
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package display-fill-column-indicator
 :ensure t
 :defer t
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package hl-line
 :ensure t
 :defer t
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package flycheck-hledger
 :ensure t
 :defer t
 :after (flycheck hledger-mode)

 :custom
 ;; TODO Also add "accounts".
 (flycheck-hledger-checks '("commodities"))

 :hook
 (hledger-mode-hook . (lambda () (eval-when-compile (require 'flycheck-hledger)))))

;;; Web Development

(use-package web-mode
 :ensure t
 :defer t
 :mode "\\.html\\'"
 :mode "\\.css\\'"
 :mode "\\.js\\'"

 :custom
 (web-mode-markup-indent-offset 2)
 (web-mode-css-indent-offset 2)
 (web-mode-code-indent-offset 2)
 (web-mode-enable-current-column-highlight t)
 (web-mode-enable-current-element-highlight t)
 (web-mode-auto-close-style 3)
 (web-mode-enable-auto-expanding t)

 :hook
 (web-mode-hook . (lambda () (setq-local tab-width 2))))

(use-package lsp-mode
 :ensure t
 :defer t
 :after web-mode
 :hook (web-mode-hook . lsp))

(use-package company-web
 :ensure t
 :defer t
 :after (company web-mode)

 :hook
 (web-mode-hook .
  (lambda ()
   (init/setup-company
    '(company-css
      company-web-html)))))

(use-package emmet-mode
 :ensure t
 :defer t
 :hook web-mode-hook

 :custom
 (emmet-indentation 2))

;;; Docker

(use-package dockerfile-mode
 :ensure t
 :defer t)

(use-package docker-compose-mode
 :ensure t
 :defer t)

(use-package docker
 :ensure t
 :defer t
 :bind ("C-c D" . docker))

;;; Archlinux PKGBUILDs

(use-package pkgbuild-mode
 :ensure t
 :defer t
 :mode "\\PKGBUILD\\'")

;;; Rust

(use-package rust-mode
 :ensure t
 :defer t

 :bind
 (:map rust-mode-map
  ("<f5>" . rust-dbg-wrap-or-unwrap)
  ("<f6>" . lsp-rust-analyzer-expand-macro)
  ("<f7>" . lsp-rust-analyzer-join-lines))

 :custom
 (rust-indent-offset 2)
 (rust-load-optional-libraries nil)
 (rust-format-on-save t)

 :hook
 (rust-mode-hook . (lambda () (electric-quote-local-mode -1))))

(use-package lsp-mode
 :ensure t
 :defer t
 :after rust-mode
 :hook (rust-mode-hook . lsp))

(use-package subword
 :ensure nil
 :defer t
 :after rust-mode
 :hook rust-mode-hook)

(use-package lsp-rust
 :ensure lsp-mode
 :defer t

 :custom
 ;; (lsp-rust-analyzer-max-inlay-hint-length 50)
 ;; (lsp-rust-unstable-features t)
 (lsp-rust-analyzer-cargo-run-build-scripts t)
 (lsp-rust-analyzer-checkonsave-features "all")
 (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
 (lsp-rust-analyzer-proc-macro-enable t)
 (lsp-rust-racer-completion nil)
 (lsp-rust-build-bin t)
 (lsp-rust-build-lib t)
 (lsp-rust-clippy-preference "on")
 (lsp-rust-analyzer-display-chaining-hints t)
 (lsp-rust-analyzer-display-parameter-hints t)
 (lsp-rust-analyzer-display-closure-return-type-hints t)
 (lsp-rust-analyzer-display-lifetime-elision-hints-enable "always")
 (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
 (lsp-rust-analyzer-binding-mode-hints t)
 (lsp-rust-analyzer-display-reborrow-hints "mutable")
 (lsp-rust-all-features t)
 (lsp-rust-all-targets t)
 (lsp-rust-full-docs t)
 (lsp-rust-analyzer-cargo-watch-command "clippy"))

;;; LSP

(use-package lsp-mode
 :ensure t
 :defer t
 :diminish "Ls"

 :init
 ;; Improvements to LSP performance.
 (setenv "LSP_USE_PLISTS" "true")
 (setq-default read-process-output-max (* 1024 1024))

 :bind
 (:map lsp-mode-map
  ("C-c f" . lsp-format-buffer)
  ("C-c g" . lsp-format-region)
  ;; ("TAB"   . lsp-format-region)
  ("C-c h" . lsp-describe-thing-at-point)
  ("M-RET" . lsp-execute-code-action)
  ("<f8>"  . lsp-inlay-hints-mode)
  ([remap er/expand-region] . lsp-extend-selection))

 :hook
 (lsp-mode-hook . (lambda () (setq-local lsp-enable-relative-indentation t)))

 :custom
 (lsp-progress-prefix "  Progress: ")
 (lsp-completion-show-detail t)
 (lsp-completion-show-kind t)
 ;; (lsp-completion-provider :none)
 (lsp-headerline-breadcrumb-enable t)
 (lsp-restart 'auto-restart)
 (lsp-enable-snippet t)
 (lsp-keymap-prefix "C-c")
 ;; (lsp-idle-delay 0.9)
 (lsp-file-watch-threshold nil)
 (lsp-enable-semantic-highlighting t)
 (lsp-enable-indentation t)
 (lsp-before-save-edits nil)
 (lsp-auto-configure t)
 (lsp-signature-auto-activate t)
 ;; (lsp-signature-render-documentation nil)
 (lsp-eldoc-enable-hover t)
 ;; (lsp-eldoc-render-all nil)
 (lsp-modeline-code-actions-enable nil)
 (lsp-modeline-diagnostics-enable t)
 (lsp-log-io nil)
 (lsp-keep-workspace-alive nil)
 ;; (lsp-enable-imenu nil)
 (lsp-use-plists t)
 (lsp-auto-execute-action t))

(use-package lsp-mode
 :ensure t
 :defer t
 :after which-key

 :hook
 (lsp-mode-hook . lsp-enable-which-key-integration))

(use-package lsp-lens
 :ensure lsp-mode
 :defer t
 :diminish

 :custom
 (lsp-lens-mode nil)
 (lsp-lens-enable nil))

(use-package lsp-headerline
 :ensure lsp-mode
 :defer t

 :custom
 (lsp-headerline-breadcrumb-icons-enable nil))

(use-package lsp-semantic-tokens
 :ensure lsp-mode
 :defer t

 :custom
 (lsp-semantic-tokens-apply-modifiers t)
 (lsp-semantic-tokens-enable-multiline-token-support t)
 (lsp-semantic-tokens-enable t))

(use-package lsp-ivy
 :ensure t
 :defer t
 :after lsp-mode

 :preface
 (defun init/lsp-ivy-workspace-symbol ()
  (interactive)
  (xref-push-marker-stack)
  (lsp-ivy-workspace-symbol nil))
 (defun init/lsp-ivy-workspace-symbol-at-point ()
  (interactive)
  (xref-push-marker-stack)
  (lsp-ivy-workspace-symbol t))

 :bind
 (:map lsp-mode-map
  ("C-c X" . init/lsp-ivy-workspace-symbol)
  ("C-c x" . init/lsp-ivy-workspace-symbol-at-point))

 :config
 ;; Recenter after using lsp-ivy-workspace-symbol.
 (defadvice lsp-ivy-workspace-symbol
  (after recenter-after-lsp-ivy-workspace-symbol activate)
  (recenter)))

(use-package lsp-ui-peek
 :ensure lsp-ui
 :defer t
 :after lsp-mode

 :bind
 (:map lsp-mode-map
  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
  ([remap xref-find-references] . lsp-ui-peek-find-references)
  ("M-I" . lsp-ui-peek-find-implementation)
  ("C-c d" . lsp-ui-doc-show)))

(use-package lsp-ui-flycheck
 :ensure lsp-ui
 :defer t

 :bind
 (:map lsp-mode-map
  ("C-c l" . lsp-ui-flycheck-list)))

(use-package lsp-ui
 :ensure t
 :defer t
 :after lsp-ui-doc

 :custom
 (lsp-ui-doc-enable t)
 (lsp-ui-doc-show-with-cursor nil)
 (lsp-ui-doc-show-with-mouse t)
 (lsp-ui-doc-alignment 'frame)
 (lsp-ui-doc-header t)
 (lsp-ui-doc-include-signature t)
 (lsp-ui-doc-max-height 30)
 (lsp-ui-doc-use-webkit t))

(use-package lsp-ui
 :ensure t
 :defer t
 :after lsp-ui-peek

 :custom
 (lsp-ui-peek-list-width 40)
 (lsp-ui-peek-always-show t))

(use-package lsp-ui
 :ensure t
 :defer t
 :after lsp-ui-sideline

 :custom
 (lsp-ui-sideline-enable nil))

(use-package dap-mode
 :ensure t
 :defer t)

;;; Treemacs

(use-package treemacs
 :ensure t
 :defer t

 :bind
 ("<f9>" . treemacs-select-window))

(use-package treemacs-themes
 :ensure treemacs
 :defer t

 :commands
 treemacs-load-theme)

(use-package treemacs-customization
 :ensure treemacs
 :defer t

 :custom
 ;; (treemacs-tag-follow-delay 0.1)
 ;; (treemacs-indent-guide-mode t)
 (treemacs-select-when-already-in-treemacs 'move-back)
 (treemacs-width 40)
 (treemacs-indentation 1))

(use-package treemacs-interface
 :ensure treemacs
 :defer t
 :after treemacs

 :bind
 ("<f12>" . treemacs-delete-other-windows))

(use-package treemacs-mode
 :ensure treemacs
 :defer t

 :hook
 (treemacs-mode-hook . treemacs-fringe-indicator-mode)
 (treemacs-mode-hook . treemacs-filewatch-mode))

(use-package treemacs-async
 :ensure treemacs
 :defer t
 :after treemacs-mode
 :hook
 (treemacs-mode-hook . (lambda () (treemacs-git-mode 'deferred))))

(use-package treemacs-git-commit-diff-mode
 :ensure treemacs
 :defer t
 :after treemacs-mode
 :hook treemacs-mode-hook)

(use-package treemacs-tag-follow-mode
 :ensure treemacs
 :defer t
 :after treemacs-mode
 :hook treemacs-mode-hook)

(use-package treemacs-async
 :ensure treemacs
 :defer t
 :commands treemacs-git-mode)

(use-package treemacs-git-commit-diff-mode
 :ensure treemacs
 :defer t
 :commands treemacs-git-commit-diff-mode)

(use-package lsp-treemacs
 :ensure t
 :defer t
 :after lsp-mode

 :preface
 (defun init/lsp-treemacs-call-hierarchy ()
  (interactive)
  (lsp-treemacs-call-hierarchy t))
 (defun init/lsp-treemacs-implementations ()
  (interactive)
  (lsp-treemacs-implementations t))
 (defun init/lsp-treemacs-references ()
  (interactive)
  (lsp-treemacs-references t))
 (defun init/lsp-treemacs-type-hierarchy ()
  (interactive)
  (lsp-treemacs-type-hierarchy 2))

 :bind
 (:map lsp-mode-map
  ("C-c e" . lsp-treemacs-errors-list)
  ("C-c s" . lsp-treemacs-symbols)
  ("C-c c" . lsp-treemacs-call-hierarchy)
  ("C-c i" . lsp-treemacs-implementations)
  ("C-c f" . lsp-treemacs-references)
  ("C-c t" . init/lsp-treemacs-type-hierarchy)
  ("C-c C" . init/lsp-treemacs-call-hierarchy)
  ("C-c T" . init/lsp-treemacs-type-hierarchy)
  ("C-c I" . init/lsp-treemacs-implementations)
  ("C-c F" . init/lsp-treemacs-references))

 :hook
 (lsp-mode-hook . lsp-treemacs-sync-mode))

(use-package treemacs-projectile
 :ensure t
 :defer t
 :after (treemacs projectile))

(use-package treemacs-magit
 :ensure t
 :demand
 :after (treemacs magit))

(use-package treemacs-nerd-icons
 :ensure t
 :demand
 :after treemacs

 :config
 (treemacs-load-theme "nerd-icons"))

;; Print startup stats.
(message "Startup in %s (%d GC runs)" (emacs-init-time) gcs-done)

(provide 'init)
;;; init.el ends here

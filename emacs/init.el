;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Quality of Life

(use-package mode-local
 :ensure nil
 :defer t
 :commands setq-mode-local)

(use-package qol
 :ensure nil
 :defer t
 :load-path "/home/fred/Workspace/dots/emacs/"
 :commands
 qol/select-package
 qol/generate-password
 qol/append
 qol/remove
 qol/get-trimmed-line-string
 qol/string-starts-with)

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

;;; Configuration Files

(use-package no-littering
 :ensure t
 :demand
 :preface (qol/select-package 'no-littering)

 :commands
 no-littering-theme-backups
 no-littering-expand-etc-file-name
 no-littering-expand-var-file-name

 :config
 (no-littering-theme-backups))

;;; Modeline

(use-package diminish
 :ensure t
 :defer t
 :preface (qol/select-package 'diminish))

(use-package uniquify
 :ensure nil
 :defer t

 :custom
 (uniquify-buffer-name-style 'forward))

;;; Moving in Text

(use-package move-text
 :ensure t
 :defer t
 :preface (qol/select-package 'move-text)

 :init
 (move-text-default-bindings))

(use-package mwim
 :ensure t
 :defer t
 :preface (qol/select-package 'mwim)

 :bind
 ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line-or-comment)
 ([remap move-end-of-line] . mwim-end-of-code-or-line))

;;; Selecting Text

(use-package cua-base
 :ensure nil
 :defer t

 :init
 (cua-selection-mode t))

(use-package expand-region
 :ensure t
 :defer t
 :preface (qol/select-package 'expand-region)

 :bind
 ("C-=" . er/expand-region))

(use-package surround
 :ensure t
 :defer t
 :preface (qol/select-package 'surround)

 :bind
 ("M-'" . surround-mark-inner)
 ("M-\"" . surround-insert))

;;; Editing Text

(use-package misc
 :ensure nil
 :defer t

 :bind
 (("C-c d" . duplicate-dwim)))

(use-package files
 :ensure nil
 :defer t

 :custom
 (mode-require-final-newline 'visit-save)
 (require-final-newline 'visit-save)
 ;; File contents.
 (coding-system-for-read 'utf-8-unix)
 (coding-system-for-write 'utf-8-unix))

;;; Filling

(use-package unfill
 :ensure t
 :defer t
 :preface (qol/select-package 'unfill)

 :bind
 ([remap fill-paragraph] . unfill-toggle))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 ;; Fill
 (colon-double-space t)
 (default-justification 'left))

;;; Fill Column

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

;;; Indentation

(use-package indent
 :ensure nil
 :defer t

 :custom
 (tab-always-indent t)
 (tab-first-completion 'word))

(use-package simple
 :ensure nil
 :defer t
 :custom
 (indent-tabs-mode nil))

;;; Spell Checking

(use-package jinx
 :ensure t
 :defer t
 :preface (qol/select-package 'jinx)
 :after text-mode

 :hook text-mode-hook)

;;; Window Movement

(use-package windmove
 :ensure nil
 :defer t

 :init
 (windmove-default-keybindings)
 (windmove-delete-default-keybindings))

;;; Buffer Movement

(use-package buffer-move
 :ensure t
 :defer t
 :preface (qol/select-package 'buffer-move)

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

;;; Help

(use-package help
 :ensure nil
 :defer t

 :custom
 (help-window-select t)
 (help-window-keep-selected t))

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
 (defadvice find-file-literally
  (after recenter-after-find-file-literally activate)
  (recenter))
 (defadvice find-file-other-window
  (after recenter-after-find-file-other-window activate)
  (recenter))

 :custom
 (confirm-kill-processes nil))

(use-package button
 :ensure nil
 :defer t

 :config
 (defadvice push-button
  (after recenter-after-push-button activate)
  (recenter)))

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
 (completion-auto-select nil))

(use-package minibuffer
 :ensure nil
 :defer t

 :custom
 (completion-auto-help nil))

(use-package simple
 :ensure nil
 :defer t
 :after files

 :hook
 (before-save-hook . delete-trailing-whitespace))

(use-package nerd-icons
 :ensure t
 :defer t
 :preface (qol/select-package 'nerd-icons))

(use-package casual-suite
 :ensure t
 :defer t
 :preface (qol/select-package 'casual-suite))

(use-package casual-symbol-overlay
 :ensure casual-suite
 :defer t

 :config
 (symbol-overlay-mc-insert-into-casual-tmenu))

(use-package visual-replace
 :ensure t
 :defer t
 :preface (qol/select-package 'visual-replace)

 :bind
 ("M-%" . visual-replace-thing-at-point)
 ("M-^" . visual-replace-selected)
 ("M-*" . visual-replace))

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
 :preface (qol/select-package 'symbol-overlay)

 :bind
 (:map symbol-overlay-map ("C-p" . casual-symbol-overlay-tmenu)))

(use-package symbol-overlay-mc
 :ensure t
 :defer t
 :preface (qol/select-package 'symbol-overlay-mc)

 :bind
 (:map symbol-overlay-map ("M-a" . symbol-overlay-mc-mark-all)))

(use-package emacs
 :ensure nil
 :defer t

 :bind
 ("C-p" . casual-editkit-main-tmenu))

(use-package consult
 :ensure t
 :defer t
 :preface (qol/select-package 'consult)

 :preface
 (defun init/consult-grep-or-git-grep ()
  "Run grep in non-project buffers and git-grep in project buffers."
  (interactive)
  (if (and (fboundp 'projectile-project-root) (projectile-project-root))
   (consult-git-grep)
   (consult-grep)))

 :bind
 ([remap switch-to-buffer] . consult-buffer)
 ([remap jump-to-register] . consult-register)
 ("M-Y" . consult-yank-pop)
 ([remap imenu] . consult-imenu)
 ("M-g I" . consult-imenu-multi)
 ("C-x S" . consult-line)
 ("M-G" . init/consult-grep-or-git-grep)
 ("M-D" . consult-fd)
 (:map consult-narrow-map
  ("C-?" . consult-narrow-help))

 :config
 (defadvice consult-register
  (after recenter-after-consult-register activate)
  (recenter))
 (defadvice consult-buffer
  (after recenter-after-consult-buffer activate)
  (recenter))

 :hook
 (consult-after-jump-hook . recenter)

 :custom
 (consult-preview-key "M-.")
 (consult-project-function (lambda (_) (projectile-project-root))))

(use-package emacs
 :ensure nil
 :defer t

 :preface
 (defun init/find-file-other-window ()
  (interactive)
  (cond
   ((and (fboundp 'projectile-project-root) (projectile-project-root))
    (other-window-prefix)
    (projectile-find-file))
   ((fboundp 'consult-buffer-other-window)
    (consult-buffer-other-window))
   (t
    (call-interactively 'find-file-other-window))))

 :bind
 ([remap other-window] . init/find-file-other-window))

(use-package consult
 :ensure t
 :defer t
 :preface (qol/select-package 'consult)
 :after xref

 :custom
 (xref-show-xrefs-function #'consult-xref)
 (xref-show-definitions-function #'consult-xref))

(use-package embark-consult
 :ensure t
 :defer t
 :preface (qol/select-package 'embark-consult))

(use-package consult-flycheck
 :ensure t
 :defer t
 :preface (qol/select-package 'consult-flycheck)
 :after flycheck

 :bind
 (:map flycheck-mode-map
  ("C-c ! a" . consult-flycheck)))

(use-package vertico
 :ensure t
 :defer t

 :bind
 (:map vertico-map
  ("M-RET" . minibuffer-force-complete-and-exit)
  ("M-TAB" . minibuffer-complete)
  ("RET" . vertico-directory-enter)
  ("DEL" . vertico-directory-delete-char)
  ("M-DEL" . vertico-directory-delete-word))

 :custom
 (vertico-cycle t)
 (vertico-resize nil)

 :preface (qol/select-package 'vertico)

 :init
 (vertico-mode))

(use-package rfn-eshadow
 :ensure nil
 :defer t

 :hook
 (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

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
 :preface (qol/select-package 'company)
 :diminish "Co"
 :commands company--active-p

 :preface
 (defun init/setup-company (&optional main-backends secondary-backends)
  "Setup company completion system with common backends."
  (setq-local company-backends
   `((,@main-backends
      company-capf
      ;; company-dabbrev-code
      ;; company-keywords
      ;; company-yasnippet
      ;; company-files
      ,@secondary-backends
      :separate)))
  (company-mode))

 :custom
 (company-idle-delay 0.7)
 (company-keywords-ignore-case t)
 (company-selection-wrap-around t)
 (company-tooltip-align-annotations t)
 (company-tooltip-minimum-width 40)
 (company-tooltip-width-grow-only t))

(use-package company-posframe
 :ensure t
 :defer t
 :preface (qol/select-package 'company-posframe)
 :diminish
 :after company
 :hook company-mode-hook

 :config
 (qol/append company-posframe-show-params :border-width 1)
 (qol/append company-posframe-quickhelp-show-params :border-width 1)

 :custom
 (company-posframe-quickhelp-x-offset 2))

(use-package company-prescient
 :ensure t
 :defer t
 :preface (qol/select-package 'company-prescient)
 :after company
 :hook company-mode-hook)

;;; Syntax Checking

(use-package flycheck
 :ensure t
 :defer t
 :preface (qol/select-package 'flycheck)

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
 (flycheck-idle-change-delay 1)
 (flycheck-idle-buffer-switch-delay 1)
 (flycheck-display-errors-delay 1)

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
 :preface (qol/select-package 'flycheck-posframe)

 :custom
 (flycheck-posframe-prefix (concat " " (char-to-string 8618)  " Info: "))
 (flycheck-posframe-warning-prefix (concat " " (char-to-string 9888)  " Warning: "))
 (flycheck-posframe-error-prefix (concat " " (char-to-string 10540) " Error: "))
 (flycheck-posframe-position 'window-bottom-left-corner)
 (flycheck-posframe-border-width 1))

(use-package flycheck-posframe
 :ensure t
 :defer t
 :preface  (qol/select-package 'flycheck-posframe)
 :after flycheck
 :hook flycheck-mode-hook)

(use-package company
 :ensure t
 :defer t
 :preface (qol/select-package 'company)
 :after flycheck-posframe

 :preface
 (defun init/company-is-active (&rest _)
  (or (company--active-p) (bound-and-true-p company-backend)))

 :hook
 (flycheck-posframe-inhibit-functions . init/company-is-active))

(use-package cape
 :ensure t
 :defer t
 :preface (qol/select-package 'cape)

 :bind ("C-c p" . cape-prefix-map)

 :init
 (add-hook 'completion-at-point-functions #'cape-dabbrev)
 (add-hook 'completion-at-point-functions #'cape-file)
 (add-hook 'completion-at-point-functions #'cape-elisp-block))

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
 (recentf-exclude `(,(no-littering-expand-var-file-name "")
                    ,(no-littering-expand-etc-file-name "")
                    ,@native-comp-eln-load-path
                    "~/.cache"
                    "~/.config/emacs/var"
                    "~/.config/emacs/elpa"
                    "/usr/share/emacs"
                    "/run/media"))

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
 (split-width-threshold 130)
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

(use-package devdocs
 :ensure t
 :defer t
 :preface (qol/select-package 'devdocs)

 :bind
 ("C-h D" . devdocs-lookup)

 :preface
 (defmacro init/devdocs-set (mode doc)
  `(setq-mode-local ,mode devdocs-current-docs '(,doc)))

 :config
 (init/devdocs-set python-mode "python~3.13")
 (init/devdocs-set python-ts-mode "python~3.13")
 (init/devdocs-set rust-mode "rust")
 (init/devdocs-set c-mode "c")
 (init/devdocs-set dockerfile-mode "docker")
 (init/devdocs-set emacs-lisp-mode "elisp")
 (init/devdocs-set makefile-mode "gnu_make"))

(use-package elec-pair
 :ensure nil
 :defer t

 :custom
 (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
 (electric-pair-preserve-balance nil))

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
 :preface (qol/select-package 'diff-hl)
 :after prog-mode
 :hook prog-mode-hook)

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
 :preface (qol/select-package 'flycheck)
 :after prog-mode
 :hook prog-mode-hook)

(use-package yasnippet
 :ensure t
 :defer t
 :preface (qol/select-package 'yasnippet)
 :after prog-mode
 :hook (prog-mode-hook . yas-minor-mode-on))

(use-package company
 :ensure t
 :defer t
 :after prog-mode
 :hook (prog-mode-hook . init/setup-company)

 :preface
 (qol/select-package 'company))

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
 :preface (qol/select-package 'jinx)
 :after prog-mode
 :hook prog-mode-hook)

(use-package whitespace
 :ensure nil
 :defer t
 :after prog-mode
 :hook prog-mode-hook)

(use-package deadgrep
 :ensure t
 :defer t
 :preface (qol/select-package 'deadgrep)
 :after prog-mode

 :bind
 (:map prog-mode-map ("M-F" . deadgrep)))

(use-package wgrep
 :ensure t
 :defer t
 :preface (qol/select-package 'wgrep))

(use-package wgrep-deadgrep
 :ensure t
 :defer t
 :preface (qol/select-package 'wgrep-deadgrep)
 :after deadgrep)

(use-package sideline
 :ensure t
 :defer t
 :preface (qol/select-package 'sideline)
 :diminish "Si")

(use-package sideline-blame
 :ensure t
 :defer t
 :preface (qol/select-package 'sideline-blame)
 :after sideline

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

(use-package editorconfig
 :ensure t
 :defer t
 :diminish "Ec"

 :preface
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
 :defer t

 :preface
 (qol/select-package 'meson-mode))

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
 :preface (qol/select-package 'blamer)

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

 :preface
 (defun init/disable-line-numbers ()
  "Disable display-line-numbers-mode."
  (display-line-numbers-mode -1))

 :hook
 (magit-mode-hook . init/disable-line-numbers)

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
 :preface (qol/select-package 'diff-hl)

 :custom
 (diff-hl-flydiff-delay 1)
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

(use-package speedrect
 :ensure t
 :defer t
 :preface (qol/select-package 'speedrect)

 :init
 (speedrect-mode))

(use-package symbol-overlay
 :ensure t
 :defer t
 :preface (qol/select-package 'symbol-overlay)
 :diminish "So"

 :bind
 (:map symbol-overlay-map
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
 :defer t
 :preface (qol/select-package 'indent-guide))

(use-package crux
 :ensure t
 :defer t
 :preface (qol/select-package 'crux))

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
  '(face
    trailing
    tabs
    lines-tail
    missing-newline-at-eof
    empty
    space-after-tab
    space-before-tab
    tab-mark))

 :custom-face
 (whitespace-tab ((t (:foreground "lavender" :background "white smoke")))))

;;; Makefiles

(use-package whitespace
 :ensure nil
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
 :preface (qol/select-package 'symbol-overlay)
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package whitespace
 :ensure nil
 :defer t
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package highlight-defined
 :ensure t
 :defer t
 :preface (qol/select-package 'highlight-defined)
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package highlight-quoted
 :ensure t
 :defer t
 :preface (qol/select-package 'highlight-quoted)
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package eros
 :ensure t
 :defer t
 :preface (qol/select-package 'eros)
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package suggest
 :ensure t
 :defer t
 :preface (qol/select-package 'suggest))

(use-package ipretty
 :ensure t
 :defer t
 :preface (qol/select-package 'ipretty)
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
 :preface (qol/select-package 'lsp-mode)
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
 :preface (qol/select-package 'hl-line)
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
 :preface (qol/select-package 'nerd-icons-dired)
 :hook (dired-mode-hook . nerd-icons-dired-mode))

;;; Search

(use-package isearch
 :ensure nil
 :defer t

 :custom
 (isearch-lazy-count t)
 (isearch-lazy-highlight t)
 (lazy-count-prefix-format "(%s/%s) ")
 (search-whitespace-regexp ".*?"))

;;; CMake

(use-package cmake-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'cmake-mode))

(use-package eldoc-cmake
 :ensure t
 :defer t
 :preface (qol/select-package 'eldoc-cmake)
 :after cmake-mode
 :hook (cmake-mode-hook . eldoc-cmake-enable))

;;; Markdown

(use-package markdown-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'markdown-mode))

;;; Sed

(use-package sed-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'sed-mode))

;;; Po Translations

(use-package po-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'po-mode))

;;; TOML

(use-package toml-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'toml-mode))

(use-package eldoc-toml
 :ensure t
 :defer t
 :preface (qol/select-package 'eldoc-toml)
 :diminish
 :after (eldoc toml-mode)
 :hook toml-mode-hook)

;;; JSON

(use-package json-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'json-mode))

(use-package indent-guide
 :ensure t
 :defer t
 :preface (qol/select-package 'ident-guide)
 :after json-mode
 :hook json-mode-hook)

(use-package indent-guide
 :ensure t
 :defer t
 :preface (qol/select-package 'ident-guide)
 :after json-ts-mode
 :hook json-ts-mode-hook)

(use-package tree-sitter
 :ensure t
 :defer t
 :preface (qol/select-package 'tree-sitter)
 :diminish "Ts"
 :after json-mode
 :hook json-mode-hook)

;;; Spell Checking

(use-package jinx
 :ensure t
 :defer t
 :preface (qol/select-package 'jinx)
 :diminish "Jx"

 :bind
 (:map jinx-mode-map
  ("M-$"   . jinx-correct)
  ("C-M-$" . jinx-languages)))

;;; Emacs Tools

(use-package which-key
 :ensure t
 :defer t
 :preface (qol/select-package 'which-key)
 :diminish

 :custom
 ;; (which-key-idle-delay 0.5)
 (which-key-show-docstrings nil)
 (which-key-add-column-padding 3)
 (which-key-max-description-length nil)
 (which-key-max-display-columns nil)

 :init
 (which-key-mode))

(use-package nerd-icons-completion
 :ensure t
 :defer t
 :preface (qol/select-package 'nerd-icons-completion)

 :init
 (nerd-icons-completion-mode))

(use-package emacs
 :ensure nil
 :defer t

 :bind
 (:map minibuffer-local-map
  ("M-A" . marginalia-cycle)))

(use-package simple
 :ensure nil
 :defer t

 :bind
 (:map completion-list-mode-map
  ("M-A" . marginalia-cycle)))

(use-package marginalia
 :ensure t
 :defer t
 :preface (qol/select-package 'marginalia)

 :config
 (nerd-icons-completion-marginalia-setup)

 :init
 (marginalia-mode))

(use-package embark
 :ensure t
 :defer t
 :preface (qol/select-package 'embark)

 :bind
 (("C-." . embark-act)
  ("C-;" . embark-dwim)
  ("C-h B" . embark-bindings)
  :map minibuffer-local-map
  ("C-'" . embark-collect)
  ("C-x E" . embark-export)
  ("C-x B" . embark-become))

 :custom
 (prefix-help-command #'embark-prefix-help-command)
 (embark-mixed-indicator-both t)
 (embark-mixed-indicator-delay 0))

(use-package hotfuzz
 :ensure t
 :defer t
 :preface (qol/select-package 'hotfuzz)
 :after minibuffer

 :init
 (push 'hotfuzz completion-styles))

(use-package orderless
 :ensure t
 :defer t
 :preface (qol/select-package 'orderless)

 :config
 (push 'orderless-initialism orderless-matching-styles)
 (push 'orderless-prefixes orderless-matching-styles))

(use-package orderless
 :ensure t
 :defer t
 :preface (qol/select-package 'orderless)
 :after minibuffer

 :init
 (push 'orderless completion-styles))

(use-package prescient
 :ensure t
 :defer t
 :preface (qol/select-package 'prescient)

 :custom
 (prescient-sort-full-matches-first t)
 (completions-sort #'prescient-completion-sort)

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
 :preface (qol/select-package 'prescient)
 :after minibuffer

 :init
 (push 'prescient completion-styles)

 :custom
 (completions-sort #'prescient-completion-sort)

 :preface
 (defun init/prescient-remember-minibuffer-contents ()
  "Remember minibuffer contents as a completion candidate.

    - If we are not completing a file name (according to
      `minibuffer-completing-file-name'), we remember the
      minibuffer contents.

    - When completing file names, we remember the last component,
      including a trailing directory separator if needed."
  (let ((txt (minibuffer-contents-no-properties)))
   (unless (string-empty-p txt)
    (prescient-remember
     (if minibuffer-completing-file-name
      (if (directory-name-p txt)
       (thread-first txt file-name-split (last 2) car file-name-as-directory)
       (thread-first txt file-name-split last car))
      txt)))))

 :hook
 (minibuffer-exit-hook . init/prescient-remember-minibuffer-contents))

(use-package emacs
 :ensure nil
 :defer t

 :custom
 (enable-recursive-minibuffers t)

 :hook
 (minibuffer-setup-hook . cursor-intangible-mode))

(use-package minibuffer
 :ensure nil
 :defer t

 :config
 (qol/remove completion-styles 'emacs22)
 ;; (qol/remove completion-styles 'basic)
 ;; (qol/remove completion-styles 'partial-completion)

 :custom
 (completion-category-defaults nil)
 (completion-category-overrides nil)
 (minibuffer-electric-default-mode t)
 (minibuffer-message-clear-timeout 4)
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
 :preface (qol/select-package 'ctrlf)

 :custom
 (ctrlf-default-search-style 'fuzzy)
 (ctrlf-auto-recenter t)

 :init
 (ctrlf-mode))

(use-package transient
 :ensure t
 :defer t
 :preface (qol/select-package 'transient)

 :custom
 (transient-default-level 7))

(use-package multiple-cursors
 :ensure t
 :defer t
 :preface (qol/select-package 'multiple-cursors))

(use-package mc-edit-lines
 :ensure multiple-cursors
 :defer t

 :bind
 ("C-c C-v"       . mc/edit-lines))

(use-package mc-mark-more
 :ensure multiple-cursors
 :defer t

 :bind
 ("C->"           . mc/mark-next-like-this)
 ("C-<"           . mc/mark-previous-like-this)
 ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))

(use-package multiple-cursors-core
 :ensure multiple-cursors
 :defer t

 :custom
 (mc/always-run-for-all t))

(use-package volatile-highlights
 :ensure t
 :defer t
 :preface (qol/select-package 'volatile-highlights)
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
 :preface (qol/select-package 'tree-sitter)
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
 :preface (qol/select-package 'tree-sitter-langs)

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
 :preface (qol/select-package 'dape)

 :hook
 (dape-stopped-hook . dape-breakpoint-save)
 (dape-breakpoint-global-mode-hook . dape-breakpoint-load)
 (lsp-mode-hook . dape-breakpoint-global-mode)
 (dape-start-hook . save-some-buffers)
 (dape-start-hook . dape-info)
 (dape-display-source-hook . pulse-momentary-highlight-one-line)

 :custom
 (dape-buffer-window-arrangement 'right)
 (dape-inlay-hints t)
 (dape-cwd-fn 'projectile-project-root)
 (dape-default-breakpoints-file
  (no-littering-expand-var-file-name "dape-breakpoints")))

;;; YAML

(use-package yaml-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'yaml-mode)

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
 :preface (qol/select-package 'llvm-ts-mode)
 :mode "\\.ll\\'")

(use-package demangle-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'demangle-mode)
 :after llvm-ts-mode
 :hook llvm-ts-mode-hook)

(use-package autodisass-llvm-bitcode
 :ensure t
 :defer t
 :preface (qol/select-package 'autodisass-llvm-bitcode)
 :mode "\\.bc\\'")

(use-package demangle-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'demangle-mode))

(use-package yaml-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'yaml-mode)

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
 (safe-local-variable-values
  '((comment-style . multi-line)
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

(use-package cc-cmds
 :ensure nil
 :defer t

 :config
 ;; Unmark region even when c-indent-line-or-region doesn't indent anything.
 (defadvice c-indent-line-or-region
  (after deactivate-mark-after-c-indent-line-or-region activate)
  (deactivate-mark)))

(use-package editorconfig
 :ensure t
 :defer t
 :preface (qol/select-package 'editorconfig)
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
 :preface (qol/select-package 'lsp-mode)
 :after cc-mode
 :hook (c-mode-common-hook . lsp))

(use-package lsp-clangd
 :ensure lsp-mode
 :defer t
 :preface (qol/select-package 'lsp-mode)

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

 :config
 (setq-mode-local python-mode fill-column 79)
 (setq-mode-local python-ts-mode fill-column 79))

(use-package lsp-mode
 :ensure t
 :defer t
 :after python
 :hook ((python-mode-hook python-ts-mode-hook) . lsp))

;;; Project Management

(use-package projectile
 :ensure t
 :defer t
 :preface (qol/select-package 'projectile)
 :diminish "Pr"

 :commands
 projectile-project-root

 :bind
 (:map projectile-mode-map
  ("C-x p" . projectile-command-map))

 :custom
 (projectile-project-search-path '("~/Workspace"))
 (projectile-sort-order 'recently-active)
 (projectile-enable-caching nil)
 (projectile-indexing-method 'hybrid)
 (projectile-require-project-root nil)

 :init
 (projectile-mode))

(use-package consult-projectile
 :ensure t
 :defer t
 :preface (qol/select-package 'consult-projectile)
 :after projectile
 :bind ("C-x P" . consult-projectile))

(use-package treemacs-projectile
 :ensure t
 :defer t
 :preface (qol/select-package 'treemacs-projectile)
 :after (treemacs projectile))

;;; Snippets

(use-package yasnippet
 :ensure t
 :defer t
 :preface (qol/select-package 'yasnippet)
 :diminish (yas-minor-mode . "Ys")

 :init
 (add-to-list 'yas-snippet-dirs "~/Workspace/dots/emacs/snippets")

 :config
 (unbind-key "TAB" yas-minor-mode-map))

(use-package consult-yasnippet
 :ensure t
 :defer t
 :preface (qol/select-package 'consult-yasnippet)

 :bind
 ("M-z" . consult-yasnippet))

(use-package yasnippet-snippets
 :ensure t
 :defer t
 :preface (qol/select-package 'yasnippet-snippets)
 :after yasnippet

 :preface
 (defvar *init/yasnippet-snippets-initialized* nil)
 (defun init/initialize-yasnippet-snippets ()
  "Initialize yasnippet snippets if they have not already been."
  (unless *init/yasnippet-snippets-initialized*
   (yasnippet-snippets-initialize)
   (setq *init/yasnippet-snippets-initialized* t)))

 :hook
 (yas-minor-mode-hook . init/initialize-yasnippet-snippets))

;;; Ledger

(use-package hledger-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'hledger-mode)
 :mode ("\\.journal\\'" "\\.ledger\\'")

 :preface
 (defun init/move-amount-to-column ()
  "Move the amount or the point to the valid column."
  (interactive)
  (let ((amount-marker (concat " " hledger-currency-string " ")))
   (end-of-line)
   (when (search-backward amount-marker (pos-bol) t)
    (right-char))
   (kill-region (point) (pos-eol))
   (let ((difference (- (current-column) 64)))
    (if (> difference 0)
     (progn
      (left-char difference)
      (yank))
     (progn
      (insert-char ?\s (abs difference))
      (yank))))))

 (defun init/find-next-unaligned ()
  "Find the next unaligned amount in a non-comment line."
  (interactive)
  (let ((amount-marker (concat " " hledger-currency-string " ")))
   (when (search-forward amount-marker)
    (left-char (- (length amount-marker) 1))
    (if (or
         (= (current-column) 64)
         (qol/string-starts-with (qol/get-trimmed-line-string) ?\;))
     (init/find-next-unaligned)))))

 (defun init/align-next-unaligned ()
  "Aligned the next unaligned amount."
  (init/find-next-unaligned)
  (init/move-amount-to-column))

 :bind
 (:map hledger-mode-map ("C-c >" . init/move-amount-to-column))

 :custom
 (hledger-currency-string "EUR")
 (hledger-comments-column 1)
 (hledger-invalidate-completions '(on-save))

 :hook
 (hledger-mode-hook . (lambda () (setq-local tab-width 1))))

(use-package whitespace
 :ensure nil
 :defer t
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package symbol-overlay
 :ensure t
 :defer t
 :preface (qol/select-package 'symbol-overlay)
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package yasnippet
 :ensure t
 :defer t
 :preface (qol/select-package 'yasnippet)
 :after hledger-mode
 :hook (hledger-mode-hook . yas-minor-mode-on))

(use-package company
 :ensure t
 :defer t
 :preface (qol/select-package 'company)
 :after hledger-mode

 :preface
 (defun init/setup-hledger-company ()
  "Setup company for hledger completion."
  (init/setup-company '(hledger-company)))

 :hook
 (hledger-mode-hook . init/setup-hledger-company))

(use-package flycheck
 :ensure t
 :defer t
 :preface (qol/select-package 'flycheck)
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package display-fill-column-indicator
 :ensure t
 :defer t
 :preface (qol/select-package 'display-fill-column-indicator)
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package hl-line
 :ensure t
 :defer t
 :preface (qol/select-package 'hl-line)
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package flycheck-hledger
 :ensure t
 :defer t
 :preface (qol/select-package 'flycheck-hledger)
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
 :preface (qol/select-package 'web-mode)
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
 :preface (qol/select-package 'company-web)
 :after (company web-mode)

 :preface
 (defun init/setup-company-web ()
  "Setup company for web development."
  (init/setup-company '(company-css company-web-html)))

 :hook
 (web-mode-hook . init/setup-company-web))

(use-package emmet-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'emmet-mode)
 :hook web-mode-hook

 :custom
 (emmet-indentation 2))

;;; Docker

(use-package dockerfile-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'dockerfile-mode))

(use-package docker-compose-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'docker-compose-mode))

(use-package docker
 :ensure t
 :defer t
 :preface (qol/select-package 'docker)
 :bind ("C-c D" . docker))

;;; Archlinux PKGBUILDs

(use-package pkgbuild-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'pkgbuild-mode)
 :mode "\\PKGBUILD\\'")

;;; Rust

(use-package rust-mode
 :ensure t
 :defer t
 :preface (qol/select-package 'rust-mode)

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
 :preface (qol/select-package 'lsp-mode)
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
 (lsp-idle-delay 0.1)
 (lsp-file-watch-threshold nil)
 (lsp-enable-semantic-highlighting t)
 (lsp-enable-indentation t)
 (lsp-before-save-edits nil)
 (lsp-auto-configure t)
 ;; (lsp-signature-auto-activate t)
 ;; (lsp-signature-render-documentation nil)
 ;; (lsp-eldoc-enable-hover nil)
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

(use-package lsp-ui-peek
 :ensure lsp-ui
 :defer t
 :after lsp-mode
 :preface (qol/select-package 'lsp-ui)

 :bind
 (:map lsp-mode-map
  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
  ([remap xref-find-references] . lsp-ui-peek-find-references)
  ("M-I" . lsp-ui-peek-find-implementation)
  ("C-c d" . lsp-ui-doc-show)))

(use-package lsp-ui-imenu
 :ensure lsp-ui
 :defer t
 :preface (qol/select-package 'lsp-ui)

 :custom
 (lsp-ui-imenu-auto-refresh t)
 (lsp-ui-imenu-auto-refresh-delay 0.1)
 (lsp-ui-imenu-buffer-position 'left)
 (lsp-ui-imenu-window-fix-width t))

(use-package consult-lsp
 :ensure t
 :defer t
 :preface (qol/select-package 'consult-lsp)
 :after lsp-mode

 :bind
 (:map lsp-mode-map
  ("C-c ! d" . consult-lsp-diagnostics)
  ("C-c s s" . consult-lsp-symbols)))

(use-package lsp-ui-flycheck
 :ensure lsp-ui
 :defer t
 :preface (qol/select-package 'lsp-ui)

 :bind
 (:map lsp-mode-map
  ("C-c ! L" . lsp-ui-flycheck-list)))

(use-package lsp-ui
 :ensure t
 :defer t
 :preface (qol/select-package 'lsp-ui)

 :bind
 (:map lsp-mode-map
  ("C-c s S" . lsp-ui-find-workspace-symbol)))

(use-package lsp-ui
 :ensure t
 :defer t
 :preface (qol/select-package 'lsp-ui)
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
 :preface (qol/select-package 'lsp-ui)
 :after lsp-ui-peek

 :custom
 (lsp-ui-peek-list-width 40)
 (lsp-ui-peek-always-show t))

(use-package lsp-ui
 :ensure t
 :defer t
 :preface (qol/select-package 'lsp-ui)
 :after lsp-ui-sideline

 :custom
 (lsp-ui-sideline-enable nil))

;;; Treemacs

(use-package treemacs
 :ensure t
 :defer t
 :preface (qol/select-package 'treemacs)

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
 :preface (qol/select-package 'lsp-treemacs)
 :after lsp-mode

 :preface
 (defun init/lsp-treemacs-call-hierarchy ()
  "Show the outgoing call hierarchy of symbol at point."
  (interactive)
  (lsp-treemacs-call-hierarchy t))
 (defun init/lsp-treemacs-implementations ()
  "Show the implementations for the symbol at point and auto-select the window."
  (interactive)
  (lsp-treemacs-implementations t))
 (defun init/lsp-treemacs-references ()
  "Show the references for the symbol at point and auto-select the window."
  (interactive)
  (lsp-treemacs-references t))
 (defun init/lsp-treemacs-type-hierarchy ()
  "Show the full type hierarchy for the symbol at point."
  (interactive)
  (lsp-treemacs-type-hierarchy 2))

 :bind
 (:map lsp-mode-map
  ("C-c t e" . lsp-treemacs-errors-list)
  ("C-c t s" . lsp-treemacs-symbols)
  ("C-c t c" . lsp-treemacs-call-hierarchy)
  ("C-c t i" . lsp-treemacs-implementations)
  ("C-c t f" . lsp-treemacs-references)
  ("C-c t t" . init/lsp-treemacs-type-hierarchy)
  ("C-c t C" . init/lsp-treemacs-call-hierarchy)
  ("C-c t T" . init/lsp-treemacs-type-hierarchy)
  ("C-c t I" . init/lsp-treemacs-implementations)
  ("C-c t F" . init/lsp-treemacs-references))

 :hook
 (lsp-mode-hook . lsp-treemacs-sync-mode))

(use-package treemacs-magit
 :ensure t
 :demand
 :preface (qol/select-package 'treemacs-magit)
 :after (treemacs magit))

(use-package treemacs-nerd-icons
 :ensure t
 :demand
 :preface (qol/select-package 'treemacs-nerd-icons)
 :after treemacs

 :config
 (treemacs-load-theme "nerd-icons"))

;; Print startup stats.
(message "Startup in %s (%d GC runs)" (emacs-init-time) gcs-done)

(provide 'init)
;;; init.el ends here

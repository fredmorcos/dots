;;; init --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst emacs-extra-dir "/home/fred/Workspace/dots/emacs/extra")
(defconst expanded-user-emacs-dir (expand-file-name user-emacs-directory))
(defconst emacs-elpa-dir (concat expanded-user-emacs-dir "elpa"))
(defconst emacs-places-file (concat expanded-user-emacs-dir "places"))
(defconst emacs-recentf-file (concat expanded-user-emacs-dir "recentf"))
(defconst emacs-temp-dir (concat temporary-file-directory "emacs/"))
(defconst emacs-autosaves-dir (concat emacs-temp-dir "autosaves"))
(defconst emacs-autosaves-pattern (concat emacs-autosaves-dir "/\\1"))
(defconst emacs-backups-dir (concat emacs-temp-dir "backups"))
(defconst emacs-backups-pattern (concat emacs-backups-dir "/"))
(make-directory emacs-autosaves-dir t)
(make-directory emacs-backups-dir t)
(push emacs-extra-dir load-path)

(package-initialize)

(setq-default package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                                 ("melpa" . "https://melpa.org/packages/")
                                 ("org"   . "https://orgmode.org/elpa/")))

(use-package subr
  :ensure nil

  :commands
  add-to-list
  add-hook
  eval-after-load
  remove-overlays

  :custom
  (read-process-output-max (* 1024 1024))

  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package startup
  :ensure nil

  :init
  (defun replace-escapes ()
    "Replace strange newline escapes with proper UNIX newlines."
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
  (after-init . (lambda () (setq file-name-handler-alist nil)))
  (after-init . (lambda () (message "Startup in %s" (emacs-init-time)))))

(use-package faces
  :ensure nil

  :custom-face
  (cursor ((t (:background "SlateGray3"))))
  (region ((t (:background "LightSteelBlue1"))))
  (mode-line ((t (:foreground "Gray30" :background "Gray90" :box nil))))
  (mode-line-inactive ((t (:foreground "Gray50" :background "Gray90" :box nil)))))

(use-package scroll-bar
  :ensure nil

  :custom
  (horizontal-scroll-bar-mode nil)
  (scroll-conservatively 4)
  (hscroll-margin 1)
  (hscroll-step 1)
  (auto-hscroll-mode 'current-line))

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
  (line-number ((t (:foreground "Gray85"))))
  (line-number-current-line ((t (:foreground "Gray70"))))

  :hook
  (prog-mode . display-line-numbers-mode))

(use-package hl-line
  :ensure nil

  :custom-face
  (hl-line ((t (:background "Wheat"))))
  (mode-line-highlight ((t (:background "PowderBlue"))))

  :hook
  ((hledger-mode prog-mode text-mode) . hl-line-mode))

(use-package fringe
  :ensure nil

  :custom-face
  (fringe ((t (:background "Gray96"))))

  :config
  (set-fringe-style '(8 . 8)))

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
  (history-length 100))

(use-package recentf
  :ensure nil

  :commands
  recentf-cleanup

  :init
  (setq recentf-exclude `(,emacs-elpa-dir ,(expand-file-name "~/Oracle")))

  :config
  (recentf-cleanup)

  :custom
  (recentf-save-file emacs-recentf-file)
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 100)
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

(use-package electric
  :ensure nil

  :custom
  (electric-layout-mode t)
  (electric-pair-mode t)
  (electric-quote-mode t))

(use-package minibuf-eldef
  :ensure nil

  :custom
  (minibuffer-electric-default-mode t))

(use-package cua-base
  :ensure nil

  :commands
  cua-selection-mode

  :init
  (cua-selection-mode 1))

(use-package help
  :ensure nil

  :custom
  (help-window-select t))

(use-package window
  :ensure nil

  :commands
  split-window-horizontally
  window-width

  :custom
  (split-height-threshold 160)
  (even-window-sizes 'width-only))

(use-package windmove
  :ensure nil

  :init
  (windmove-default-keybindings))

(use-package simple
  :ensure nil

  :hook
  (before-save . delete-trailing-whitespace)

  :custom
  (undo-limit (* 1024 1024))
  (suggest-key-bindings 10)
  (column-number-mode t)
  (line-number-mode nil)
  (auto-save-mode t)
  (save-interprogram-paste-before-kill t)

  :init
  (defun move-line-up ()
    "Move a line up."
    (interactive)
    (transpose-lines 1)
    (forward-line -2))
  (defun move-line-down ()
    "Move a line down."
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1))

  :bind
  ("M-<up>"   . move-line-up)
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
  :diminish "Ab")

(use-package newcomment
  :ensure nil

  :custom
  (comment-fill-column 80))

(use-package fill
  :ensure nil

  :custom
  (fill-column 90)
  (colon-double-space t)
  (default-justification 'left))

(use-package indent
  :ensure nil

  :custom
  (indent-tabs-mode nil))

(use-package ediff-wind
  :ensure nil

  :commands
  ediff-setup-windows-plain

  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package whitespace
  :ensure nil

  :diminish "Ws"

  :custom
  (whitespace-line-column 90)
  (show-trailing-whitespace nil)
  (whitespace-action '(cleanup))
  (whitespace-style
   '(face tabs lines empty tab-mark
     indentation indentation::tab indentation::space
     space-after-tab space-after-tab::tab space-after-tab::space
     space-before-tab space-before-tab::tab space-before-tab::space))

  :hook
  ((hledger-mode emacs-lisp-mode makefile-mode) . whitespace-mode))

(use-package elisp-mode
  :ensure nil

  :custom
  (lisp-indent-offset 2)
  (lisp-indent-function #'common-lisp-indent-function)

  :mode
  ("\\emacs\\'" . emacs-lisp-mode)
  ("\\.config/emacs/init\\'" . emacs-lisp-mode))

(use-package text-mode
  :ensure nil

  :mode
  "\\Passwords.txt\\'"
  "\\Passwords_old.txt\\'"

  :hook
  (text-mode . (lambda () (toggle-truncate-lines t))))

(use-package eldoc
  :ensure nil
  :diminish "Ed"

  :hook
  (prog-mode . eldoc-mode))

(use-package paren
  :ensure nil

  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-style 'mixed)
  (show-paren-highlight-openparen t)

  :custom-face
  (show-paren-match ((t (:background "PowderBlue"))))
  (show-paren-match-expression ((t (:background "Lavender"))))
  (show-paren-mismatch ((t (:background "LightSalmon"))))

  :hook
  (prog-mode . show-paren-mode))

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
  (auto-revert-interval 1)
  (auto-revert-mode-text " AR")

  :hook
  (dired-mode . auto-revert-mode))

(use-package f)
(use-package ht)
(use-package dash)
(use-package diminish)
(use-package bind-key)
(use-package smex)
(use-package lv)

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("●" "○"))

  :hook
  (org-mode . org-bullets-mode))

(use-package org
  :pin org

  :custom
  (org-cycle-separator-lines 0)
  (org-startup-folded nil)
  (org-ellipsis "   ▾")

  :hook
  (org-mode . org-indent-mode)

  :custom-face
  (org-ellipsis ((t (:underline nil :foreground "DarkGoldenRod"))))
  (org-level-1 ((t (:height 1.3 :inherit (outline-1)))))
  (org-level-2 ((t (:height 1.2 :inherit (outline-2)))))
  (org-level-3 ((t (:height 1.1 :inherit (outline-3)))))
  (org-todo ((t (:foreground "Red1" :height 0.9))))
  (org-done ((t (:foreground "ForestGreen" :height 0.9)))))

(use-package which-key
  :diminish

  :custom
  (which-key-idle-delay 0.3)
  (which-key-mode t))

(use-package counsel
  :diminish

  :custom
  (counsel-mode t)

  :config
  (put 'counsel-find-symbol 'no-counsel-M-x t))

(use-package swiper
  :bind
  ("C-s"         . swiper-isearch)
  ("C-c C-c C-s" . swiper-all)
  ("C-c C-s"     . swiper-thing-at-point)
  ("C-r"         . swiper-isearch-backward))

(use-package ivy
  :diminish

  :bind
  (:map ivy-minibuffer-map ("RET" . ivy-alt-done))

  :custom
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order) (t . ivy--regex-plus)))
  (ivy-wrap t)
  (ivy-mode t)
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-virtual-abbreviate 'full)
  (ivy-initial-inputs-alist nil)
  (ivy-extra-directories nil))

(use-package ivy-rich
  :custom
  (ivy-rich-mode t))

(use-package fzf
  :bind
  ("M-F" . fzf-git-files))

(use-package deadgrep
  :bind
  ("M-G" . deadgrep))

(use-package transient
  ;; Magit-related

  :custom
  (transient-default-level 7))

(use-package magit
  :commands
  magit-display-buffer-same-window-except-diff-v1

  :bind
  ("C-x g" . magit-status)

  :hook
  (after-save . magit-after-save-refresh-status)

  :custom
  (magit-auto-revert-tracked-only nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories '(("~/Workspace" . 3) ("~/Oracle" . 3))))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package flycheck
  :bind
  (:map flycheck-mode-map
    ("M-n" . flycheck-next-error)
    ("M-p" . flycheck-previous-error))

  :custom
  (flycheck-checker-error-threshold nil)
  (flycheck-mode-line-prefix "FC")

  :hook
  (prog-mode . flycheck-mode))

(use-package company
  :diminish "Co"

  :custom
  (company-frontends '(company-echo-metadata-frontend company-pseudo-tooltip-frontend))
  (completion-ignore-case t)
  (company-echo-truncate-lines nil)
  (company-selection-wrap-around t)
  (company-tooltip-minimum 10)
  (company-tooltip-limit 20)
  (company-tooltip-align-annotations t)
  ;; (company-transformers '(company-sort-by-backend-importance))
  ;; (company-idle-delay 0.1)

  :hook
  (prog-mode . company-mode)
  (company-mode . (lambda () (setq company-backends
                                   '((company-capf company-keywords company-files)))))

  :custom-face
  (company-tooltip ((t (:background "gray95")))))

(use-package diff-hl
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-flydiff-delay 0.1)

  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  (prog-mode . diff-hl-mode)

  :custom-face
  (diff-hl-delete ((t (:background "RosyBrown1"))))
  (diff-hl-insert ((t (:background "DarkSeaGreen2"))))
  (diff-hl-change ((t (:background "PowderBlue")))))

(use-package symbol-overlay
  :diminish "Sy"

  :bind
  (:map symbol-overlay-mode-map
    ("M->" . symbol-overlay-jump-next)
    ("M-<" . symbol-overlay-jump-prev))

  :custom
  (symbol-overlay-idle-time 0.1)

  :custom-face
  (symbol-overlay-default-face ((t (:background "HoneyDew2"))))

  :hook
  ((hledger-mode emacs-lisp-mode) . symbol-overlay-mode))

(use-package multiple-cursors
  :bind
  ("C-c C-v"       . mc/edit-lines)
  ("C->"           . mc/mark-next-like-this)
  ("C-<"           . mc/mark-previous-like-this)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)

  :custom
  (mc/always-run-for-all t)

  :custom-face
  (mc/cursor-bar-face ((t (:background "Gray40" :foreground "White"))))
  (mc/cursor-face ((t (:background "Gray50" :foreground "White")))))

(use-package hledger-mode
  :mode
  "\\.journal\\'"
  "\\.ledger\\'"

  :custom
  (hledger-currency-string "EUR")
  (hledger-current-overlay t)
  (hledger-comments-column 1)

  :hook
  (hledger-mode . (lambda ()
                    (toggle-truncate-lines t)
                    (setq tab-width 1))))

(use-package toml-mode)
(use-package json-mode)

(use-package js
  :ensure nil

  :mode
  ("\\.hocon\\'" . js-mode))

(use-package llvm-mode
  :ensure nil

  :mode
  "\\.ll\\'"

  :hook
  (llvm-mode . (lambda () (toggle-truncate-lines t))))

(use-package indent-guide
  :hook
  (json-mode . indent-guide-mode)

  :custom-face
  (indent-guide-face ((t (:foreground "gray80")))))

(use-package rustic
  :commands
  rustic-clippy

  :init
  (autoload 'rust-dbg-wrap-or-unwrap "rust-mode")

  :bind
  (:map rustic-mode-map
    ("<f5>" . rust-dbg-wrap-or-unwrap)
    ("<f6>" . lsp-rust-analyzer-expand-macro)
    ("<f7>" . lsp-rust-analyzer-join-lines)
    ("<f8>" . lsp-rust-analyzer-inlay-hints-mode))

  :custom
  (rustic-lsp-server 'rust-analyzer)
  (rustic-analyzer-command '("/usr/bin/rust-analyzer"))
  (rustic-lsp-format t)

  :config
  (eval-after-load 'flycheck
    '(push #'rustic-clippy flycheck-checkers)))

(use-package lsp-mode
  :commands
  lsp
  lsp-deferred
  lsp-organize-imports
  lsp-rust-analyzer-initialized?
  lsp-request-async
  lsp--text-document-identifier
  lsp--range-to-region

  :bind
  (:map lsp-mode-map
        ("C-c f" . lsp-format-buffer)
        ("C-c r" . lsp-rename)
        ("C-c t" . lsp-describe-thing-at-point)
        ("C-="   . lsp-extend-selection)
        ("M-RET" . lsp-execute-code-action))

  :custom
  (lsp-enable-snippet nil)
  (lsp-keymap-prefix "C-c")
  (lsp-prefer-flymake nil)
  (lsp-prefer-capf t)
  (lsp-file-watch-threshold nil)
  (lsp-enable-semantic-highlighting t)
  (lsp-enable-indentation t)
  (lsp-enable-on-type-formatting t)
  (lsp-before-save-edits t)
  (lsp-auto-configure t)

  ;; (lsp-rust-full-docs t)
  (lsp-rust-racer-completion nil)
  (lsp-rust-build-bin t)
  (lsp-rust-build-lib t)
  (lsp-rust-clippy-preference "on")
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-server-display-inlay-hints t)

  (lsp-diagnostics-attributes `((unnecessary :background "Gray90")
                                (deprecated  :strike-through t)))

  (lsp-signature-auto-activate t)
  (lsp-signature-doc-lines 1)
  ;; (lsp-signature-render-documentation nil)

  :hook
  (lsp-mode . lsp-enable-which-key-integration)

  :config
  (defface lsp-rust-inlay-type-face
    '((t :background "OldLace" :foreground "DarkGray"))
    "Face for inlay type hints (e.g. inferred types)."
    :group 'lsp-rust)

  (defface lsp-rust-inlay-param-face
    '((t :background "Azure" :foreground "DarkGray"))
    "Face for inlay parameter hints (e.g. function parameter names at call-site)."
    :group 'lsp-rust)

  (eval-after-load 'lsp-rust
    '(defun lsp-rust-analyzer-update-inlay-hints (buffer)
       (if (and (lsp-rust-analyzer-initialized?)
             (eq buffer (current-buffer)))
         (lsp-request-async
           "rust-analyzer/inlayHints"
           (list :textDocument (lsp--text-document-identifier))
           (lambda (res)
             (remove-overlays (point-min) (point-max) 'lsp-rust-analyzer-inlay-hint t)
             (dolist (hint res)
               (-let* (((&hash "range" "label" "kind") hint)
                        ((beg . end) (lsp--range-to-region range))
                        (overlay (make-overlay beg end)))
                 (overlay-put overlay 'lsp-rust-analyzer-inlay-hint t)
                 (overlay-put overlay 'evaporate t)
                 (cond
                   ((string= kind "TypeHint")
                     (overlay-put overlay
                       'after-string
                       (concat (propertize ": " 'font-lock-face
                                 '(:foreground "darkgray"))
                         (propertize label 'font-lock-face
                           'lsp-rust-inlay-type-face))))
                   ((string= kind "ParameterHint")
                     (overlay-put overlay
                       'before-string
                       (concat (propertize label 'font-lock-face
                                 'lsp-rust-inlay-param-face)
                         (propertize ": " 'font-lock-face
                           '(:foreground "darkgray")))))))))
           :mode 'tick))
       nil))

  :custom-face
  (lsp-lens-face ((t (:inherit shadow))))
  (lsp-lens-mouse-face ((t (:inherit link)))))

(use-package lsp-ui
  :commands
  lsp-ui-mode

  :bind
  (:map lsp-mode-map
    ("M-."   . lsp-ui-peek-find-definitions)
    ("M-?"   . lsp-ui-peek-find-references)
    ("C-c h" . lsp-ui-doc-glance))

  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable nil)

  ;; :custom
  ;; (lsp-ui-flycheck-enable t)
  ;; (lsp-ui-flycheck-list-mode t)

  ;; (lsp-ui-peek-always-show t)
  ;; (lsp-ui-peek-show-directory nil)

  ;; (lsp-ui-doc-enable nil)
  ;; (lsp-ui-doc-header t)
  ;; (lsp-ui-doc-include-signature t)
  ;; (lsp-ui-doc-delay 0.5)
  ;; (lsp-ui-doc-border "black")
  ;; (lsp-ui-doc-alignment 'window)

  ;; (lsp-ui-sideline-enable nil)
  ;; (lsp-ui-sideline-delay 0.1)
  ;; (lsp-ui-sideline-update-mode 'line)
  ;; (lsp-ui-sideline-ignore-duplicate t)
  ;; (lsp-ui-sideline-show-hover t)

  :custom-face
  (lsp-ui-doc-background ((t (:background "Gray95"))))
  (lsp-ui-doc-header ((t (:background "Pale Turquoise"))))
  (lsp-ui-doc-border ((t (:background "Gray70"))))
  (lsp-ui-sideline-code-action ((t (:foreground "Sienna"))))
  (lsp-ui-sideline-global ((t (:foreground "Gray70"))))
  (lsp-ui-sideline-symbol-info ((t (:foreground "Gray70" :slant italic))))
  (lsp-ui-sideline-current-symbol ((t (:foreground "White" :background "Gray75"))))
  (lsp-ui-sideline-symbol ((t (:foreground "White" :background "Gray75")))))

(use-package posframe
  :pin melpa)

(use-package company-posframe
  :diminish

  :hook
  (company-mode . company-posframe-mode)

  :custom
  (company-posframe-show-params
   (list :internal-border-width 1 :internal-border-color "gray60")))

(use-package flycheck-posframe
  :hook
  (flycheck-mode . flycheck-posframe-mode)

  :custom
  (flycheck-posframe-position 'window-bottom-right-corner)
  (flycheck-posframe-border-width 1)
  (flycheck-posframe-warnings-prefix "Warning: ")
  (flycheck-posframe-error-prefix "Error: ")
  (flycheck-posframe-prefix "Info: ")

  :custom-face
  (flycheck-posframe-background-face ((t (:background "CornSilk"))))
  (flycheck-posframe-border-face ((t (:background "Wheat" :foreground "Wheat"))))
  (flycheck-posframe-error-face ((t (:foreground "DarkRed"))))
  (flycheck-posframe-warning-face ((t (:foreground "DarkOrange")))))

(use-package systemd
  :hook
  (systemd-mode . company-mode))

(use-package esup
  :commands esup)

(use-package projectile
  :diminish "Prj"

  :bind
  (:map projectile-mode-map ("C-x p" . projectile-command-map))

  :init
  (projectile-mode)

  :custom
  (projectile-project-search-path '("~/Workspace"))
  (projectile-sort-order '(recently-active))
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package cmake-mode)

(provide 'init)
;;; init ends here

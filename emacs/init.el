;;; init --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst expanded-user-emacs-dir (expand-file-name user-emacs-directory))
(defconst emacs-extra-dir (concat expanded-user-emacs-dir "extra/"))
(defconst emacs-elpa-dir (concat expanded-user-emacs-dir "elpa"))
(defconst emacs-places-file (concat expanded-user-emacs-dir "places"))
(defconst emacs-recentf-file (concat expanded-user-emacs-dir "recentf"))
(defconst emacs-temp-dir (concat temporary-file-directory "emacs/"))
(defconst emacs-autosaves-dir (concat emacs-temp-dir "autosaves"))
(defconst emacs-autosaves-pattern (concat emacs-autosaves-dir "/\\1"))
(defconst emacs-backups-dir (concat emacs-temp-dir "backups"))
(defconst emacs-backups-pattern (concat emacs-backups-dir "/"))
(make-directory emacs-extra-dir t)
(make-directory emacs-autosaves-dir t)
(make-directory emacs-backups-dir t)

(package-initialize)

(use-package package
  :ensure nil

  :custom
  (package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/")
                      ("org"   . "https://orgmode.org/elpa/"))))

(use-package subr
  :ensure nil

  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package startup
  :ensure nil

  :load-path
  emacs-extra-dir

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
  (default ((t (:font "Monospace 11"))))
  (cursor ((t (:background "SlateGray3"))))
  (region ((t (:background "LightSteelBlue1"))))
  (mode-line ((t (:foreground "Gray30" :background "Gray90" :box nil))))
  (mode-line-inactive ((t (:foreground "Gray50" :background "Gray90" :box nil)))))

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

(use-package fringe
  :ensure nil

  :custom-face
  (fringe ((t (:background "Gray97"))))

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
  (history-length 50))

(use-package recentf
  :ensure nil

  :init
  (setq recentf-exclude `(,emacs-elpa-dir))

  :config
  (recentf-cleanup)

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
  (electric-layout-mode t))

(use-package help
  :ensure nil

  :custom
  (help-window-select t))

(use-package window
  :ensure nil

  :custom
  (split-height-threshold 160))

(use-package windmove
  :ensure nil

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
  :diminish "Ab")

(use-package newcomment
  :ensure nil

  :custom
  (comment-fill-column 80))

(use-package display-fill-column-indicator
  :ensure nil

  :custom-face
  (fill-column-indicator ((t (:foreground "Azure2"))))

  :hook
  (prog-mode . display-fill-column-indicator-mode))

(use-package fill
  :ensure nil

  :custom
  (fill-column 80)
  (colon-double-space t)
  (default-justification 'left))

(use-package indent
  :ensure nil

  :custom
  (indent-tabs-mode nil))

(use-package ediff-wind
  :ensure nil

  :custom
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package whitespace
  :ensure nil

  :diminish

  :custom
  (show-trailing-whitespace nil)
  (whitespace-action '(cleanup))
  (whitespace-style
   '(face
     tabs
     indentation::tab
     indentation::space
     indentation))

  :hook
  ((prog-mode hledger-mode) . whitespace-mode))

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

  :hook
  (prog-mode . eldoc-mode))

(use-package paren
  :ensure nil

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
  (auto-revert-interval 1)
  (auto-revert-mode-text " AR")

  :hook
  (dired-mode . auto-revert-mode))

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
(use-package pkgbuild-mode)

(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '("●" "○"))

  :hook
  (org-mode . org-bullets-mode))

(use-package org
  :pin org

  :custom
  (org-cycle-separator-lines 0)
  (org-startup-folded nil)

  :hook
  (org-mode . org-indent-mode)

  :custom-face
  (org-ellipsis ((t (:underline nil :foreground "DarkGoldenRod"))))

  :config
  (setq org-ellipsis "   ▾"))

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
  ("M-x"     . counsel-M-x)
  ("C-x C-f" . counsel-find-file))

(use-package ivy
  :diminish

  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-alt-done))

  :custom
  ;; (ivy-display-style 'fancy)
  (ivy-wrap t)
  ;; (ivy-regex-ignore-order t)
  ;; (ivy-action-wrap t)
  (ivy-mode t)
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-virtual-abbreviate 'full)
  (ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :config
  (ivy-set-display-transformer
   'ivy-switch-buffer
   'ivy-rich--ivy-switch-buffer-transformer)

  :custom
  ;; (ivy-rich-switch-buffer-align-virtual-buffer t)
  ;; (ivy-rich-path-style 'abbrev)
  ;; (ivy-format-function #'ivy-format-function-line)
  (ivy-rich-mode t))

;; (use-package swiper
;;   :bind
;;   ("C-s" . swiper)
;;   ("C-r" . swiper-backward))

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
  (flycheck-mode-line-prefix "FC")

  :hook
  (prog-mode . flycheck-mode))

(use-package company
  :diminish "Co"

  :custom
  (company-backends '(company-capf company-keywords company-files))
  (company-frontends '(company-echo-metadata-frontend)) ;; company-pseudo-tooltip-frontend
  (completion-ignore-case t)
  (company-echo-truncate-lines nil)
  (company-selection-wrap-around t)
  (company-tooltip-minimum 10)
  (company-tooltip-limit 20)
  (company-tooltip-align-annotations t)
  ;; (company-transformers '(company-sort-by-backend-importance))
  ;; (company-idle-delay 0.1)

  :hook
  (prog-mode . company-mode))

(use-package company-box
  :diminish

  :custom
  (company-box-show-single-candidate t)
  (company-box-enable-icon nil)

  :hook
  (company-mode . company-box-mode))

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
  :diminish

  :bind
  ("M->" . symbol-overlay-jump-next)
  ("M-<" . symbol-overlay-just-prev)

  :custom
  (symbol-overlay-idle-time 0.1)

  :custom-face
  (symbol-overlay-default-face ((t (:background "HoneyDew2"))))

  :hook
  ((hledger-mode emacs-lisp-mode) . symbol-overlay-mode))

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
  :diminish "YS"

  :config
  (push (expand-file-name "~/Workspace/dots/emacs/snippets") yas-snippet-dirs)
  (diminish 'yas-minor-mode " Y")

  :hook
  ((hledger-mode prog-mode) . yas-minor-mode)
  (yas-minor-mode . yas-reload-all))

(use-package yasnippet-snippets)

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

(use-package js-mode
  :ensure nil

  :mode
  "\\.hocon\\'")

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

(use-package rust-mode
  :hook
  (rust-mode . (lambda ()
                 (setq-local standard-indent 4)
                 (setq-local comment-fill-column 90)
                 (setq tab-width 4
                       fill-column 100)))

  :bind
  (:map rust-mode-map
        ("<f6>" . lsp-rust-analyzer-expand-macro)
        ("<f7>" . lsp-rust-analyzer-join-lines))

  :custom-face
  (rust-question-mark-face ((t (:inherit (font-lock-builtin-face)))))

  :custom
  (rust-always-locate-project-on-open t))

;; (use-package rustic
;;   :mode
;;   "\\.rs\\'")

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
  (lsp lsp-deferred)

  :bind
  (:map lsp-mode-map
        ("C-c f" . lsp-format-buffer)
        ("C-c r" . lsp-rename)
        ("C-c t" . lsp-describe-thing-at-point)
        ("C-c s" . lsp-extend-selection))

  :custom
  (lsp-keymap-prefix "C-c")
  (lsp-idle-delay 0.1)
  (lsp-prefer-flymake nil)
  (lsp-file-watch-threshold 100000)
  (lsp-auto-guess-root t)
  (lsp-enable-semantic-highlighting t)

  (lsp-rust-full-docs t)
  (lsp-rust-wait-to-build 0.1)
  (lsp-rust-racer-completion nil)
  (lsp-rust-build-bin t)
  (lsp-rust-build-lib t)
  (lsp-rust-clippy-preference "on")
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-server-display-inlay-hints t)

  :hook
  (rust-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ui
  :commands
  lsp-ui-mode

  :bind
  (:map lsp-mode-map
        ("M-." . lsp-ui-peek-find-definitions)
        ("M-?" . lsp-ui-peek-find-references)
        ("C-c h" . lsp-ui-doc-glance))

  :custom
  (lsp-ui-flycheck-enable t)
  (lsp-ui-flycheck-list-mode t)

  (lsp-ui-peek-always-show t)
  (lsp-ui-peek-show-directory nil)

  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-doc-border "black")
  (lsp-ui-doc-alignment 'window)

  (lsp-ui-sideline-delay 0.1)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-hover t)

  :custom-face
  (lsp-lens-face ((t (:inherit shadow))))
  (lsp-lens-mouse-face ((t (:inherit link))))
  (lsp-ui-doc-background ((t (:background "Gray95"))))
  (lsp-ui-doc-header ((t (:background "Pale Turquoise"))))
  (lsp-ui-doc-border ((t (:background "Gray70"))))
  (lsp-ui-sideline-code-action ((t (:foreground "Sienna"))))
  (lsp-ui-sideline-global ((t (:foreground "Gray70"))))
  (lsp-ui-sideline-symbol-info ((t (:foreground "Gray70" :slant italic))))
  (lsp-ui-sideline-current-symbol ((t (:foreground "White" :background "Gray75"))))
  (lsp-ui-sideline-symbol ((t (:foreground "White" :background "Gray75")))))

(use-package company-lsp
  :commands
  company-lsp)

(use-package lsp-treemacs
  :commands
  lsp-treemacs-errors-list

  :hook
  (lsp-mode-hook . lsp-treemacs-sync-mode))

(use-package dap-mode)

(provide 'init)
;;; init ends here

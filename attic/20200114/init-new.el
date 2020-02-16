;;; init --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defalias 'set-f-a 'set-face-attribute)
(defalias 'set-f-bg 'set-face-background)
(defalias 'set-f-fg 'set-face-foreground)
(defalias 'custom 'custom-set-variables)
(defalias 'on 'eval-after-load)
(defalias 'setdef 'setq-default)

(set-f-a 'default nil :height 110)
(set-f-a 'mode-line nil :foreground "Gray30" :background "Gray90" :box nil)
(set-f-a 'mode-line-inactive nil :foreground "Gray50" :background "Gray90" :box nil)
(set-f-bg 'cursor "SlateGray3")
(set-f-bg 'region "LightSteelBlue1")

(custom '(tool-bar-mode nil)
        '(menu-bar-mode nil)
        '(scroll-bar-mode nil)
        '(horizontal-scroll-bar-mode nil))
(setdef scroll-conservatively 4
        horizontal-scroll-bar nil
        hscroll-margin 1
        hscroll-step 1
        auto-hscroll-mode 'current-line)
(setdef blink-cursor-mode nil
        frame-resize-pixelwise t
        frame-title-format "%b - emacs")

(on 'package '(setdef url-privacy-level 'high
                      url-proxy-services '(("no_proxy" . "127.0.0.1"))
                      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                                         ("org" . "https://orgmode.org/elpa/")
                                         ("melpa" . "https://melpa.org/packages/"))))

(setdef inhibit-startup-screen t
        inhibit-startup-message t
        inhibit-startup-buffer-menu t
        initial-scratch-message nil
        initial-major-mode 'fundamental-mode
        auto-save-list-file-prefix nil
        after-init-hook nil)
(add-hook 'after-init-hook #'(lambda () (setdef file-name-handler-alist nil)))

(defalias 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(setdef suggest-key-bindings 10)

(defun replace-escapes ()
  "Replace strange newline escapes with proper UNIX newlines."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\\n" nil t)
    (replace-match (char-to-string ?\n) nil t))
  (while (search-forward "\\t" nil t)
    (replace-match (char-to-string ?\t) nil t)))
(global-set-key (kbd "C-x e") #'replace-escapes)

(on 'display-line-numbers '(progn (set-f-fg 'line-number "Gray90")
                                  (set-f-fg 'line-number-current-line "Gray60")
                                  (setdef display-line-numbers-grow-only t
                                          display-line-numbers-width-start t)))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(on 'hl-line '(set-f-bg 'hl-line "CornSilk"))
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

(set-fringe-style '(8 . 8))
(set-f-bg 'fringe "Gray97")

(custom '(enable-recursive-minibuffers t)
        '(minibuffer-depth-indicate-mode t)
        '(minibuffer-electric-default-mode t))
(setdef minibuffer-allow-text-properties t
        minibuffer-auto-raise t)

(defconst emacs-elpa-dir (concat user-emacs-directory "elpa/"))
(defconst emacs-extra-dir (concat user-emacs-directory "extra/"))
(defconst emacs-places-file (concat user-emacs-directory "places"))
(defconst emacs-recentf-file (concat user-emacs-directory "recentf"))
(defconst emacs-temp-dir (concat temporary-file-directory "emacs/"))
(defconst emacs-autosaves-dir (concat emacs-temp-dir "autosaves"))
(defconst emacs-autosaves-pattern (concat emacs-autosaves-dir "/\\1"))
(defconst emacs-backups-dir (concat emacs-temp-dir "backups"))
(defconst emacs-backups-pattern (concat emacs-backups-dir "/"))
(push emacs-extra-dir load-path)
(run-with-idle-timer 0 nil #'(lambda () (progn (make-directory emacs-autosaves-dir t)
                                               (make-directory emacs-backups-dir t))))

(on 'saveplace '(setdef save-place-file emacs-places-file))
(on 'savehist '(setdef history-delete-duplicates t
                       history-length 50))
(on 'recentf '(progn (setdef recentf-save-file emacs-recentf-file
                             recentf-max-menu-items 50
                             recentf-max-saved-items 50)
                     (push emacs-elpa-dir recentf-exclude)
                     (recentf-exclude)))
(custom '(save-place t)
        '(save-place-mode t)
        '(savehist-mode t)
        '(recentf-mode t))

(setdef confirm-kill-processes nil
        auto-save-file-name-transforms `((".*" ,emacs-autosaves-pattern t))
        backup-directory-alist `((".*" . ,emacs-backups-pattern))
        backup-inhibited nil
        make-backup-files t
        delete-old-versions t
        mode-require-final-newline 'visit-save
        require-final-newline 'visit-save
        load-prefer-newer t
        coding-system-for-read 'utf-8-unix
        coding-system-for-write 'utf-8-unix)

(setdef help-window-select t)

(global-set-key (kbd "C-z") #'bury-buffer)
(global-set-key (kbd "s-w") #'delete-window)
(global-set-key (kbd "s-o") #'other-window)
(global-set-key (kbd "s-n") #'next-buffer)
(global-set-key (kbd "s-b") #'previous-buffer)
(global-set-key (kbd "s-v") #'split-window-below)
(global-set-key (kbd "s-h") #'split-window-right)
(global-set-key (kbd "s-a") #'backward-page)
(global-set-key (kbd "s-e") #'forward-page)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

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
(global-set-key (kbd "M-<up>") #'move-line-up)
(global-set-key (kbd "M-<down>") #'move-line-down)

(setdef column-number-mode t
        column-number-indicator-zero-based nil
        line-number-mode nil
        save-interprogram-paste-before-kill t)

(custom '(auto-save-mode t))

(setdef uniquify-buffer-name-style 'forward)

(on 'vc '(setdef vc-make-backup-files t))

(defun diminish (mode name)
  "Rename minor MODE to NAME."
  (let ((new (assq mode minor-mode-alist)))
    (setf (nth 1 new) name)))

(on 'abbrev '(diminish 'abbrev-mode " Abb"))

(setdef comment-fill-column 70)

(on 'display-fill-column-indicator '(progn (set-f-fg 'fill-column-indicator "Azure2")))
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(setdef fill-column 70
        colon-double-space t
        default-justification 'left)

(setdef indent-tabs-mode nil)

(on 'ediff-wind '(custom '(ediff-split-window-function #'split-window-horizontally)
                         '(ediff-window-setup-function #'ediff-setup-windows-plain)))

(push '("\\emacs\\'" . emacs-lisp-mode) auto-mode-alist)
(push '("\\.config/emacs/init\\'" . emacs-lisp-mode) auto-mode-alist)
(add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                    (setq fill-column 90)
                                    (setq-local comment-fill-column 90)))

(on 'whitespace '(progn (diminish 'global-whitespace-newline-mode " GNL")
                        (diminish 'global-whitespace-mode " GWS")
                        (diminish 'whitespace-newline-mode " NL")
                        (diminish 'whitespace-mode "")
                        (setdef whitespace-style '(face
                                                   tabs
                                                   indentation::tab
                                                   indentation::space
                                                   indentation))))
(add-hook 'prog-mode-hook #'whitespace-mode)

(add-hook 'prog-mode-hook #'eldoc-mode)

(push '("\\Passwords.txt\\'" . text-mode) auto-mode-alist)
(push '("\\Passwords_old.txt\\'" . text-mode) auto-mode-alist)
(add-hook 'text-mode-hook #'(lambda () (toggle-truncate-lines t)))

(custom '(show-paren-mode t))
(set-f-bg 'show-paren-match "PowderBlue")
(set-f-bg 'show-paren-match-expression "AliceBlue")
(set-f-bg 'show-paren-mismatch "LightSalmon")

(setdef dired-listing-switches "-l --group-directories-first"
        dired-hide-details-hide-symlink-targets nil)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(on 'autorevert '(progn (setdef auto-revert-mode-text " AR"
                                auto-revert-interval 1)))
(add-hook 'dired-mode-hook #'auto-revert-mode)

;; (use-package f)
;; (use-package ht)
;; (use-package dash)
;; (use-package diminish)
;; (use-package bind-key)
;; (use-package flx)
;; (use-package amx)
;; (use-package smex)
;; (use-package lv)
;; (use-package all-the-icons)
;; (use-package pkgbuild-mode)

;; (use-package org-bullets
;;   :config
;;   (setq org-bullets-bullet-list '("●" "○"))

;;   :hook
;;   (org-mode . org-bullets-mode))

;; (use-package org
;;   :pin org

;;   :custom
;;   (org-cycle-separator-lines 0)
;;   (org-indent-indentation-per-level 2)
;;   (org-startup-folded t)

;;   :hook
;;   (org-mode . org-indent-mode)
;;   (org-mode . (lambda () (jit-lock-register 'flyspell-region)))
;;   (org-mode . (lambda () (add-hook 'after-save-hook #'flyspell-buffer nil t)))

;;   :custom-face
;;   (org-ellipsis ((t (:underline nil :foreground "DarkGoldenRod"))))

;;   :config
;;   (setq org-ellipsis "   ▾"))

;; (use-package iedit
;;   :bind
;;   ("C-;" . iedit-mode))

;; (use-package which-key
;;   :diminish

;;   :custom
;;   (which-key-idle-delay 0.1)
;;   (which-key-mode t))

;; (use-package counsel
;;   :diminish

;;   :custom
;;   (counsel-mode t)

;;   :bind
;;   ("M-x"     . counsel-M-x)
;;   ("C-x C-f" . counsel-find-file)
;;   ("M-A"     . counsel-ag)
;;   ("M-R"     . counsel-rg))

;; (use-package ivy
;;   :diminish

;;   :bind
;;   (:map ivy-minibuffer-map
;;         ("RET" . ivy-alt-done))

;;   :custom
;;   (ivy-mode t)
;;   (ivy-use-selectable-prompt t)
;;   (ivy-use-virtual-buffers t)
;;   (ivy-display-style 'fancy)
;;   (ivy-count-format "(%d/%d) ")
;;   (ivy-wrap t)
;;   (ivy-regex-ignore-order t)
;;   (ivy-virtual-abbreviate 'full)
;;   (ivy-action-wrap t)
;;   (ivy-initial-inputs-alist nil))

;; (use-package ivy-rich
;;   :commands
;;   ivy-set-display-transformer
;;   ivy-format-function-line

;;   :config
;;   (ivy-set-display-transformer
;;    'ivy-switch-buffer
;;    'ivy-rich--ivy-switch-buffer-transformer)

;;   :custom
;;   (ivy-rich-mode t)
;;   (ivy-rich-switch-buffer-align-virtual-buffer t)
;;   (ivy-rich-path-style 'abbrev)
;;   (ivy-format-function #'ivy-format-function-line))

;; (use-package swiper
;;   :bind
;;   ("C-s" . swiper)
;;   ("C-r" . swiper-backward))

;; (use-package fzf
;;   :bind
;;   ("M-F" . fzf-git-files)
;;   ("M-P" . fzf-git-grep))

;; (use-package deadgrep
;;   :bind
;;   ("M-G" . deadgrep))

;; (use-package transient
;;   ;; Magit-related

;;   :custom
;;   (transient-default-level 7))

;; (use-package magit
;;   :bind
;;   ("C-x g" . magit-status)

;;   :hook
;;   (after-save . magit-after-save-refresh-status)

;;   :defines
;;   magit-status-buffer-switch-function

;;   :commands
;;   magit-display-buffer-same-window-except-diff-v1

;;   :custom
;;   (magit-auto-revert-tracked-only nil)
;;   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
;;   (magit-repository-directories '(("~/Workspace" . 3) ("~/Oracle" . 3))))

;; (use-package expand-region
;;   :bind
;;   ("C-=" . er/expand-region))

;; (use-package ivy-xref
;;   :init
;;   (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; (use-package flycheck
;;   :bind
;;   (:map flycheck-mode-map
;;         ("M-n" . flycheck-next-error)
;;         ("M-p" . flycheck-previous-error)
;;         ("C-c l" . flycheck-list-errors))

;;   :custom
;;   (flycheck-checker-error-threshold nil)
;;   (flycheck-mode-line-prefix "Chk")
;;   (flycheck-idle-change-delay 0.1)
;;   (flycheck-display-errors-delay 0.1)
;;   (flycheck-idle-buffer-switch-delay 0.1)
;;   (flycheck-disabled-checkers '(rust-cargo rust-clippy rust))

;;   :hook
;;   ((prog-mode z3-smt2-mode) . flycheck-mode))

;; (use-package company
;;   :diminish "Com"

;;   :custom
;;   (company-backends '(company-capf company-keywords company-files))
;;   (company-frontends '(company-echo-metadata-frontend)) ;; company-pseudo-tooltip-frontend
;;   (completion-ignore-case t)
;;   (company-echo-truncate-lines nil)
;;   (company-selection-wrap-around t)
;;   (company-tooltip-minimum 10)
;;   (company-tooltip-limit 20)
;;   (company-tooltip-align-annotations t)
;;   (company-transformers '(company-sort-by-backend-importance))
;;   (company-idle-delay 0.1)

;;   :hook
;;   (prog-mode . company-mode))

;; (use-package font-lock+
;;   :ensure nil)

;; (use-package icons-in-terminal
;;   :ensure nil

;;   :init
;;   (add-to-list 'load-path "/usr/share/icons-in-terminal/")

;;   :config
;;   (require 'icons-in-terminal))

;; (use-package all-the-icons
;;   :commands
;;   all-the-icons-faicon)

;; (use-package company-box
;;   :diminish

;;   :custom
;;   (company-box-show-single-candidate t)
;;   (company-box-icons-alist 'company-box-icons-icons-in-terminal)

;;   :hook
;;   (company-mode . company-box-mode))

;; ;; (use-package company-quickhelp
;; ;;   :hook
;; ;;   (company-mode . company-quickhelp-mode)

;; ;;   :bind
;; ;;   (:map company-active-map
;; ;;         ("C-c h" . company-quickhelp-manual-begin))

;; ;;   :custom
;; ;;   (company-quickhelp-use-propertized-text t)
;; ;;   (company-quickhelp-delay nil))

;; (use-package diff-hl
;;   :custom
;;   (diff-hl-draw-borders nil)
;;   (diff-hl-flydiff-delay 0.1)

;;   :commands
;;   diff-hl-magit-post-refresh

;;   :hook
;;   (magit-post-refresh . diff-hl-magit-post-refresh)
;;   ((prog-mode z3-smt2-mode) . diff-hl-mode)

;;   :custom-face
;;   (diff-hl-delete ((t (:background "RosyBrown1"))))
;;   (diff-hl-insert ((t (:background "DarkSeaGreen2"))))
;;   (diff-hl-change ((t (:background "PowderBlue")))))

;; (use-package symbol-overlay
;;   :diminish

;;   :bind
;;   ("M->" . symbol-overlay-jump-next)
;;   ("M-<" . symbol-overlay-just-prev)

;;   :custom
;;   (symbol-overlay-idle-time 0.1)

;;   :custom-face
;;   (symbol-overlay-default-face ((t (:background "HoneyDew2"))))

;;   :hook
;;   ((hledger-mode prog-mode z3-smt2-mode) . symbol-overlay-mode))

;; (use-package multiple-cursors
;;   :bind
;;   ("C-c C-v" . mc/edit-lines)
;;   ("C->" . mc/mark-next-like-this)
;;   ("C-<" . mc/mark-previous-like-this)
;;   ("C-S-<mouse-1>" . mc/add-cursor-on-click)

;;   :custom
;;   (mc/always-run-for-all t)

;;   :custom-face
;;   (mc/cursor-bar-face ((t (:background "Gray40" :foreground "White"))))
;;   (mc/cursor-face ((t (:background "Gray50" :foreground "White")))))

;; (use-package yasnippet
;;   :commands
;;   yas-reload-all

;;   :defines
;;   yas-snippet-dirs

;;   :config
;;   (push (expand-file-name "~/Workspace/dots/emacs/snippets") yas-snippet-dirs)
;;   (make-thread #'yas-reload-all)

;;   :hook
;;   ((hledger-mode c-mode rust-mode) . yas-minor-mode))

;; (use-package yasnippet-snippets)

;; (use-package helpful
;;   :after counsel

;;   :bind
;;   ([remap describe-variable] . helpful-variable)
;;   ([remap describe-function] . helpful-function)
;;   ([remap describe-key]      . helpful-key)
;;   ([remap describe-mode]     . helpful-mode)

;;   :custom
;;   (counsel-describe-variable-function #'helpful-variable)
;;   (counsel-describe-function-function #'helpful-function))

;; (use-package unfill
;;   :bind
;;   ([remap fill-paragraph] . unfill-toggle))

;; (use-package hledger-mode
;;   :mode
;;   "\\.journal\\'"
;;   "\\.ledger\\'"

;;   :custom
;;   (hledger-comments-column 2)
;;   (hledger-currency-string "EUR")
;;   (hledger-current-overlay t)

;;   :commands
;;   toggle-truncate-lines

;;   :hook
;;   (hledger-mode . (lambda () (toggle-truncate-lines t))))

;; (use-package toml-mode)
;; (use-package json-mode)

;; (use-package llvm-mode
;;   :ensure nil

;;   :mode
;;   "\\.ll\\'"

;;   :hook
;;   (llvm-mode . (lambda () (toggle-truncate-lines t))))

;; (use-package rmsbolt
;;   :diminish "Bolt"

;;   :custom
;;   (rms-bolt-lighter "Bolt"))

;; (use-package boogie-friends
;;   :custom
;;   (z3-smt2-prover-custom-args
;;    '("smt.relevancy=1" "sat.acce=true" "smt.arith.solver=6")))

;; (use-package rust-mode
;;   :hook
;;   (rust-mode . (lambda ()
;;                  ;; (setq-local standard-indent 4)
;;                  (setq-local comment-fill-column 90)
;;                  ;; (setq tab-width 4
;;                        fill-column 90)))

;;   :bind
;;   (:map rust-mode-map
;;         ("<f6>" . rust-analyzer-expand-macro))

;;   :custom-face
;;   (rust-question-mark-face ((t (:inherit (font-lock-builtin-face)))))

;;   :custom
;;   (rust-always-locate-project-on-open t))

;; (use-package cargo
;;   :init
;;   (defun cargo-fmt-and-lint ()
;;     (interactive)
;;     (cargo-process-fmt)
;;     (sit-for 1)
;;     (cargo-process-clippy))

;;   :bind
;;   (:map cargo-minor-mode-map
;;         ("C-c C-c C-c" . cargo-fmt-and-lint))

;;   :hook
;;   (rust-mode . cargo-minor-mode))

;; (use-package c-mode
;;   :ensure nil

;;   :custom
;;   (lsp-clients-clangd-args
;;    '("--background-index"
;;      "--clang-tidy"
;;      "--completion-style=detailed"
;;      "--header-insertion=iwyu"
;;      "--header-insertion-decorators"
;;      "--suggest-missing-includes"
;;      "--fallback-style=llvm"
;;      "-j=13"
;;      "--pch-storage=memory")))

;; (use-package lsp-mode
;;   :commands
;;   lsp-deferred

;;   :bind
;;   (:map lsp-mode-map
;;         ("C-c f" . lsp-format-buffer)
;;         ("C-c r" . lsp-rename)
;;         ("C-c t" . lsp-describe-thing-at-point))

;;   :custom
;;   (lsp-idle-delay 0.1)
;;   (lsp-prefer-flymake nil)
;;   (lsp-file-watch-threshold 100000)

;;   (lsp-rust-full-docs t)
;;   (lsp-rust-wait-to-build 0.1)
;;   (lsp-rust-racer-completion nil)
;;   (lsp-rust-build-bin t)
;;   (lsp-rust-build-lib t)
;;   (lsp-rust-clippy-preference "on")
;;   (lsp-rust-server 'rust-analyzer)
;;   (lsp-rust-analyzer-server-display-inlay-hints t)

;;   (lsp-auto-guess-root t)

;;   :hook
;;   ((c-mode rust-mode) . lsp-deferred))

;; (use-package lsp-ui
;;   :commands
;;   lsp-ui-mode

;;   :bind
;;   (:map lsp-mode-map
;;         ("M-." . lsp-ui-peek-find-definitions)
;;         ("M-?" . lsp-ui-peek-find-references)
;;         ("C-c h" . lsp-ui-doc-glance))

;;   :custom
;;   (lsp-ui-flycheck-enable t)
;;   (lsp-ui-flycheck-list-mode t)

;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-peek-show-directory nil)

;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-header t)
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-ui-doc-delay 0.5)
;;   (lsp-ui-doc-border "black")
;;   (lsp-ui-doc-alignment 'window)

;;   (lsp-ui-sideline-delay 0.1)
;;   (lsp-ui-sideline-update-mode 'line)
;;   (lsp-ui-sideline-ignore-duplicate t)
;;   (lsp-ui-sideline-show-hover t)

;;   :custom-face
;;   (lsp-lens-face ((t (:inherit shadow))))
;;   (lsp-lens-mouse-face ((t (:inherit link))))
;;   (lsp-ui-doc-background ((t (:background "Gray95"))))
;;   (lsp-ui-doc-header ((t (:background "Pale Turquoise"))))
;;   (lsp-ui-doc-border ((t (:background "Gray70"))))
;;   (lsp-ui-sideline-code-action ((t (:foreground "Sienna"))))
;;   (lsp-ui-sideline-global ((t (:foreground "Gray70"))))
;;   (lsp-ui-sideline-symbol-info ((t (:foreground "Gray70" :slant italic))))
;;   (lsp-ui-sideline-current-symbol ((t (:foreground "White" :background "Gray75"))))
;;   (lsp-ui-sideline-symbol ((t (:foreground "White" :background "Gray75")))))

;; (use-package company-lsp)

;; (use-package indent-guide
;;   :hook
;;   (json-mode . indent-guide-mode)

;;   :custom-face
;;   (indent-guide-face ((t (:foreground "gray80")))))

(provide 'init)
;;; init ends here

;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (use-package tree-sitter-langs
;;  :hook (tree-sitter-mode . (lambda () (tree-sitter-langs-install-grammars t))))

;; (use-package tree-sitter-hl
;;  :ensure nil
;;  :hook tree-sitter-mode)

;; (use-package treesit
;;  :ensure nil
;;  :custom
;;  (treesit-language-source-alist
;;   '((bash   . ("https://github.com/tree-sitter/tree-sitter-bash"))
;;     (c      . ("https://github.com/tree-sitter/tree-sitter-c"))
;;     (cpp    . ("https://github.com/tree-sitter/tree-sitter-cpp"))
;;     (json   . ("https://github.com/tree-sitter/tree-sitter-json.git"))
;;     (python . ("https://github.com/tree-sitter/tree-sitter-python.git"))
;;     (toml   . ("https://github.com/ikatyang/tree-sitter-toml.git"))
;;     (yaml   . ("https://github.com/ikatyang/tree-sitter-yaml.git")))))

;; (use-package mwim
;;  :bind
;;  ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line-or-comment)
;;  ([remap move-end-of-line]       . mwim-end-of-code-or-line))

;; (use-package expand-region
;;  :bind
;;  ("C-=" . er/expand-region))

;; (use-package transient
;;  :custom
;;  (transient-default-level 7))

;; (use-package magit
;;  :autoload magit-after-save-refresh-status
;;  :bind
;;  ("C-x g" . magit-status)
;;  :custom
;;  (magit-log-section-commit-count 20)
;;  (magit-auto-revert-tracked-only nil)
;;  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
;;  (magit-bury-buffer-function 'magit-restore-window-configuration)
;;  (magit-repository-directories '(("~/Workspace" . 3)))
;;  :config
;;  (add-hook 'after-save-hook #'magit-after-save-refresh-status))

;; (use-package magit-diff
;;  :ensure nil
;;  :custom
;;  (magit-revision-show-gravatars t)
;;  (magit-revision-fill-summary-line fill-column)
;;  :config
;;  (defadvice magit-diff-visit-file
;;   (after recenter-after-magit-diff-visit-file activate)
;;   (recenter)))

;; (use-package diff-hl
;;  :hook (prog-mode conf-desktop-mode)
;;  :custom
;;  (diff-hl-draw-borders nil)
;;  (diff-hl-flydiff-delay 0.1)
;;  :hook
;;  (magit-pre-refresh . diff-hl-magit-pre-refresh)
;;  (magit-post-refresh . diff-hl-magit-post-refresh))

;; (use-package multiple-cursors
;;  :bind
;;  ("C->" . mc/mark-next-like-this)
;;  ("C-<" . mc/mark-previous-like-this))

;; (use-package multiple-cursors-core
;;  :ensure nil
;;  :custom
;;  (mc/always-run-for-all t))

;; (use-package volatile-highlights
;;  :commands volatile-highlights-mode
;;  :delight
;;  :init
;;  (volatile-highlights-mode))

;; (use-package yaml-mode
;;  :mode "\\clang-format\\'"
;;  :bind (:map yaml-mode-map ("C-c p" . fm/generate-password)))

;; (use-package hledger-mode
;;  :mode ("\\.journal\\'" "\\.ledger\\'")
;;  :custom
;;  (hledger-currency-string "EUR")
;;  (hledger-current-overlay t)
;;  (hledger-comments-column 1)
;;  :hook
;;  (hledger-mode . (lambda () (setq-local tab-width 1))))

;; (use-package buffer-move
;;  :bind
;;  ("C-x m" . buf-move))

;; (use-package deadgrep
;;  :bind
;;  ("M-F" . deadgrep)
;;  :config
;;  (require 'wgrep-deadgrep))

;; (use-package wgrep-deadgrep)

;; (use-package flycheck
;;  :commands flycheck-next-error flycheck-previous-error
;;  :hook
;;  (prog-mode yaml-mode hledger-mode)
;;  :custom
;;  (flycheck-checker-error-threshold nil)
;;  (flycheck-mode-line-prefix "Fc")
;;  (flycheck-idle-change-delay 0.2)
;;  (flycheck-idle-buffer-switch-delay 0.2)
;;  (flycheck-display-errors-delay 0.2)
;;  :config
;;  (defadvice flycheck-next-error
;;   (after recenter-after-flycheck-next activate)
;;   (recenter))
;;  (defadvice flycheck-previous-error
;;   (after recenter-after-flycheck-previous activate)
;;   (recenter))
;;  :bind
;;  (:map flycheck-mode-map
;;   ("M-n" . flycheck-next-error)
;;   ("M-p" . flycheck-previous-error)))

;; (use-package flycheck-posframe
;;  :custom
;;  (flycheck-posframe-position 'window-bottom-left-corner)
;;  (flycheck-posframe-border-width 1)
;;  :hook
;;  flycheck-mode)

;; (use-package flycheck-hledger
;;  :demand t
;;  :after (flycheck hledger-mode)
;;  :custom
;;  ;; TODO Also add "accounts"
;;  (flycheck-hledger-checks '("commodities")))

;; (use-package company
;;  :delight " Co"
;;  :hook (prog-mode yaml-mode hledger-mode systemd-mode)
;;  :custom
;;  (company-tooltip-align-annotations t)
;;  (company-tooltip-minimum-width 40)
;;  (company-tooltip-width-grow-only t)
;;  (company-keywords-ignore-case t)
;;  (company-idle-delay 0.5)
;;  :bind (:map company-mode-map ("<tab>" . company-indent-or-complete-common)))

;; (use-package company-posframe
;;  :delight
;;  :custom
;;  (company-posframe-quickhelp-x-offset 2)
;;  :hook company-mode)

;; (use-package company-prescient
;;  :hook company-mode
;;  :custom
;;  (company-prescient-sort-length-enable nil))

;; (use-package c-ts-mode
;;  :ensure nil
;;  :after lsp-mode
;;  :bind (:map c-ts-base-mode-map ("<f2>" . lsp-clangd-find-other-file)))

;; (use-package ivy
;;  :delight
;;  :bind (:map ivy-minibuffer-map ("<RET>" . ivy-alt-done))
;;  :custom
;;  (ivy-use-selectable-prompt t)
;;  (ivy-use-virtual-buffers t)
;;  (ivy-count-format "(%d/%d) ")
;;  (ivy-virtual-abbreviate 'abbreviate)
;;  (ivy-extra-directories nil)
;;  :init
;;  (ivy-mode))

;; (use-package ivy-rich
;;  :custom
;;  (ivy-rich-path-style 'abbrev)
;;  :init
;;  (ivy-rich-mode))

;; (use-package ivy-xref
;;  :demand
;;  :custom
;;  (xref-show-xrefs-function 'ivy-xref-show-xrefs))

;; (use-package nerd-icons-ivy-rich
;;  :init
;;  (nerd-icons-ivy-rich-mode))

;; (use-package ivy-prescient
;;  :init
;;  (ivy-prescient-mode))

;; (use-package counsel
;;  :delight
;;  :init
;;  (counsel-mode)
;;  :config
;;  (defadvice counsel-register
;;   (after recenter-after-counsel-register activate)
;;   (recenter)))

;; (use-package register
;;  :ensure nil
;;  :bind ([remap jump-to-register] . counsel-register))

;; (use-package swiper
;;  ;; :bind
;;  ;; ([remap isearch-forward] . swiper)
;;  ;; ([remap isearch-backward] . swiper-isearch-backward)
;;  :custom
;;  (swiper-include-line-number-in-search t)
;;  (swiper-action-recenter t))

;; (im/config ctrlf
;;  :package
;;  :custom
;;  (ctrlf-default-search-style 'fuzzy)
;;  (ctrlf-auto-recenter t)
;;  :init
;;  (ctrlf-mode))

;; (use-package projectile
;;  :delight " Pr"
;;  :bind (:map projectile-mode-map ("C-x p" . projectile-command-map))
;;  :custom
;;  (projectile-project-search-path '("~/Workspace"))
;;  (projectile-sort-order 'recently-active)
;;  (projectile-indexing-method 'hybrid))

;; (use-package counsel-projectile
;;  :custom
;;  (counsel-projectile-mode t)
;;  :config
;;  (defadvice counsel-projectile-git-grep
;;   (after recenter-after-counsel-projectile-git-grep activate)
;;   (recenter)))

;; (use-package treemacs-projectile)

;; (use-package flyspell-correct-ivy
;;  :bind (:map flyspell-mode-map ("C-M-;" . flyspell-correct-wrapper))
;;  :custom
;;  (flyspell-correct-interface #'flyspell-correct-ivy))

;; (use-package nerd-icons-completion
;;  :init
;;  (nerd-icons-completion-mode))

;; (use-package yasnippet
;;  :delight yas-minor-mode " Ys"
;;  :autoload yas-minor-mode-on
;;  :init
;;  (add-to-list 'yas-snippet-dirs "~/Workspace/dots/emacs/snippets")
;;  (defun init/start-ivy-yasnippet ()
;;   (interactive)
;;   (yas-minor-mode-on)
;;   (ivy-yasnippet))
;;  :bind
;;  ("C-c Y" . init/start-ivy-yasnippet)
;;  ("C-c y s" . yas-expand-from-trigger-key))

;; (use-package yasnippet-snippets)
;; (use-package ivy-yasnippet)

;; (use-package lsp-mode
;;  :delight " Ls"
;;  :hook
;;  ((python-ts-mode c-ts-mode c++-ts-mode c-or-c++-ts-mode) . lsp)
;;  (lsp-mode . lsp-enable-which-key-integration)
;;  :init
;;  (defun init/lsp-treemacs-call-hierarchy () (lsp-treemacs-call-hierarchy t))
;;  (defun init/lsp-treemacs-implementations () (lsp-treemacs-implementations t))
;;  (defun init/lsp-treemacs-references () (lsp-treemacs-references t))
;;  (defun init/lsp-treemacs-type-hierarchy () (lsp-treemacs-type-hierarchy 2))
;;  :bind
;;  (:map lsp-mode-map
;;   ([remap er/expand-region] . lsp-extend-selection)
;;   ("C-c r" . lsp-rename)
;;   ("C-c h" . lsp-describe-thing-at-point)
;;   ("M-<return>" . lsp-execute-code-action)
;;   ("C-c e" . lsp-treemacs-errors-list)
;;   ("C-c s" . lsp-treemacs-symbols)
;;   ("C-c c" . lsp-treemacs-call-hierarchy)
;;   ("C-c C" . init/lsp-treemacs-call-hierarchy)
;;   ("C-c i" . lsp-treemacs-implementations)
;;   ("C-c I" . lsp-treemacs-references)
;;   ("C-c t" . lsp-treemacs-type-hierarchy))
;;  :custom
;;  (lsp-semantic-tokens-enable t)
;;  (lsp-enable-relative-indentation t)
;;  (lsp-idle-delay 0.3)
;;  (lsp-use-plists))

;; (use-package lsp-treemacs
;;  :hook
;;  (lsp-mode . lsp-treemacs-sync-mode))

;; (use-package lsp-ui
;;  :bind
;;  (:map lsp-mode-map
;;   ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;   ([remap xref-find-references] . lsp-ui-peek-find-references)
;;   ("M-I" . lsp-ui-peek-find-implementation)))

;; ;; (fm/pkg lsp-ui
;; ;;  (fm/after lsp-ui-doc
;; ;;   (setq-default lsp-ui-doc-enable t)
;; ;;   (setq-default lsp-ui-doc-show-with-cursor nil)
;; ;;   (setq-default lsp-ui-doc-show-with-mouse t)
;; ;;   (setq-default lsp-ui-doc-alignment 'frame)
;; ;;   (setq-default lsp-ui-doc-header t)
;; ;;   (setq-default lsp-ui-doc-include-signature t)
;; ;;   (setq-default lsp-ui-doc-max-height 30)
;; ;;   (setq-default lsp-ui-doc-use-webkit t))
;; ;;  (fm/after lsp-ui-peek
;; ;;   (setq-default lsp-ui-peek-list-width 40)
;; ;;   (setq-default lsp-ui-peek-always-show t))
;; ;;  (fm/after lsp-ui-sideline
;; ;;   (setq-default lsp-ui-sideline-enable nil))
;; ;;  (fm/after lsp-ui
;; ;;   (fm/key-local "M-."   lsp-ui-peek-find-definitions    lsp-ui-mode-map "lsp-ui-peek")
;; ;;   (fm/key-local "M-?"   lsp-ui-peek-find-references     lsp-ui-mode-map "lsp-ui-peek")
;; ;;   (fm/key-local "M-I"   lsp-ui-peek-find-implementation lsp-ui-mode-map "lsp-ui-peek")
;; ;;   (fm/key-local "C-c d" lsp-ui-doc-show                 lsp-ui-mode-map "lsp-ui-doc")
;; ;;   ;; (fm/key-local "C-c l" lsp-ui-flycheck-list            lsp-ui-mode-map "lsp-ui-flycheck")))
;; ;;   ))

;; ;; (use-package lsp-clangd
;; ;;  :ensure lsp-mode
;; ;;  :config
;; ;;  (add-to-list 'lsp-clients-clangd-args "--all-scopes-completion")
;; ;;  (add-to-list 'lsp-clients-clangd-args "--clang-tidy")
;; ;;  (add-to-list 'lsp-clients-clangd-args "--completion-style=detailed")
;; ;;  (add-to-list 'lsp-clients-clangd-args "--header-insertion=iwyu")
;; ;;  (add-to-list 'lsp-clients-clangd-args "-j=8")
;; ;;  (add-to-list 'lsp-clients-clangd-args "--malloc-trim")
;; ;;  (add-to-list 'lsp-clients-clangd-args "--pch-storage=memory")
;; ;;  (add-to-list 'lsp-clients-clangd-args "--background-index")
;; ;;  (add-to-list 'lsp-clients-clangd-args "--function-arg-placeholders")
;; ;;  (add-to-list 'lsp-clients-clangd-args "--limit-references=0")
;; ;;  (add-to-list 'lsp-clients-clangd-args "--limit-results=0"))

;; (use-package treemacs
;;  :commands treemacs-load-theme
;;  :bind ("<f9>" . treemacs-select-window)
;;  :custom
;;  (treemacs-indentation 1)
;;  (treemacs-select-when-already-in-treemacs 'move-back)
;;  (treemacs-tag-follow-delay 0.1))

;; (use-package treemacs-interface
;;  :ensure treemacs
;;  :after treemacs
;;  :bind ("<f12>" . treemacs-delete-other-windows))

;; (use-package treemacs-async
;;  :ensure treemacs
;;  :commands treemacs-git-mode)

;; (use-package treemacs-mode
;;  :ensure treemacs
;;  :hook
;;  (treemacs-mode . treemacs-tag-follow-mode)
;;  (treemacs-mode . treemacs-fringe-indicator-mode)
;;  (treemacs-mode . treemacs-filewatch-mode)
;;  (treemacs-mode . treemacs-git-commit-diff-mode)
;;  (treemacs-mode . (lambda () (treemacs-git-mode 'deferred))))

;; (use-package treemacs-projectile
;;  :demand
;;  :after (treemacs projectile))

;; (use-package treemacs-magit
;;  :demand
;;  :after (treemacs magit))

;; (use-package treemacs-nerd-icons
;;  :demand
;;  :after treemacs
;;  :config
;;  (treemacs-load-theme "nerd-icons"))

;; (use-package lsp-ivy
;;  :after lsp-mode
;;  :bind
;;  (:map lsp-mode-map ("C-c x" . lsp-ivy-workspace-symbol)))

;; ;; (fm/pkg blamer
;; ;;  (fm/after blamer
;; ;;   (setq-default blamer-idle-time 0)
;; ;;   (setq-default blamer-commit-formatter ": %s")
;; ;;   (setq-default blamer-datetime-formatter "%s")
;; ;;   (setq-default blamer-max-commit-message-length 60))
;; ;;  (fm/after prog-mode
;; ;;   (fm/key-local "C-c b" blamer-mode prog-mode-map)))

;; ;; (fm/pkg sideline
;; ;;  (fm/after sideline
;; ;;   (fm/dim sideline-mode "Si")
;; ;;   (setq-default sideline-delay 0.1)))

;; ;; (fm/pkg sideline-blame
;; ;;  (fm/after sideline
;; ;;   (setq-default sideline-backends-right '(sideline-blame))
;; ;;   (setq-default sideline-blame-commit-format "- %s")))

;; ;; (fm/pkg scopeline
;; ;;  (fm/after tree-sitter
;; ;;   (fm/hook tree-sitter-mode-hook scopeline-mode))
;; ;;  (fm/after scopeline
;; ;;   (fm/dim scopeline-mode "Sl")
;; ;;   (setq-default scopeline-min-lines 10)))

;; ;; (fm/pkg rust-mode
;; ;;  (fm/after rust-mode
;; ;;   (fm/key-local "<f5>" rust-dbg-wrap-or-unwrap            rust-mode-map "rust-utils")
;; ;;   (fm/key-local "<f6>" lsp-rust-analyzer-expand-macro     rust-mode-map "lsp-rust")
;; ;;   (fm/key-local "<f7>" lsp-rust-analyzer-join-lines       rust-mode-map "lsp-rust")
;; ;;   (fm/key-local "<f8>" lsp-rust-analyzer-inlay-hints-mode rust-mode-map "lsp-rust")
;; ;;   (setq-default rust-indent-offset 2)
;; ;;   (setq-default rust-load-optional-libraries nil)
;; ;;   (setq-default rust-format-on-save t)
;; ;;   (fm/hookn rust-mode-hook (electric-quote-local-mode -1))
;; ;;   (fm/hook rust-mode-hook subword-mode)
;; ;;   (fm/hook rust-mode-hook lsp)))

;; ;; (fm/after rust-ts-mode
;; ;;  (fm/key-local "<f5>" rust-dbg-wrap-or-unwrap            rust-ts-mode-map "rust-utils")
;; ;;  (fm/key-local "<f6>" lsp-rust-analyzer-expand-macro     rust-ts-mode-map "lsp-rust")
;; ;;  (fm/key-local "<f7>" lsp-rust-analyzer-join-lines       rust-ts-mode-map "lsp-rust")
;; ;;  (fm/key-local "<f8>" lsp-rust-analyzer-inlay-hints-mode rust-ts-mode-map "lsp-rust")
;; ;;  (setq-default rust-ts-mode-indent-offset 2)
;; ;;  (fm/hookn rust-ts-mode-hook (electric-quote-local-mode -1))
;; ;;  (fm/hook rust-ts-mode-hook subword-mode)
;; ;;  (fm/hook rust-ts-mode-hook lsp))

;; ;; (fm/pkg lsp-mode
;; ;;  (fm/after lsp-mode
;; ;;   (fm/dim lsp-mode "Ls")
;; ;;   (fm/key-local "C-c f" lsp-format-buffer           lsp-mode-map "lsp-mode")
;; ;;   (fm/key-local "C-c g" lsp-format-region           lsp-mode-map "lsp-mode")
;; ;;   (fm/key-local "C-c r" lsp-rename                  lsp-mode-map "lsp-mode")
;; ;;   (fm/key-local "C-c h" lsp-describe-thing-at-point lsp-mode-map "lsp-mode")
;; ;;   (fm/key-local "C-="   lsp-extend-selection        lsp-mode-map "lsp-mode")
;; ;;   (fm/key-local "M-RET" lsp-execute-code-action     lsp-mode-map "lsp-mode")
;; ;;   (setq-default lsp-progress-prefix "  Progress: ")
;; ;;   (setq-default lsp-completion-show-detail t)
;; ;;   (setq-default lsp-completion-show-kind t)
;; ;;   (setq-default lsp-completion-provider :none)
;; ;;   (setq-default lsp-headerline-breadcrumb-enable t)
;; ;;   (setq-default lsp-restart 'auto-restart)
;; ;;   (setq-default lsp-enable-snippet t)
;; ;;   (setq-default lsp-keymap-prefix "C-c")
;; ;;   (setq-default lsp-idle-delay 0.1)
;; ;;   (setq-default lsp-file-watch-threshold nil)
;; ;;   (setq-default lsp-enable-semantic-highlighting t)
;; ;;   (setq-default lsp-enable-indentation t)
;; ;;   (setq-default lsp-enable-on-type-formatting nil)
;; ;;   (setq-default lsp-before-save-edits nil)
;; ;;   (setq-default lsp-auto-configure t)
;; ;;   (setq-default lsp-signature-auto-activate t)
;; ;;   (setq-default lsp-signature-render-documentation nil)
;; ;;   (setq-default lsp-eldoc-enable-hover t)
;; ;;   (setq-default lsp-eldoc-render-all nil)
;; ;;   (setq-default lsp-modeline-code-actions-enable nil)
;; ;;   (setq-default lsp-modeline-diagnostics-enable t)
;; ;;   (setq-default lsp-log-io nil)
;; ;;   (setq-default lsp-keep-workspace-alive nil)
;; ;;   (setq-default lsp-enable-imenu nil)
;; ;;   (fm/after which-key
;; ;;    (fm/hook lsp-mode-hook lsp-enable-which-key-integration "lsp-mode")))
;; ;;  (fm/after lsp-lens
;; ;;   (fm/dim lsp-lens-mode)
;; ;;   (setq-default lsp-lens-mode nil)
;; ;;   (setq-default lsp-lens-enable nil))
;; ;;  (fm/after lsp-headerline
;; ;;   (setq-default lsp-headerline-breadcrumb-icons-enable nil))
;; ;;  (fm/after lsp-semantic-tokens
;; ;;   (setq-default lsp-semantic-tokens-apply-modifiers t))
;; ;;  (fm/after lsp-rust
;; ;;   ;; (setq-default lsp-rust-analyzer-max-inlay-hint-length 50)
;; ;;   ;; (setq-default lsp-rust-unstable-features t)
;; ;;   (setq-default lsp-rust-analyzer-checkonsave-features "all")
;; ;;   (setq-default lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
;; ;;   (setq-default lsp-rust-analyzer-proc-macro-enable t)
;; ;;   (setq-default lsp-rust-racer-completion nil)
;; ;;   (setq-default lsp-rust-build-bin t)
;; ;;   (setq-default lsp-rust-build-lib t)
;; ;;   (setq-default lsp-rust-clippy-preference "on")
;; ;;   (setq-default lsp-rust-analyzer-server-display-inlay-hints t)
;; ;;   (setq-default lsp-rust-analyzer-display-chaining-hints t)
;; ;;   (setq-default lsp-rust-analyzer-display-parameter-hints t)
;; ;;   (setq-default lsp-rust-analyzer-display-closure-return-type-hints t)
;; ;;   (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-enable "always")
;; ;;   (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
;; ;;   (setq-default lsp-rust-analyzer-binding-mode-hints t)
;; ;;   (setq-default lsp-rust-analyzer-display-reborrow-hints "mutable")
;; ;;   (setq-default lsp-rust-all-features t)
;; ;;   (setq-default lsp-rust-all-targets t)
;; ;;   (setq-default lsp-rust-full-docs t)
;; ;;   (setq-default lsp-rust-analyzer-cargo-watch-command "clippy"))

;; ;; (fm/pkg lsp-ui
;; ;;  (fm/after lsp-ui-doc
;; ;;   (setq-default lsp-ui-doc-enable t)
;; ;;   (setq-default lsp-ui-doc-show-with-cursor nil)
;; ;;   (setq-default lsp-ui-doc-show-with-mouse t)
;; ;;   (setq-default lsp-ui-doc-alignment 'frame)
;; ;;   (setq-default lsp-ui-doc-header t)
;; ;;   (setq-default lsp-ui-doc-include-signature t)
;; ;;   (setq-default lsp-ui-doc-max-height 30)
;; ;;   (setq-default lsp-ui-doc-use-webkit t))
;; ;;  (fm/after lsp-ui-peek
;; ;;   (setq-default lsp-ui-peek-list-width 40)
;; ;;   (setq-default lsp-ui-peek-always-show t))
;; ;;  (fm/after lsp-ui-sideline
;; ;;   (setq-default lsp-ui-sideline-enable nil))
;; ;;  (fm/after lsp-ui
;; ;;   (fm/key-local "M-."   lsp-ui-peek-find-definitions    lsp-ui-mode-map "lsp-ui-peek")
;; ;;   (fm/key-local "M-?"   lsp-ui-peek-find-references     lsp-ui-mode-map "lsp-ui-peek")
;; ;;   (fm/key-local "M-I"   lsp-ui-peek-find-implementation lsp-ui-mode-map "lsp-ui-peek")
;; ;;   (fm/key-local "C-c d" lsp-ui-doc-show                 lsp-ui-mode-map "lsp-ui-doc")
;; ;;   ;; (fm/key-local "C-c l" lsp-ui-flycheck-list            lsp-ui-mode-map "lsp-ui-flycheck")))
;; ;;   ))

;; ;; (fm/pkg surround
;; ;;  (require 'surround)
;; ;;  (fm/key "M-'" surround-mark-inner)
;; ;;  (fm/key "M-\"" surround-insert))

;; Print startup stats.
(message "Startup in %s (%d GC runs)" (emacs-init-time) gcs-done)

(provide 'init)
;;; init.el ends here

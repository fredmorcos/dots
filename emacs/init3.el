;; (use-package projectile
;;  :delight " Pr"
;;  :bind (:map projectile-mode-map ("C-x p" . projectile-command-map))
;;  :custom
;;  (projectile-project-search-path '("~/Workspace"))
;;  (projectile-sort-order 'recently-active)
;;  (projectile-indexing-method 'hybrid))

;; (use-package treemacs-projectile)

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

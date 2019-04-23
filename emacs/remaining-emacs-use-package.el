;; 	avy
;; 	hledger-mode
;; 	rmsbolt
;; 	company-tabnine
;; 	lsp-mode lsp-ui company-lsp
;; 	toml-mode markdown-mode json-mode gnuplot-mode dockerfile-mode meson-mode
;; 	yaml-mode flycheck-yamllint
;; 	rust-mode
;; 	lsp-java
;; 	irony irony-eldoc flycheck-irony company-irony company-irony-c-headers
;; 	cquery ccls
;; 	z3-mode boogie-friends))

;; (defvar background-thread
;;   (make-thread
;;    (lambda ()
;;      (progn
;;        (custom-set-faces
;;         '(rust-question-mark-face ((t (:inherit (font-lock-builtin-face)))))
;;         '(lsp-ui-doc-background ((t (:background "white smoke"))))
;;         '(lsp-ui-sideline-code-action ((t (:foreground "orange"))))
;;         '(lsp-ui-sideline-current-symbol
;;           ((t (:height 0.99 :weight ultra-bold :box
;;                        (:line-width -1 :color "dim gray" :style nil)
;;                        :foreground "dim gray")))))

;; ;; lsp
;; (require 'lsp)
;; (require 'lsp-clients)
;; (require 'lsp-ui)

;; (define-key lsp-ui-mode-map
;;   [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;; (define-key lsp-ui-mode-map
;;   [remap xref-find-references] #'lsp-ui-peek-find-references)

;; ;; c/c++
;; (add-hook 'c++-mode-hook   #'irony-mode)
;; (add-hook 'c++-mode-hook   #'flycheck-mode)
;; (add-hook 'c++-mode-hook   #'company-mode)
;; (add-hook 'c-mode-hook     #'irony-mode)
;; (add-hook 'c-mode-hook     #'flycheck-mode)
;; (add-hook 'c-mode-hook     #'company-mode)
;; (add-hook
;;  'c-mode-hook
;;  #'(lambda ()
;;      (setq-default flycheck-cppcheck-standards '("c11" "posix"))))
;; (add-hook 'irony-mode-hook #'irony-eldoc)
;; (add-hook 'irony-mode-hook #'flycheck-irony-setup)
;; (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
;; (add-hook
;;  'irony-mode-hook
;;  #'(lambda ()
;;      (add-to-list 'company-backends #'company-irony)
;;      (add-to-list 'company-backends #'company-irony-c-headers)
;;      (setq-default flycheck-cppcheck-checks '("all"))
;;      (setq-default flycheck-cppcheck-suppressions '("missingIncludeSystem"))))
;; (with-eval-after-load 'flycheck
;;   (with-eval-after-load 'flycheck-irony
;;     (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
;;     (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
;;     (flycheck-add-next-checker 'irony '(t . c/c++-cppcheck))))

;; ;; rust
;; (add-hook 'rust-mode-hook #'lsp)

;; ;; java
;; (require 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp)

;; ;; customizations
;; (custom-set-variables
;;  '(rust-indent-method-chain t)
;;  '(rust-always-locate-project-on-open t)
;;  '(rust-indent-where-clause t)
;;  '(rust-format-on-save t)

;;  '(lsp-prefer-flymake nil)
;;  '(lsp-ui-doc-include-signature t)
;;  '(lsp-ui-doc-border "light salmon")
;;  '(lsp-ui-doc-position 'at-point)
;;  '(lsp-ui-doc-max-width 60)
;;  '(lsp-ui-flycheck-enable t)
;;  '(lsp-ui-sideline-ignore-duplicate t)
;;  '(lsp-ui-sideline-update-mode 'point)

(load "/usr/local/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)

(custom-set-variables
 '(auto-insert-mode nil)
 '(auto-save-timeout 10)
 '(c-backslash-column 70)
 '(c-backslash-max-column 80)
 '(c-block-comment-prefix "* ")
 '(c-cleanup-list
   (quote
    (brace-else-brace
     brace-elseif-brace
     brace-catch-brace
     empty-defun-braces
     one-liner-defun
     defun-close-semi
     list-close-comma
     scope-operator
     compact-empty-funcall)))
 '(c-default-style (quote ((other . "gnu"))))
 '(c-echo-syntactic-information-p t)
 '(c-max-one-liner-length 80)
 '(c-report-syntactic-errors t)
 '(colon-double-space t)
 '(column-number-mode t)
 '(compilation-always-kill t)
 '(default-justification (quote full))
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(fill-column 80)
 '(global-cwarn-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 ;; '(global-semantic-highlight-edits-mode t)
 ;; '(global-semantic-highlight-func-mode t)
 ;; '(global-semantic-show-parser-state-mode t)
 ;; '(global-semantic-show-unmatched-syntax-mode t)
 ;; '(global-semantic-stickyfunc-mode t)
 '(global-whitespace-mode t)
 '(history-delete-duplicates t)
 '(icomplete-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ispell-complete-word-dict "")
 '(ispell-dictionary "american")
 '(ispell-following-word t)
 '(ispell-have-new-look t)
 '(ispell-lazy-highlight nil)
 '(ispell-local-dictionary "american")
 '(lazy-highlight-initial-delay 0)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(minibuffer-auto-raise t)
 '(minibuffer-electric-default-mode t)
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(recentf-mode t)
 '(require-final-newline (quote visit-save))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(scroll-step 1000)
 ;; '(semantic-default-submodes
 ;;   (quote
 ;;    (global-semantic-highlight-func-mode
 ;;     global-semantic-stickyfunc-mode
 ;;     global-semantic-idle-completions-mode
 ;;     global-semantic-idle-scheduler-mode
 ;;     global-semanticdb-minor-mode
 ;;     global-semantic-idle-summary-mode
 ;;     global-semantic-show-unmatched-syntax-mode
 ;;     global-semantic-show-parser-state-mode)))
 ;; '(semantic-mode t)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(which-function-mode t)
 '(whitespace-action nil)
 '(whitespace-line-column nil)
 '(whitespace-style
   (quote
    (face
     tabs
     trailing
     lines-tail
     space-before-tab
     indentation
     empty
     space-after-tab
     tab-mark)))
 '(windmove-wrap-around t))
(custom-set-faces
 '(hl-line ((t (:background "snow2"))))
 '(show-paren-match ((t (:background "light steel blue")))))

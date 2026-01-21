;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
  (push "~/Workspace/dots/emacs/" load-path)
  (require 'init-macros))

(config "Quality of Life"
  (autoloads
    "qol"
    'qol/insert-pair
    'qol/insert-pair-curly
    'qol/insert-pair-parens
    'qol/insert-pair-quote
    'qol/insert-pair-double-quotes
    'qol/insert-pair-backtick
    'qol/generate-password
    'qol/insert-buffer-name
    'qol/get-trimmed-line-string
    'qol/replace-escapes))

(config "Mode Local Variables"
  (autoloads
    "mode-local"
    'setq-mode-local))

(config "Recentering Advice"
  (after 'emacs
    (eval-and-compile
      (defun init/recenter (&rest _)
        "A recentering function we can use as an advice."
        (recenter)))))

(config "Enable and Disable Various Functions"
  (after 'emacs
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
    (put 'diary            'disabled t)
    (put 'scroll-left      'disabled t)
    (put 'scroll-right     'disabled t)))

(config "Clean Configuration Files"
  (package 'no-littering
    (eval-and-compile
      (require 'no-littering))
    (no-littering-theme-backups)))

(config "Modeline"
  (package 'diminish
    (eval-and-compile
      (require 'diminish)))

  (after 'uniquify
    (setopt
      uniquify-buffer-name-style 'forward)))

(config "Moving in Text"
  (package 'move-text
    (move-text-default-bindings))

  (package 'mwim
    (bind-key [remap move-beginning-of-line] #'mwim-beginning-of-code-or-line-or-comment)
    (bind-key [remap move-end-of-line] #'mwim-end-of-code-or-line)))

(config "Selecting Text"
  (builtin 'files
    (cua-selection-mode t))

  (package 'expand-region
    (bind-key "C-=" #'er/expand-region))

  (package 'surround
    (bind-key "M-'" #'surround-mark-inner)
    (bind-key "M-\"" #'surround-insert)))

(config "Editing Text"
  (builtin 'misc
    (bind-key "C-c d" #'duplicate-dwim))

  (after 'files
    (setopt
      mode-require-final-newline 'visit-save
      require-final-newline 'visit-save
      ;; File contents.
      coding-system-for-read 'utf-8-unix
      coding-system-for-write 'utf-8-unix)))

(config "Filling Text"
  (package 'unfill
    (bind-key [remap fill-paragraph] #'unfill-toggle))

  (after 'emacs
    (setopt
      colon-double-space t
      default-justification 'left
      fill-column 90))

  (after 'newcomment
    (setopt
      comment-fill-column 80)))

(config "Indentation"
  (after 'indent
    (setopt
      tab-always-indent 'complete
      tab-first-completion 'word-or-paren-or-punct))

  (after 'simple
    (setopt
      indent-tabs-mode nil)))

(config "Window Movement"
  (builtin 'windmove
    (windmove-default-keybindings)
    (windmove-delete-default-keybindings)))

(config "Buffer Movement"
  (package 'buffer-move
    (bind-key "C-x m" #'buf-move)))

(config "Backups and autosaves"
  (after 'files
    (setopt
      auto-save-default t
      backup-inhibited nil
      make-backup-files t
      ;; Prefer the newest version of a file.
      load-prefer-newer t
      delete-old-versions t
      remote-file-name-inhibit-auto-save-visited t
      remote-file-name-inhibit-locks t)))

(config "Tramp"
  (after 'tramp-sh
    (setopt
      tramp-use-scp-direct-remote-copying t
      tramp-copy-size-limit (* 1024 1024)))

  (after 'files
    (setopt
      ;; Read .dir-locals.el over TRAMP.
      enable-remote-dir-locals t)))

(config "Warnings"
  (after 'warnings
    (setopt warning-minimum-level :emergency)
    (add-to-list 'warning-suppress-types 'defvaralias)))

(config "Help"
  (after 'help
    (setopt
      help-window-select t
      help-window-keep-selected t
      help-enable-completion-autoload nil
      help-enable-autoload nil
      help-enable-symbol-autoload nil))

  (after 'help-mode
    (advice-add 'help-button-action :after #'init/recenter)
    (advice-add 'help-function-def--button-function :after #'init/recenter))

  (package 'casual
    (after 'info
      (bind-key "C-p" #'casual-info-tmenu 'Info-mode-map))))

(config "User Interface"
  (after 'tooltip
    (setopt
      tooltip-use-echo-area t))

  (after 'display-line-numbers
    (setopt
      display-line-numbers-grow-only t
      display-line-numbers-width-start t)))

(config "User Experience"
  (after 'emacs
    (setopt
      delete-by-moving-to-trash t))

  (after 'files
    (advice-add 'find-file :after #'init/recenter)
    (advice-add 'find-file-literally :after #'init/recenter)
    (advice-add 'find-file-other-window :after #'init/recenter)
    (setopt
      confirm-kill-processes nil))

  (after 'button
    (advice-add 'push-button :after #'init/recenter))

  (after 'mouse
    (setopt
      mouse-yank-at-point t
      mouse-1-click-follows-link 'double))

  (after 'simple
    (advice-add 'goto-line :after #'init/recenter)
    (setopt
      ;; Hide commands in M-x that do not work in the current mode
      read-extended-command-predicate #'command-completion-default-include-p
      undo-limit (* 1024 1024)
      suggest-key-bindings 10
      save-interprogram-paste-before-kill t
      backward-delete-char-untabify-method 'hungry
      ;; Recenter after jump to next error.
      next-error-recenter '(4)
      next-error-message-highlight t
      completion-auto-select nil)))

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
 :preface (package 'nerd-icons))

(use-package casual-suite
 :ensure t
 :defer t
 :preface (package 'casual-suite))

(use-package casual-symbol-overlay
 :ensure casual-suite
 :defer t

 :config
 (symbol-overlay-mc-insert-into-casual-tmenu))

(use-package visual-replace
 :ensure t
 :defer t
 :preface (package 'visual-replace)

 :bind
 ("M-%" . visual-replace-thing-at-point)
 ("M-^" . visual-replace-selected)
 ("M-*" . visual-replace))

(use-package isearch
 :ensure nil
 :defer t

 :bind
 (:map isearch-mode-map ("C-p" . casual-isearch-tmenu))

 :custom
 (isearch-allow-motion t)
 (isearch-motion-changes-direction t))

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
 :preface (package 'symbol-overlay)

 :bind
 (:map symbol-overlay-map ("C-p" . casual-symbol-overlay-tmenu)))

(use-package symbol-overlay-mc
 :ensure t
 :defer t
 :preface (package 'symbol-overlay-mc)

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
 :preface (package 'consult)

 :preface
 (defun init/consult-grep-or-git-grep ()
  "Run grep in non-project buffers and git-grep in project buffers."
  (interactive)
  (if (and (fboundp 'projectile-project-root) (projectile-project-root))
   (consult-git-grep)
   (consult-grep)))

 :bind
 ([remap switch-to-buffer] . consult-buffer)
 ("M-Y" . consult-yank-pop)
 ([remap imenu] . consult-imenu)
 ("M-g I" . consult-imenu-multi)
 ("C-x S" . consult-line)
 ("M-G" . init/consult-grep-or-git-grep)
 ("M-D" . consult-fd)
 (:map consult-narrow-map
  ("C-?" . consult-narrow-help))

 :custom
 (consult-preview-key "M-.")
 (consult-project-function (lambda (_) (projectile-project-root)))

 :config
 (advice-add #'consult-buffer :before #'init/recentf-load-list))

(use-package consult-register
 :ensure consult
 :defer t
 :preface (package 'consult)
 :config
 (advice-add 'consult-register :after #'init/recenter))

(use-package consult-register
 :ensure consult
 :defer t
 :preface (package 'consult)
 :after register
 :bind
 ([remap jump-to-register] . consult-register)
 ([remap point-to-register] . consult-register-store))

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
   ((fboundp 'find-file-other-window)
    (call-interactively 'find-file-other-window))
   (t
    (error "Cannot find any more other-window functions to run"))))

 :bind
 ([remap other-window] . init/find-file-other-window))

(use-package embark-consult
 :ensure t
 :defer t
 :preface (package 'embark-consult))

(use-package consult-flycheck
 :ensure t
 :defer t
 :preface (package 'consult-flycheck)
 :after flycheck
 :bind (:map flycheck-mode-map ("C-c ! a" . consult-flycheck)))

(use-package vertico
 :ensure t
 :defer t
 :preface (package 'vertico)

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
  '(try-expand-dabbrev
    try-expand-dabbrev-visible
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

(use-package emacs
 :ensure nil
 :defer t
 :preface
 (defvar init/completion-system :corfu "Which completion system to use.")

 (defun init/buffer-completion-mode ()
  "Activate in-buffer completion system."
  (cond
   ((eq init/completion-system :company)
    (company-mode))
   ((eq init/completion-system :corfu)
    (corfu-mode))
   (t (error "init.el: Unknown completion system requested")))))

(use-package corfu
 :ensure t
 :defer t
 :preface (init/package 'corfu)
 :custom
 (corfu-preview-current nil)
 ;; (corfu-auto nil)
 ;; (corfu-auto-delay 0)
 ;; (corfu-quit-no-match t)
 (corfu-scroll-margin 5)
 ;; (corfu-max-width 50)
 (corfu-min-width 50)
 :config
 (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-popupinfo
 :ensure corfu
 :defer t
 :preface (init/package 'corfu)
 :after corfu
 :hook corfu-mode-hook)

(use-package corfu-popupinfo
 :ensure corfu
 :defer t
 :preface (init/package 'corfu)
 :custom
 (corfu-popupinfo-delay '(1.25 . 0.5)))

(use-package corfu-history
 :ensure corfu
 :defer t
 :preface (init/package 'corfu)
 :after corfu
 :hook corfu-mode-hook)

(use-package corfu-history
 :ensure corfu
 :defer t
 :preface (init/package 'corfu)
 :after savehist
 :config
 (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package nerd-icons-corfu
 :ensure t
 :defer t
 :preface (init/package 'nerd-icons-corfu))

(use-package company
 :ensure t
 :defer t
 :preface (init/package 'company)
 :diminish "Co"
 :commands company--active-p
 :hook prog-mode-hook

 :custom
 (company-idle-delay 0.7)
 (company-keywords-ignore-case t)
 (company-selection-wrap-around t)
 (company-tooltip-align-annotations t)
 (company-tooltip-minimum-width 40)
 (company-tooltip-maximum-width 80)
 (company-tooltip-limit 15)
 (company-tooltip-minimum 10)
 (company-tooltip-flip-when-above t)
 (company-tooltip-annotation-padding 3)
 (company-tooltip-width-grow-only t))

(use-package elisp-mode
 :ensure nil
 :defer t
 :config
 (setq-mode-local emacs-lisp-mode
  company-backends '(company-capf
                     company-keywords
                     company-dabbrev-code
                     company-files
                     :separate)))

(use-package company-posframe
 :ensure t
 :defer t
 :preface (init/package 'company-posframe)
 :diminish
 :after company
 :hook company-mode-hook
 :config
 (nconc company-posframe-show-params '(:border-width 1))
 (nconc company-posframe-quickhelp-show-params '(:border-width 1))
 :custom
 (company-posframe-quickhelp-x-offset 2))

(use-package cape
 :ensure t
 :defer t
 :preface (package 'cape)
 :bind ("C-c p" . cape-prefix-map)
 :config
 (advice-add 'cape-file :around #'cape-wrap-nonexclusive)
 (advice-add 'cape-dabbrev :around #'cape-wrap-nonexclusive))

;;; Syntax Checking

(use-package flycheck
 :ensure t
 :defer t
 :preface (package 'flycheck)

 :functions flycheck-overlay-errors-at

 :commands
 (flycheck-next-error
  flycheck-previous-error
  flycheck-add-next-checker)

 :preface
 (defun init/flycheck-eldoc (callback &rest _ignored)
  "Print flycheck messages at point by calling CALLBACK."
  (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
   (mapc
    (lambda (err)
     (funcall callback
      (format "%s: %s"
       (let ((level (flycheck-error-level err)))
        (pcase level
         ('info (propertize "I" 'face 'flycheck-error-list-info))
         ('error (propertize "E" 'face 'flycheck-error-list-error))
         ('warning (propertize "W" 'face 'flycheck-error-list-warning))
         (_ level)))
       (flycheck-error-message err))
      :thing (or
              (flycheck-error-id err)
              (flycheck-error-group err))
      :face 'font-lock-doc-face))
    flycheck-errors)))

 (defun init/setup-flycheck-eldoc ()
  (with-eval-after-load 'eldoc
   (add-hook 'eldoc-documentation-functions #'init/flycheck-eldoc nil t)))

 :hook
 (flycheck-mode-hook . init/setup-flycheck-eldoc)

 :bind
 (:map flycheck-mode-map
  ("M-n" . flycheck-next-error)
  ("M-p" . flycheck-previous-error))

 :custom
 (flycheck-checker-error-threshold nil)
 (flycheck-mode-line-prefix "Fc")
 (flycheck-check-syntax-automatically
  '(idle-change new-line mode-enabled idle-buffer-switch))
 (flycheck-idle-change-delay 0.1)
 (flycheck-idle-buffer-switch-delay 0.1)
 (flycheck-display-errors-delay 0.1)
 (flycheck-display-errors-function nil)
 (flycheck-help-echo-function nil)

 :config
 (advice-add 'flycheck-next-error :after #'init/recenter)
 (advice-add 'flycheck-previous-error :after #'init/recenter)
 (advice-add 'flycheck-error-list-goto-error :after #'init/recenter))

(use-package company
 :ensure t
 :defer t
 :preface (init/package 'company)
 :after (company flycheck-posframe)

 :preface
 (defun init/company-is-active (&rest _)
  (or (company--active-p) (bound-and-true-p company-backend)))

 :hook
 (flycheck-posframe-inhibit-functions . init/company-is-active))

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

 :custom
 (save-place-abbreviate-file-names t)

 :preface
 (defun init/activate-save-place-mode (&rest _)
  (unless save-place-mode (save-place-mode))))

(use-package files
 :ensure nil
 :defer t

 :config
 (advice-add 'find-file-noselect :before #'init/activate-save-place-mode))

(use-package savehist
 :ensure nil
 :defer t

 :init
 (savehist-mode))

(use-package recentf
 :ensure nil
 :defer t
 :commands (recentf-load-list)
 :config (recentf-mode)

 :preface
 (defvar init/recentf-loaded-p nil)
 (defun init/recentf-load-list (&rest _)
  (unless init/recentf-loaded-p
   (recentf-load-list)
   (setq init/recentf-loaded-p t)))

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
                    "/run/media")))

;;; Windows

(use-package winner
 :ensure nil
 :defer t

 :init
 (winner-mode))

(use-package window
 :ensure nil
 :defer t

 :custom
 (switch-to-buffer-in-dedicated-window 'pop)
 ;; (switch-to-buffer-obey-display-actions t)
 (split-height-threshold 160)
 (split-width-threshold 130)
 (even-window-sizes 'width-only)
 ;; Skip *SPECIALS* when switching buffers.
 (switch-to-prev-buffer-skip-regexp '("\\`\\*.+\\*\\'"))

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
 (advice-add 'previous-buffer :after #'init/recenter)
 (advice-add 'next-buffer :after #'init/recenter)
 (advice-add 'split-window-below :after #'init/recenter)
 (advice-add 'switch-to-buffer :after #'init/recenter)

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

(use-package prog-mode
 :ensure nil
 :defer t

 :preface
 (defun init/prog-capfs ()
  (let ((capfs (list (quote #'cape-file) (quote #'cape-dabbrev))))
   (when (bound-and-true-p yas-minor-mode)
    (push (quote #'yasnippet-capf) capfs))
   (when (bound-and-true-p lsp-mode)
    (push (quote #'lsp-completion-at-point) capfs))
   (apply 'cape-wrap-super capfs)))

 (defun init/setup-prog-capfs ()
  (setq-local completion-at-point-functions
   (list #'init/prog-capfs)))

 :hook
 (prog-mode-hook . init/setup-prog-capfs))

(use-package devdocs
 :ensure t
 :defer t
 :preface (package 'devdocs)

 :bind
 ("C-h D" . devdocs-lookup)

 :preface
 (defmacro init/devdocs-set (mode doc)
  `(setq-mode-local ,mode devdocs-current-docs '(,doc)))

 :config
 (init/devdocs-set python-mode "python~3.13")
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
 (eldoc-documentation-strategy 'eldoc-documentation-compose)
 (eldoc-idle-delay 0.1))

(use-package eldoc-mouse
 :ensure t
 :defer t
 :diminish "Em"
 :preface (init/package 'eldoc-mouse)
 :bind (:map eldoc-mouse-mode-map
        ("<f1> <f1>" . eldoc-mouse-pop-doc-at-cursor))
 :hook eldoc-mode-hook)

(use-package subword
 :ensure nil
 :defer t
 :diminish "Sw")

(use-package display-fill-column-indicator
 :ensure nil
 :defer t
 :preface (package 'display-fill-column-indicator)
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
 :preface (package 'diff-hl)
 :after prog-mode
 :hook prog-mode-hook)

(use-package eldoc
 :ensure nil
 :defer t
 :after prog-mode
 :hook prog-mode-hook)

(use-package eldoc
 :ensure nil
 :defer t
 :after flycheck
 :config
 (eldoc-add-command-completions "flycheck-")
 (eldoc-add-command-completions "mwim-"))

(use-package paren
 :ensure nil
 :defer t
 :after prog-mode
 :hook (prog-mode-hook . show-paren-mode))

(use-package flycheck
 :ensure t
 :defer t
 :preface (package 'flycheck)
 :after prog-mode
 :hook prog-mode-hook)

(use-package yasnippet
 :ensure t
 :defer t
 :preface (package 'yasnippet)
 :after prog-mode
 :hook (prog-mode-hook . yas-minor-mode-on))

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
 :preface (package 'jinx)
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
 :preface (package 'deadgrep)
 :after prog-mode

 :bind
 (:map prog-mode-map ("M-F" . deadgrep)))

(use-package wgrep
 :ensure t
 :defer t
 :preface (package 'wgrep))

(use-package wgrep-deadgrep
 :ensure t
 :defer t
 :preface (package 'wgrep-deadgrep)
 :after deadgrep)

(use-package sideline
 :ensure t
 :defer t
 :preface (package 'sideline)
 :diminish "Si")

(use-package sideline-blame
 :ensure t
 :defer t
 :preface (package 'sideline-blame)
 :after sideline

 :custom
 (sideline-backends-right '(sideline-blame))
 (sideline-blame-commit-format "- %s"))

(use-package xref
 :ensure nil
 :defer t
 :commands xref-push-marker-stack)

(use-package emacs
 :ensure nil
 :defer t
 :after xref
 :hook (xref-after-return-hook . recenter))

(use-package xref
 :ensure nil
 :defer t
 :preface (package 'consult)
 :custom
 (xref-show-xrefs-function #'consult-xref)
 (xref-show-definitions-function #'consult-xref))

(use-package editorconfig
 :ensure t
 :defer t
 :diminish "Ec"
 :preface (package 'editorconfig))

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
 (package 'meson-mode))

(use-package symbol-overlay
 :ensure t
 :defer t
 :after meson-mode
 :hook meson-mode-hook)

(use-package lsp-meson
 :ensure lsp-mode
 :defer t
 :preface (package 'lsp-mode)
 :after meson-mode

 :hook (meson-mode-hook . lsp)

 :custom
 (lsp-meson-server-executable '("mesonlsp" "--full")))

(use-package lsp-meson
 :ensure lsp-mode
 :defer t
 :preface (package 'lsp-mode)
 :after (meson-mode lsp-completion)

 :init
 (setq-mode-local meson-mode lsp-completion-mode nil)
 (setq-mode-local meson-mode lsp-completion-enable nil))

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
 :preface (package 'blamer)

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
 (magit-repository-directories '(("~/Workspace" . 3)))
 (magit-format-file-function #'magit-format-file-nerd-icons))

(use-package magit-status
 :ensure magit
 :defer t
 :after magit
 :config
 (add-hook 'magit-status-sections-hook 'magit-insert-worktrees t)
 (add-hook 'magit-status-sections-hook 'magit-insert-xref-buttons t)
 (add-hook 'magit-status-sections-hook 'magit-insert-local-branches t))

(use-package magit
 :ensure t
 :defer t
 :preface (package 'magit)
 :after files
 :commands magit-after-save-refresh-status

 :preface
 (defun init/magit-after-save-refresh-status ()
  "Refresh magit after save."
  (add-hook 'after-save-hook #'magit-after-save-refresh-status 0 t))

 :hook
 (magit-mode-hook . init/magit-after-save-refresh-status))

(use-package magit-diff
 :ensure magit
 :defer t
 :preface (package 'magit)
 :preface
 (defun init/magit-load-nerd-icons (&rest args)
  (if (require 'nerd-icons nil t)
   (apply args)
   (message "Installing the `nerd-icons' package...")
   (unless (package-installed-p 'nerd-icons)
    (unless package-archive-contents
     (package-refresh-contents))
    (package-install 'nerd-icons))))

 :custom
 (magit-revision-show-gravatars t)
 (magit-revision-fill-summary-line fill-column)

 :config
 (advice-add 'magit-diff-visit-file :after #'init/recenter)
 (advice-add 'magit-format-file-nerd-icons :around #'init/magit-load-nerd-icons))

(use-package diff-hl
 :ensure t
 :defer t
 :preface (package 'diff-hl)

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
 :preface (package 'speedrect)

 :init
 (speedrect-mode))

(use-package symbol-overlay
 :ensure t
 :defer t
 :preface (package 'symbol-overlay)
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

(use-package crux
 :ensure t
 :defer t
 :preface (package 'crux)

 :bind
 ([remap keyboard-quit] . crux-keyboard-quit-dwim))

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
 ;; (whitespace-style
 ;;  '(face
 ;;    trailing
 ;;    tabs
 ;;    spaces
 ;;    lines-tail
 ;;    missing-newline-at-eof
 ;;    empty
 ;;    space-after-tab
 ;;    space-before-tab
 ;;    tab-mark))
 (whitespace-style nil)

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
 (lisp-indent-offset 2)
 (lisp-indent-function #'common-lisp-indent-function)

 :preface
 (defun init/emacs-lisp-expand-current-macro-call ()
  "Expand the current macro expression."
  (interactive)
  (beginning-of-defun)
  (emacs-lisp-macroexpand))

 :bind
 (:map emacs-lisp-mode-map
  ("<f6>" . init/emacs-lisp-expand-current-macro-call))

 :config
 (advice-add 'elisp-completion-at-point :around #'cape-wrap-case-fold)
 (advice-add 'elisp-completion-at-point :around #'cape-wrap-nonexclusive)

 :preface
 (defun init/elisp-capfs ()
  (cape-wrap-super
   'elisp-completion-at-point
   #'yasnippet-capf
   #'cape-file
   #'cape-dabbrev))

 (defun init/setup-elisp-capfs ()
  (setq-local completion-at-point-functions
   (list #'init/elisp-capfs)))

 :hook
 (emacs-lisp-mode-hook . init/setup-elisp-capfs))

(use-package symbol-overlay
 :ensure t
 :defer t
 :preface (package 'symbol-overlay)
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
 :preface (package 'highlight-defined)
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package highlight-quoted
 :ensure t
 :defer t
 :preface (package 'highlight-quoted)
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package eros
 :ensure t
 :defer t
 :preface (package 'eros)
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package suggest
 :ensure t
 :defer t
 :preface (package 'suggest))

(use-package ipretty
 :ensure t
 :defer t
 :preface (package 'ipretty)
 :after elisp-mode
 :hook (emacs-lisp-mode-hook . (lambda () (ipretty-mode t))))

;;; Shell Scripting

(use-package sh-script
 :ensure nil
 :defer t
 :mode (rx bos ".bashrc.user" eos)

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
 :preface (package 'hl-line)
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
 :preface (package 'nerd-icons-dired)
 :diminish
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

;;; Markdown

(use-package markdown-mode
 :ensure t
 :defer t
 :preface (package 'markdown-mode)
 :config
 (setq-mode-local markdown-mode fill-column 79))

(use-package display-fill-column-indicator
 :ensure t
 :defer t
 :preface (package 'display-fill-column-indicator)
 :after markdown-mode
 :hook markdown-mode-hook)

(use-package hl-line
 :ensure t
 :defer t
 :preface (package 'hl-line)
 :after markdown-mode
 :hook markdown-mode-hook)

;;; Sed

(use-package sed-mode
 :ensure t
 :defer t
 :preface (package 'sed-mode))

;;; Po Translations

(use-package po-mode
 :ensure t
 :defer t
 :preface (package 'po-mode))

;;; TOML

(use-package toml-mode
 :ensure t
 :defer t
 :preface (package 'toml-mode))

(use-package eldoc-toml
 :ensure t
 :defer t
 :preface (package 'eldoc-toml)
 :diminish
 :after (eldoc toml-mode)
 :hook toml-mode-hook)

;;; JSON

  ;; TODO
  ;; Add a call to push this in JSON mode
  ;;  (major-mode-remap-alist
  ;; '((json-mode . json-ts-mode))))

(use-package json-mode
 :ensure t
 :defer t
 :preface (package 'json-mode))

(use-package indent-bars
 :ensure t
 :defer t
 :preface (package 'indent-bars)
 :after json-mode
 :hook json-mode-hook)

(use-package indent-bars
 :ensure t
 :defer t
 :preface (package 'indent-bars)
 :after json-ts-mode
 :hook json-ts-mode-hook)

(use-package tree-sitter
 :ensure t
 :defer t
 :preface (package 'tree-sitter)
 :diminish "Ts"
 :after json-mode
 :hook json-mode-hook)

;;; Spell Checking

(use-package jinx
 :ensure t
 :defer t
 :preface (package 'jinx)
 :diminish "Jx"

 :bind
 (:map jinx-mode-map
  ("M-$"   . jinx-correct)
  ("C-M-$" . jinx-languages)))

;;; Emacs Tools

(use-package speedrect
 :ensure t
 :demand
 :preface (package 'speedrect)
 :diminish "Sr"
 :config (speedrect-mode))

(use-package vundo
 :ensure t
 :defer t
 :preface (package 'vundo)
 :bind ("C-x u" . vundo)
 :custom
 (vundo-glyph-alist vundo-unicode-symbols))

(use-package register
 :ensure nil
 :defer t

 :custom
 (register-use-preview t))

(use-package which-key
 :ensure t
 :defer t
 :preface (package 'which-key)
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
 :preface (package 'nerd-icons-completion))

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
 :preface (package 'marginalia)

 :preface
 (defun init/marginalia-mode ()
  (unless (bound-and-true-p marginalia-mode)
   (marginalia-mode)))

 :hook
 (marginalia-mode-hook . nerd-icons-completion-marginalia-setup)
 (minibuffer-setup-hook . init/marginalia-mode))

(use-package embark
 :ensure t
 :defer t
 :preface (package 'embark)

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
 :preface (package 'hotfuzz)
 :after minibuffer

 :init
 (push 'hotfuzz completion-styles))

(use-package orderless
 :ensure t
 :defer t
 :preface (package 'orderless)

 :config
 (push 'orderless-initialism orderless-matching-styles)
 (push 'orderless-prefixes orderless-matching-styles))

(use-package orderless
 :ensure t
 :defer t
 :preface (package 'orderless)
 :after minibuffer

 :init
 (push 'orderless completion-styles))

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
 (delete 'tags-completion-at-point-function completion-at-point-functions)
 (delete 'emacs22 completion-styles)

 :custom
 (completion-category-defaults nil)
 (completion-category-overrides nil)
 ;; (minibuffer-electric-default-mode t)
 (minibuffer-message-clear-timeout 4)
 (completions-max-height 20)
 (read-file-name-completion-ignore-case t)
 (completions-format 'one-column)
 (completions-detailed t)
 (completions-group t)
 (completion-cycle-threshold nil))

(use-package map-ynp
 :ensure nil
 :defer t

 :custom
 (read-answer-short t))

(use-package ctrlf
 :ensure t
 :defer t
 :preface (package 'ctrlf)

 :custom
 (ctrlf-default-search-style 'fuzzy)
 (ctrlf-auto-recenter t)

 :init
 (ctrlf-mode))

(use-package transient
 :ensure t
 :defer t
 :preface (package 'transient)

 :custom
 (transient-default-level 7))

(use-package multiple-cursors
 :ensure t
 :defer t
 :preface (package 'multiple-cursors))

(use-package mc-edit-lines
 :ensure multiple-cursors
 :defer t

 :bind
 ("C-c C-v" . mc/edit-lines))

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
 :preface (package 'volatile-highlights)
 :diminish)

(use-package volatile-highlights
 :ensure volatile-highlights
 :defer t
 :preface (package 'volatile-highlights)
 :diminish
 :after hledger-mode

 :hook
 (hledger-mode-hook . volatile-highlights-mode))

(use-package volatile-highlights
 :ensure volatile-highlights
 :defer t
 :preface (package 'volatile-highlights)
 :diminish
 :after prog-mode

 :hook
 (prog-mode-hook . volatile-highlights-mode))

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
    (toml   . ("https://github.com/ikatyang/tree-sitter-toml.git"))
    (yaml   . ("https://github.com/ikatyang/tree-sitter-yaml.git")))))

(use-package tree-sitter
 :ensure t
 :defer t
 :preface (package 'tree-sitter)
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
 :preface (package 'tree-sitter-langs)

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
 :preface (package 'dape)

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
 :preface (package 'yaml-mode)

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

(use-package indent-bars
 :ensure t
 :defer t
 :preface (package 'indent-bars)
 :after yaml-mode
 :hook yaml-mode-hook)

;;; LLVM

(use-package llvm-ts-mode
 :ensure t
 :defer t
 :preface (package 'llvm-ts-mode)
 :mode (rx ".ll" eos))

(use-package demangle-mode
 :ensure t
 :defer t
 :preface (package 'demangle-mode)
 :after llvm-ts-mode
 :hook llvm-ts-mode-hook)

(use-package autodisass-llvm-bitcode
 :ensure t
 :defer t
 :preface (package 'autodisass-llvm-bitcode)
 :mode (rx ".bc" eos))

(use-package demangle-mode
 :ensure t
 :defer t
 :preface (package 'demangle-mode))

(use-package yaml-mode
 :ensure t
 :defer t
 :preface (package 'yaml-mode)

 :mode (rx ".clang-format" eos)
 :mode (rx ".clang-tidy" eos))

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
    (backward-delete-char-untabify-method . nil)
    (electric-indent-inhibit . nil)
    (lsp-enable-indentation . nil)
    (lsp-enable-on-type-formatting . nil)
    (lsp-enable-semantic-highlighting . nil))))

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
 (advice-add 'c-indent-line-or-region :after #'keyboard-quit))

(use-package editorconfig
 :ensure t
 :defer t
 :preface (package 'editorconfig)
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
 :preface (package 'lsp-mode)
 :after cc-mode
 :hook (c-mode-common-hook . lsp))

(use-package lsp-completion
 :ensure lsp-mode
 :defer t
 :preface (package 'lsp-mode)
 :config
 (advice-add #'lsp-completion-at-point :around #'cape-wrap-case-fold)
 (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive))

(use-package lsp-clangd
 :ensure lsp-mode
 :defer t
 :preface (package 'lsp-mode)

 :config
 (add-to-list 'lsp-clients-clangd-args "--enable-config")
 (add-to-list 'lsp-clients-clangd-args "--all-scopes-completion")
 (add-to-list 'lsp-clients-clangd-args "--query-driver=/**/*")
 (add-to-list 'lsp-clients-clangd-args "--all-scopes-completion")
 (add-to-list 'lsp-clients-clangd-args "--log=error")
 (add-to-list 'lsp-clients-clangd-args "--background-index")
 (add-to-list 'lsp-clients-clangd-args "--clang-tidy")
 (add-to-list 'lsp-clients-clangd-args "--completion-style=detailed")
 (add-to-list 'lsp-clients-clangd-args "--function-arg-placeholders")
 (add-to-list 'lsp-clients-clangd-args "--header-insertion=never")
 ;; This sometimes breaks LSP completion.
 (add-to-list 'lsp-clients-clangd-args "--header-insertion-decorators=0")
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

(use-package uv-mode
 :ensure t
 :defer t
 :preface (package 'uv-mode)
 :after python
 :hook (python-base-mode-hook . uv-mode-auto-activate-hook))

(use-package lsp-mode
 :ensure t
 :defer t
 :after python
 :hook (python-base-mode-hook . lsp))

(use-package lsp-pylsp
 :ensure lsp-mode
 :defer t
 :after (python lsp-mode)
 :custom
 (lsp-pylsp-plugins-autopep8-enabled t)
 (lsp-pylsp-plugins-black-enabled t)
 (lsp-pylsp-plugins-isort-enabled t)
 (lsp-pylsp-plugins-jedi-completion-fuzzy t)
 (lsp-pylsp-plugins-mypy-dmypy t)
 (lsp-pylsp-plugins-mypy-enabled t)
 (lsp-pylsp-plugins-mypy-report-progress t)
 (lsp-pylsp-plugins-pycodestyle-enabled t)
 (lsp-pylsp-plugins-pyflakes-enabled t)
 (lsp-pylsp-plugins-pylint-enabled t)
 (lsp-pylsp-plugins-rope-autoimport-code-actions-enabled t)
 (lsp-pylsp-plugins-rope-autoimport-completions-enabled t)
 (lsp-pylsp-plugins-rope-autoimport-enabled t)
 (lsp-pylsp-plugins-rope-completion-enabled t)
 (lsp-pylsp-plugins-ruff-enabled t)
 (lsp-pylsp-plugins-ruff-preview t)
 (lsp-pylsp-plugins-yapf-enabled t))

(use-package indent-bars
 :ensure t
 :defer t
 :preface (package 'indent-bars)
 :after python
 :hook python-base-mode-hook

 :custom
 (indent-bars-treesit-support t)
 (indent-bars-width-frac 0.1))

;;; Project Management

(use-package projectile
 :ensure t
 :defer t
 :preface (package 'projectile)
 :diminish "Pr"

 :commands
 projectile-project-root

 :bind-keymap ("C-x p" . projectile-command-map)

 :custom
 (projectile-project-search-path '(("~/Workspace" . 3)))
 (projectile-sort-order 'recently-active)
 (projectile-auto-cleanup-known-projects t)
 (projectile-enable-caching nil)
 (projectile-auto-discover t)
 ; (projectile-indexing-method 'hybrid)
 ; (projectile-require-project-root nil)

 :hook (after-init-hook . projectile-mode))

(use-package consult-projectile
 :ensure t
 :defer t
 :preface (package 'consult-projectile)
 :after projectile
 :bind ("C-x P" . consult-projectile))

(use-package treemacs-projectile
 :ensure t
 :defer t
 :preface (package 'treemacs-projectile)
 :after (treemacs projectile))

;;; Snippets

(use-package yasnippet
 :ensure t
 :defer t
 :preface (package 'yasnippet)
 :diminish (yas-minor-mode . "Ys")

 :init
 (add-to-list 'yas-snippet-dirs "~/Workspace/dots/emacs/snippets")

 :config
 (unbind-key "TAB" yas-minor-mode-map))

(use-package yasnippet-snippets
 :ensure t
 :defer t
 :preface (package 'yasnippet-snippets)
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

(use-package yasnippet-capf
 :ensure t
 :defer t
 :preface (package 'yasnippet-capf)
 :custom
 (yasnippet-capf-lookup-by 'name)
 :config
 (advice-add 'yasnippet-capf :around #'cape-wrap-nonexclusive))

;;; Ledger

(use-package hledger-mode
 :ensure t
 :defer t
 :preface (package 'hledger-mode)
 :mode (rx ".journal" eos)
 :mode (rx ".ledger" eos)
 :mode (rx ".hledger" eos)

 :preface
 (defvar init/hledger-currency-string "EUR")

 (defun init/hledger-move-amount-to-column ()
  "Move the amount or the point to the valid column."
  (interactive)
  (let ((amount-marker (concat " " init/hledger-currency-string " "))
        (original-pos (point)))
   (end-of-line)
   (when (search-backward amount-marker (pos-bol) t)
    (right-char))
   (let ((text (buffer-substring (point) (pos-eol)))
         (difference (- (current-column) 64)))
    (delete-region (point) (pos-eol))
    (if (> difference 0)
     (progn
      (left-char difference)
      (insert text))
     (progn
      (insert-char ?\s (abs difference))
      (insert text)))
    (unless (string-blank-p text)
     (goto-char original-pos)))))

 (defun init/hledger-find-next-unaligned ()
  "Find the next unaligned amount in a non-comment line."
  (interactive)
  (let ((amount-marker (concat " " init/hledger-currency-string " ")))
   (catch 'found
    (while (search-forward amount-marker nil t)
     (left-char (- (length amount-marker) 1))
     (when (not (or
                 (= (current-column) 64)
                 (looking-back ";.*" (line-beginning-position))))
      (throw 'found t))))))

 :bind
 (:map hledger-mode-map
  ("C-c >" . init/hledger-move-amount-to-column)
  ("C-c x" . init/hledger-find-next-unaligned)
  ("C-c +" . hledger-increment-entry-date))

 :custom
 (hledger-invalidate-completions '(on-save on-idle))
 (hledger-refresh-completions-idle-delay 5)
 (hledger-currency-string "")

 :config
 (setq-mode-local hledger-mode
  tab-width 1
  fill-column 100
  comment-fill-column 100)

 (advice-add 'hledger-completion-at-point :around #'cape-wrap-case-fold)
 (advice-add 'hledger-completion-at-point :around #'cape-wrap-nonexclusive)

 :preface
 (defun init/hledger-capfs ()
  ; 'hledger-completion-at-point
  (cape-wrap-super #'cape-dabbrev #'yasnippet-capf))

 (defun init/setup-hledger-capfs ()
  (setq-local completion-at-point-functions (list #'init/hledger-capfs)))

 :hook
 (hledger-mode-hook . init/setup-hledger-capfs)
 (hledger-mode-hook . init/buffer-completion-mode))

(use-package hledger-core
 :ensure hledger-mode
 :defer t
 :custom
 (hledger-currency-string "")
 (hledger-comments-column 1)
 (hledger-jfile "~/Documents/Expenses/Expenses.ledger"))

(use-package elec-pair
 :ensure nil
 :defer t
 :after hledger-mode
 :hook (hledger-mode-hook . electric-pair-local-mode))

(use-package company
 :ensure t
 :defer t
 :preface (package 'company)
 :config
 (setq-mode-local hledger-mode
  company-backends '((hledger-company
                      company-yasnippet))
  completion-at-point-functions nil))

(use-package flycheck-hledger
 :ensure t
 :defer t
 :preface (package 'flycheck-hledger)
 :after hledger-mode

 :hook
 (hledger-mode-hook .
  (lambda ()
   (require 'flycheck-hledger)
   ;; TODO Also add "accounts".
   (setopt flycheck-hledger-checks '("commodities")))))

(use-package whitespace
 :ensure nil
 :defer t
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package symbol-overlay
 :ensure t
 :defer t
 :preface (package 'symbol-overlay)
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package yasnippet
 :ensure t
 :defer t
 :preface (package 'yasnippet)
 :after hledger-mode
 :hook (hledger-mode-hook . yas-minor-mode-on))

(use-package flycheck
 :ensure t
 :defer t
 :preface (package 'flycheck)
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package display-fill-column-indicator
 :ensure t
 :defer t
 :preface (package 'display-fill-column-indicator)
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package hl-line
 :ensure t
 :defer t
 :preface (package 'hl-line)
 :after hledger-mode
 :hook hledger-mode-hook)

;;; Web Development

(use-package web-mode
 :ensure t
 :defer t
 :preface (package 'web-mode)
 :mode (rx ".html" eos)
 :mode (rx ".css" eos)
 :mode (rx ".js" eos)

 :custom
 (web-mode-markup-indent-offset 2)
 (web-mode-css-indent-offset 2)
 (web-mode-code-indent-offset 2)
 (web-mode-enable-current-column-highlight t)
 (web-mode-enable-current-element-highlight t)
 (web-mode-auto-close-style 3)
 (web-mode-enable-auto-expanding t)

 :config
 (setq-mode-local web-mode tab-width 2))

(use-package company
 :ensure t
 :defer t
 :preface (package 'company)
 :hook web-mode-hook)

(use-package company-web
 :ensure t
 :defer nil
 :preface (package 'company-web)
 :after (company web-mode)

 :config
 (setq-mode-local web-mode
  company-backends '((company-css
                      company-web-html))))

(use-package emmet-mode
 :ensure t
 :defer t
 :diminish "Em"
 :preface (package 'emmet-mode)
 :hook web-mode-hook

 :custom
 (emmet-indentation 2))

;;; Docker

(use-package dockerfile-mode
 :ensure t
 :defer t
 :preface (package 'dockerfile-mode))

(use-package docker-compose-mode
 :ensure t
 :defer t
 :preface (package 'docker-compose-mode))

(use-package docker
 :ensure t
 :defer t
 :preface (package 'docker)
 :bind ("C-c D" . docker))

;;; Archlinux PKGBUILDs

(use-package pkgbuild-mode
 :ensure t
 :defer t
 :preface (package 'pkgbuild-mode)
 :mode (rx bos "PKGBUILD" eos))

;;; Rust

(use-package rust-mode
 :ensure t
 :defer t
 :preface (init/package 'rust-mode)

 :bind
 (:map rust-mode-map
  ("<f5>" . rust-dbg-wrap-or-unwrap)
  ("<f6>" . lsp-rust-analyzer-expand-macro)
  ("<f7>" . lsp-rust-analyzer-join-lines))

 :custom
 (rust-indent-offset 2)
 (rust-load-optional-libraries nil)
 (rust-format-on-save t)

 :config
 (setq-mode-local rust-mode fill-column 110)

 :hook
 (rust-mode-hook . (lambda () (electric-quote-local-mode -1))))

(use-package rust-mode
 :ensure t
 :defer t
 :preface (init/package 'rust-mode)
 :after newcomment
 :config
 (setq-mode-local rust-mode comment-fill-column 100))

(use-package rust-mode
 :ensure t
 :defer t
 :preface (init/package 'rust-mode)
 :after corfu
 :config
 (setq-mode-local rust-mode
  corfu-auto t
  corfu-auto-delay 0.4
  corfu-auto-prefix 1))

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
 (lsp-rust-analyzer-closure-capture-hints t)
 (lsp-rust-analyzer-closure-return-type-hints "always")
 (lsp-rust-analyzer-discriminants-hints "fieldless")
 (lsp-rust-analyzer-expression-adjustment-hints "reborrow")
 (lsp-rust-analyzer-implicit-drops t)
 (lsp-rust-analyzer-display-reborrow-hints "mutable")
 (lsp-rust-all-features t)
 (lsp-rust-all-targets t)
 (lsp-rust-full-docs t)
 (lsp-rust-analyzer-cargo-watch-command "clippy"))

;;; LSP

(use-package lsp-mode
 :ensure t
 :defer t
 :preface (package 'lsp-mode)
 :diminish "Ls"

 :init
 ;; Improvements to LSP performance.
 (setenv "LSP_USE_PLISTS" "true")
 (setq-default read-process-output-max (* 10 1024 1024))

 :config
 ;; Unmark after formatting.
 (advice-add 'lsp-format-region :after #'keyboard-quit)

 :bind
 (:map lsp-mode-map
  ("C-c h" . lsp-describe-thing-at-point)
  ("M-RET" . lsp-execute-code-action)
  ("<f8>"  . lsp-inlay-hints-mode)
  ([remap er/expand-region] . lsp-extend-selection))

 :hook
 (lsp-mode-hook . (lambda () (setq-local lsp-enable-relative-indentation t)))
 (lsp-mode-hook . init/setup-prog-capfs)

 :custom
 (lsp-progress-prefix "  Progress: ")
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

(use-package lsp-completion
 :ensure lsp-mode
 :defer t
 :custom
 (lsp-completion-provider :none)
 (lsp-completion-show-detail t)
 (lsp-completion-show-kind t)
 :config
 (setq-mode-local rust-mode
  lsp-completion-mode nil
  lsp-completion-enable nil))

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

(use-package lsp-icons
 :ensure lsp-mode
 :defer t

 :custom
 (lsp-headerline-breadcrumb-icons-enable t))

(use-package lsp-headerline
 :ensure lsp-mode
 :defer t

 :custom
 (lsp-headerline-arrow "▶"))

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
 :preface (package 'lsp-ui)

 :bind
 (:map lsp-mode-map
  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
  ([remap xref-find-references] . lsp-ui-peek-find-references)
  ("M-I" . lsp-ui-peek-find-implementation)
  ("C-c D" . lsp-ui-doc-show)))

(use-package lsp-ui-imenu
 :ensure lsp-ui
 :defer t
 :preface (package 'lsp-ui)

 :custom
 (lsp-ui-imenu-auto-refresh t)
 (lsp-ui-imenu-auto-refresh-delay 0.1)
 (lsp-ui-imenu-buffer-position 'left)
 (lsp-ui-imenu-window-fix-width t))

(use-package consult-lsp
 :ensure t
 :defer t
 :preface (package 'consult-lsp)
 :after lsp-mode

 :bind
 (:map lsp-mode-map
  ("C-c ! d" . consult-lsp-diagnostics)
  ("C-c s s" . consult-lsp-symbols)))

(use-package lsp-ui-flycheck
 :ensure lsp-ui
 :defer t
 :preface (package 'lsp-ui)

 :bind
 (:map lsp-mode-map
  ("C-c ! L" . lsp-ui-flycheck-list)))

(use-package lsp-ui
 :ensure t
 :defer t
 :preface (package 'lsp-ui)

 :bind
 (:map lsp-mode-map
  ("C-c s S" . lsp-ui-find-workspace-symbol)))

(use-package lsp-ui-doc
 :ensure lsp-ui
 :defer t
 :preface (package 'lsp-ui)

 :custom
 (lsp-ui-doc-enable t)
 (lsp-ui-doc-show-with-cursor t)
 (lsp-ui-doc-show-with-mouse t)
 (lsp-ui-doc-alignment 'frame)
 (lsp-ui-doc-header t)
 (lsp-ui-doc-include-signature t)
 (lsp-ui-doc-max-height 30)
 (lsp-ui-doc-use-webkit t))

(use-package lsp-ui-peek
 :ensure lsp-ui
 :defer t
 :preface (package 'lsp-ui)

 :custom
 (lsp-ui-peek-list-width 40)
 (lsp-ui-peek-always-show t))

(use-package lsp-ui-sideline
 :ensure lsp-ui
 :defer t
 :preface (package 'lsp-ui)

 :custom
 (lsp-ui-sideline-enable nil))

;;; Treemacs

(use-package treemacs
 :ensure t
 :defer t
 :preface (package 'treemacs)

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
 :preface (package 'lsp-treemacs)
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
 :preface (package 'treemacs-magit)
 :after (treemacs magit))

(use-package treemacs-nerd-icons
 :ensure t
 :demand
 :preface (package 'treemacs-nerd-icons)
 :after treemacs

 :config
 (treemacs-load-theme "nerd-icons"))

;;; Sessions

(use-package easysession
 :ensure t
 :defer t
 :preface (package 'easysession)
 :custom
 (easysession-save-mode-lighter-show-session-name t)
 :bind
 ("C-c u" . easysession-save-as)
 ("C-c U" . easysession-switch-to))

;; Print startup stats.
(message "Startup in %s (%d GC runs that took %fs)" (emacs-init-time) gcs-done gc-elapsed)

(provide 'init)
;;; init.el ends here

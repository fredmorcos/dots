;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
 (push "~/Workspace/dots/emacs/" load-path)
 (require 'init-macros))

(config "Configuration options"
 (defvar *init/completion-system* :corfu "Which completion system to use."))

(config "Quality of Life"
 (autoload 'qol/insert-pair               "qol")
 (autoload 'qol/insert-pair-curly         "qol")
 (autoload 'qol/insert-pair-parens        "qol")
 (autoload 'qol/insert-pair-quote         "qol")
 (autoload 'qol/insert-pair-double-quotes "qol")
 (autoload 'qol/insert-pair-backtick      "qol")
 (autoload 'qol/get-trimmed-line-string   "qol")
 (autoload 'qol/insert-buffer-name        "qol" nil t)
 (autoload 'qol/replace-escapes           "qol" nil t)
 (autoload 'qol/generate-password         "qol" nil t))

(config "Mode Local Variables"
 (autoload 'setq-mode-local "mode-local"))

(config "Recentering Advice"
 (eval-and-compile
  (defun init/recenter (&rest _)
   "A recentering function we can use as an advice."
   (recenter))))

(config "Enable and Disable Various Functions"
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
 (put 'scroll-right     'disabled t))

(config "Clean Configuration Files"
 (packages 'no-littering)
 (eval-and-compile (require 'no-littering))
 (no-littering-theme-backups))

(config "Modeline"
 (packages 'diminish)
 (after 'uniquify (setopt uniquify-buffer-name-style 'forward)))

(config "Moving in Text"
 (packages 'move-text 'mwim)
 (move-text-default-bindings)
 (bind-key [remap move-beginning-of-line] #'mwim-beginning-of-code-or-line-or-comment)
 (bind-key [remap move-end-of-line] #'mwim-end-of-code-or-line))

(config "Selecting Text"
 (cua-selection-mode t)
 (packages 'expand-region 'surround)
 (bind-key "C-=" #'er/expand-region)
 (bind-key "M-'" #'surround-mark-inner)
 (bind-key "M-\"" #'surround-insert))

(config "Editing Text"
 (packages 'casual 'speedrect 'volatile-highlights 'multiple-cursors)

 (bind-key "C-p" #'casual-editkit-main-tmenu)
 (bind-key "C-c d" #'duplicate-dwim)

 (after 'volatile-highlights (diminish 'volatile-highlights-mode))

 (after 'files
  (setopt
   mode-require-final-newline 'visit-save
   require-final-newline 'visit-save
   ;; File contents.
   coding-system-for-read 'utf-8-unix
   coding-system-for-write 'utf-8-unix)

  (add-hook 'before-save-hook #'delete-trailing-whitespace))

 (after 'simple (setopt backward-delete-char-untabify-method 'hungry))

 (after 'speedrect (diminish 'speedrect-mode "Sr"))
 (speedrect-mode))

(config "Filling Text"
 (packages 'unfill)
 (after 'fill
  (bind-key [remap fill-paragraph] #'unfill-toggle))

 ;; fill.el
 (after 'emacs
  (setopt
   colon-double-space t
   default-justification 'left))

 (after 'emacs (setopt fill-column 90))
 (after 'newcomment (setopt comment-fill-column 80)))

(config "Undo & Redo"
 (packages 'vundo)
 (bind-key "C-x u" #'vundo)
 (after 'vundo
  (setopt vundo-glyph-alist vundo-unicode-symbols))
 (after 'emacs (setopt undo-limit (* 1024 1024))))

(config "Kill Ring"
 (after 'simple (setopt save-interprogram-paste-before-kill t)))

(config "Indentation"
 (after 'indent
  (setopt
   tab-always-indent 'complete
   tab-first-completion 'word-or-paren-or-punct))

 (after 'simple (setopt indent-tabs-mode nil)))

(config "Window Movement and Management"
 (windmove-default-keybindings)
 (windmove-swap-states-default-keybindings)
 (windmove-delete-default-keybindings)

 (autoload 'winner-undo "winner" nil t)
 (autoload 'winner-redo "winner" nil t)

 (after 'winner
  (unbind-key "C-c <left>" 'winner-mode-map)
  (unbind-key "C-c <right>" 'winner-mode-map)
  (bind-key "C-x w u" #'winner-undo 'winner-mode-map)
  (bind-key "C-x w r" #'winner-redo 'winner-mode-map))

 (winner-mode)

 (after 'window
  (setopt
   switch-to-buffer-in-dedicated-window 'pop
   ;; switch-to-buffer-obey-display-actions t
   split-height-threshold 160
   split-width-threshold 130
   even-window-sizes 'width-only
   ;; Skip *SPECIALS* when switching buffers.
   switch-to-prev-buffer-skip-regexp `(,(rx bos "*" (1+ nonl) "*" eos)))

  (advice-add 'split-window-below :after #'init/recenter))

 (bind-key "<f12>" #'delete-other-windows)

 (after 'emacs (setopt resize-mini-windows t)))

(config "Buffer Management"
 (packages 'buffer-move)
 (bind-key "C-x m" #'buf-move)

 (after 'ibuffer
  (bind-keys :map 'ibuffer-mode-map
   ("C-p" . casual-ibuffer-tmenu)
   ("F"   . casual-ibuffer-filter-tmenu)
   ("s"   . casual-ibuffer-sortby-tmenu)))

 (defun init/disable-popup (regexp)
  "Stop buffers that match REGEXP from popping up."
  (push `(,regexp
          (display-buffer-no-window)
          (allow-no-window . t))
   display-buffer-alist))

 (after 'window
  (init/disable-popup (rx bos "*Compile-Log*" (0+ nonl) eos))
  (init/disable-popup (rx bos "*Native-compile-Log*" (0+ nonl) eos))
  (init/disable-popup (rx bos "*Async-native-compile-log*" (0+ nonl) eos))
  (init/disable-popup (rx bos "*Warnings*" (0+ nonl) eos))
  (advice-add 'previous-buffer :after #'init/recenter)
  (advice-add 'next-buffer :after #'init/recenter)
  (advice-add 'switch-to-buffer :after #'init/recenter)))

(config "Backups and Autosaves"
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
  (push 'defvaralias warning-suppress-types))
 (setopt warning-minimum-level :emergency))

(config "Help"
 (packages 'casual 'casual-suite 'transient)

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

 (after 'info
  (bind-key "C-p" #'casual-info-tmenu 'Info-mode-map))

 (after 'transient (setopt transient-default-level 7))
 (after 'woman (setopt woman-fill-column 100)))

(config "User Interface"
 (packages 'nerd-icons)
 (after 'tooltip (setopt tooltip-use-echo-area t))
 (after 'display-line-numbers
  (setopt
   display-line-numbers-grow-only t
   display-line-numbers-width-start t)))

(config "User Experience"
 (after 'emacs (setopt delete-by-moving-to-trash t))
 (after 'files (setopt confirm-kill-processes nil))

 (after 'files
  (advice-add 'find-file :after #'init/recenter)
  (advice-add 'find-file-literally :after #'init/recenter)
  (advice-add 'find-file-other-window :after #'init/recenter))

 (after 'button (advice-add 'push-button :after #'init/recenter))
 (after 'simple (advice-add 'goto-line :after #'init/recenter))

 (after 'mouse
  (setopt
   mouse-yank-at-point t
   mouse-1-click-follows-link 'double))

 (after 'map-ynp (setopt read-answer-short t)))

(config "Search & Replace"
 (packages 'visual-replace 'casual 'ctrlf)

 (bind-key "M-%" #'visual-replace-thing-at-point)
 (bind-key "M-^" #'visual-replace-selected)
 (bind-key "M-*" #'visual-replace)

 (after 'isearch
  (setopt
   isearch-allow-motion t
   isearch-motion-changes-direction t
   isearch-lazy-count t
   isearch-lazy-highlight t
   lazy-count-prefix-format "(%s/%s) "
   search-whitespace-regexp ".*?"))

 (bind-key "C-p" #'casual-isearch-tmenu)

 (after 'ctrlf
  (setopt
   ctrlf-default-search-style 'fuzzy
   ctrlf-auto-recenter t))

 (ctrlf-mode))

(config "Regular Expressions"
 (after 're-builder
  (bind-keys :map 'reb-mode-map ("C-p" . casual-re-builder-tmenu))
  (bind-keys :map 'reb-lisp-mode-map ("C-p" . casual-re-builder-tmenu))))

(config "Symbol handling and Multiple Cursors"
 (packages 'symbol-overlay 'symbol-overlay-mc 'casual-symbol-overlay)
 (after 'symbol-overlay
  (bind-keys :map 'symbol-overlay-map ("C-p" . casual-symbol-overlay-tmenu))
  (bind-keys :map 'symbol-overlay-map ("M-a" . symbol-overlay-mc-mark-all))))

(config "File Management"
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
    (call-interactively 'other-window))))

 (after 'window
  (bind-key [remap other-window] #'init/find-file-other-window)))

(config "Scrolling"
 (defun init/scroll-other-window ()
  "Scroll up the other window in a split frame."
  (interactive)
  (scroll-other-window 1))
 (defun init/scroll-other-window-down ()
  "Scroll down the other window in a split frame."
  (interactive)
  (scroll-other-window-down 1))

 (after 'emacs
  (setopt
   scroll-conservatively 104
   scroll-margin 1
   hscroll-margin 1
   hscroll-step 1
   auto-hscroll-mode 'current-line
   fast-but-imprecise-scrolling t))

 (bind-key "C-<f11>" #'init/scroll-other-window)
 (bind-key "C-<f12>" #'init/scroll-other-window-down)
 (bind-key "<mouse-4>" #'previous-line)
 (bind-key "<mouse-5>" #'next-line))

(config "Dynamic Expansion"
 (after 'abbrev (diminish 'abbrev-mode "Ab"))

 (after 'dabbrev
  ;; Replace dabbrev-expand with hippie-expand
  (bind-key [remap dabbrev-expand] #'hippie-expand))

 (after 'hippie-expand
  (setopt
   hippie-expand-try-functions-list '(try-expand-dabbrev
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
                                      try-complete-lisp-symbol-partially))))

(config "Minibuffer"
 (packages 'hotfuzz 'orderless)

 (after 'minibuffer
  (delete 'tags-completion-at-point-function completion-at-point-functions)
  (delete 'emacs22 completion-styles)
  (push 'hotfuzz completion-styles)
  (push 'orderless completion-styles)
  (setopt
   completion-auto-help nil
   minibuffer-message-clear-timeout 4
   read-file-name-completion-ignore-case t
   completion-category-defaults nil
   completion-category-overrides nil
   completions-max-height 20
   completions-format 'one-column
   completions-detailed t
   completions-group t
   completion-cycle-threshold nil))

 (declvar orderless-matching-styles)
 (after 'orderless
  (push 'orderless-initialism orderless-matching-styles)
  (push 'orderless-prefixes orderless-matching-styles))

 (after 'simple
  (setopt
   completion-auto-select nil
   ;; Hide commands in M-x that do not work in the current mode
   read-extended-command-predicate #'command-completion-default-include-p
   suggest-key-bindings 10))

 (after 'emacs
  (setopt
   completion-ignore-case t
   read-buffer-completion-ignore-case t
   enable-recursive-minibuffers t)

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)))

(config "Minibuffer Completion"
 (packages 'vertico 'consult)

 (after 'vertico
  (bind-keys :map 'vertico-map
   ("M-RET" . minibuffer-force-complete-and-exit)
   ("M-TAB" . minibuffer-complete)
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char)
   ("M-DEL" . vertico-directory-delete-word)))

 (after 'vertico
  (setopt
   vertico-cycle t
   vertico-resize nil))

 (vertico-mode)

 (after 'rfn-eshadow
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

 (after 'window
  (bind-key [remap switch-to-buffer] #'consult-buffer))

 (after 'imenu
  (bind-key [remap imenu] #'consult-imenu))

 (after 'consult
  (bind-key "C-?" 'consult-narrow-help 'consult-narrow-map)
  (setopt
   consult-preview-key "M-."
   consult-project-function (lambda (_) (projectile-project-root))))

 (defun init/consult-grep-or-git-grep ()
  "Run grep in non-project buffers and git-grep in project buffers."
  (interactive)
  (if (and (fboundp 'projectile-project-root) (projectile-project-root))
   (consult-git-grep)
   (consult-grep)))

 (bind-key "M-Y" #'consult-yank-pop)
 (bind-key "M-g I" #'consult-imenu-multi)
 (bind-key "C-x S" #'consult-line)
 (bind-key "M-G" #'init/consult-grep-or-git-grep)
 (bind-key "M-D" #'consult-fd))

(config "Registers"
 (packages 'consult)

 (after 'consult-register
  (advice-add #'consult-register :after #'init/recenter))

 (after 'register
  (bind-key [remap jump-to-register] #'consult-register)
  (bind-key [remap point-to-register] #'consult-register-store)
  (setopt
   register-use-preview t)))

(config "Bookmarks"
 (after 'bookmark
  (bind-keys :map 'bookmark-bmenu-mode-map
   ("C-p" . casual-bookmarks-tmenu))))

(config "Embark"
 (packages 'embark 'embark-consult))

(config "In-buffer Completion"
 (defun init/buffer-completion-mode ()
  "Activate in-buffer completion system."
  (cond
   ((eq *init/completion-system* :company)
    (company-mode))
   ((eq *init/completion-system* :corfu)
    (corfu-mode))
   (t (error "init.el: Unknown completion system requested"))))

 (packages 'corfu 'nerd-icons-corfu 'company 'company-posframe 'cape)

 (config "Corfu"
  ;; Show icons in corfu popups.
  (after 'corfu
   (declvar corfu-margin-formatters)
   (push #'nerd-icons-corfu-formatter corfu-margin-formatters)
   (setopt
    corfu-preview-current nil
    ;; corfu-auto nil
    ;; corfu-auto-delay 0
    ;; corfu-quit-no-match t
    corfu-scroll-margin 5
    ;; corfu-max-width 50
    corfu-min-width 50)

   (add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
   (add-hook 'corfu-mode-hook #'corfu-history-mode))

  (after 'corfu-popupinfo (setopt corfu-popupinfo-delay '(1.25 . 0.5)))

  (after 'corfu-history
   (after 'savehist
    (defvar savehist-additional-variables)
    (push 'corfu-history savehist-additional-variables))))

 (config "Company"
  (defun init/company-is-active (&rest _)
   (declfun company--active-p 'company)
   (or (company--active-p) (bound-and-true-p company-backend)))

  (after 'company
   (diminish 'company-mode "Co")
   (setopt
    company-idle-delay 0.7
    company-keywords-ignore-case t
    company-selection-wrap-around t
    company-tooltip-align-annotations t
    company-tooltip-minimum-width 40
    company-tooltip-maximum-width 80
    company-tooltip-limit 15
    company-tooltip-minimum 10
    company-tooltip-flip-when-above t
    company-tooltip-annotation-padding 3
    company-tooltip-width-grow-only t)
   (add-hook 'company-mode-hook #'company-posframe-mode))

  (after 'company-posframe
   (diminish 'company-posframe-mode)
   (declvar company-posframe-show-params)
   (declvar company-posframe-quickhelp-show-params)
   (nconc company-posframe-show-params '(:border-width 1))
   (nconc company-posframe-quickhelp-show-params '(:border-width 1))
   (setopt company-posframe-quickhelp-x-offset 2)))

 (config "Cape"
  (bind-key "C-c p" #'cape-prefix-map)

  (after 'cape
   (advice-add 'cape-file :around #'cape-wrap-nonexclusive)
   (advice-add 'cape-dabbrev :around #'cape-wrap-nonexclusive))))

(config "Syntax Checkers and Error Lists"
 (packages 'flycheck 'consult-flycheck)

 (after 'simple
  (setopt
   ;; Recenter after jump to next error.
   next-error-recenter '(4)
   next-error-message-highlight t))

 (defface init/flycheck-errors-mode-line '((t :height 0.2)) "Flycheck errors modeline")
 (defface init/flycheck-errors-text '((t :height 0.8)) "Flycheck errors text")
 (defvar-local init/flycheck-errors-mode-line-cookie nil)
 (defvar-local init/flycheck-errors-text-cookie nil)

 (defun init/delete-ancillary-window (window)
  (unless (eq window (selected-window))
   (delete-window window)))

 (autoload 'face-remap-remove-relative "face-remap")

 (defun init/setup-flycheck-errors-window (window)
  (with-current-buffer (window-buffer window)
   (add-hook 'window-selection-change-functions #'init/delete-ancillary-window nil t))
  (with-selected-window window
   (face-remap-remove-relative init/flycheck-errors-mode-line-cookie)
   (face-remap-remove-relative init/flycheck-errors-text-cookie)
   (setq init/flycheck-errors-mode-line-cookie
    (face-remap-add-relative 'mode-line-active 'init/flycheck-errors-mode-line))
   (setq init/flycheck-errors-text-cookie
    (face-remap-add-relative 'default 'init/flycheck-errors-text))
   (bind-key [remap keyboard-quit]
    #'(lambda ()
       (interactive)
       (delete-window window))
    'flycheck-error-list-mode-map)))

 (after 'window
  (push `(,(rx bos "*Flycheck errors*" eos)
          (display-buffer-reuse-mode-window display-buffer-in-side-window)
          (mode . flycheck-errors-list-mode)
          (side . bottom)
          (slot . 0)
          (dedicated . t)
          (window-height . 0.15)
          (post-command-select-window . t)
          (window-parameters . ((mode-line-format . "")))
          (body-function . init/setup-flycheck-errors-window))
   display-buffer-alist))

 (after 'flycheck
  (bind-key "C-c ! L" #'consult-flycheck 'flycheck-mode-map)
  (bind-key "M-n" 'flycheck-next-error 'flycheck-mode-map)
  (bind-key "M-p" 'flycheck-previous-error 'flycheck-mode-map)
  (advice-add 'flycheck-next-error :after #'init/recenter)
  (advice-add 'flycheck-previous-error :after #'init/recenter)
  (advice-add 'flycheck-error-list-goto-error :after #'init/recenter)

  (setopt
   flycheck-help-echo-function nil
   flycheck-checker-error-threshold nil
   flycheck-mode-line-prefix "Fc"
   flycheck-check-syntax-automatically '(idle-change mode-enabled save new-line))))

(config "History"
 (after 'emacs
  (setopt
   history-delete-duplicates t
   history-length 150))

 (after 'saveplace (setopt save-place-abbreviate-file-names t))

 (defun init/activate-save-place-mode (&rest _)
  (unless save-place-mode (save-place-mode)))

 (after 'files
  (advice-add 'find-file-noselect :before #'init/activate-save-place-mode))

 (savehist-mode)

 (after 'recentf
  (setopt
   recentf-max-menu-items 50
   recentf-max-saved-items 100
   recentf-exclude `(,(no-littering-expand-var-file-name "")
                     ,(no-littering-expand-etc-file-name "")
                     ,@native-comp-eln-load-path
                     "~/.cache"
                     "~/.config/emacs/var"
                     "~/.config/emacs/elpa"
                     "/usr/share/emacs"
                     "/run/media")))

 (autoload 'recentf-load-list "recentf")
 (defvar init/recentf-loaded-p nil)
 (defun init/recentf-load-list (&rest _)
  (unless init/recentf-loaded-p
   (recentf-load-list)
   (setq init/recentf-loaded-p t)))

 (after 'consult
  (advice-add #'consult-buffer :before #'init/recentf-load-list))

 (after 'recentf (recentf-mode)))

(config "Session Management"
 (packages 'easysession)
 (after 'easysession (setopt easysession-save-mode-lighter-show-session-name t))
 (bind-key "C-c u" #'easysession-save)
 (bind-key "C-c U" #'easysession-load))

(config "Spell Checking"
 (packages 'jinx)
 (after 'jinx
  (diminish 'jinx-mode "Jx")
  (bind-key "M-$"   #'jinx-correct   'jinx-mode-map)
  (bind-key "C-M-$" #'jinx-languages 'jinx-mode-map)))

(config "Text"
 (packages 'jinx)
 (after 'text-mode
  (add-hook 'text-mode-hook #'jinx-mode)))

(config "Translation Files"
 (packages 'po-mode))

(config "Sed Files"
 (packages 'sed-mode))

(config "Markdown"
 (packages 'markdown-mode)
 (after 'markdown-mode
  (declvar markdown-mode)
  (setq-mode-local markdown-mode fill-column 79)
  (add-hook 'markdown-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'markdown-mode-hook #'hl-line-mode)))

(config "TOML"
 (packages 'toml-mode 'eldoc-toml)
 (after 'eldoc-toml (diminish 'eldoc-toml))
 (after 'toml-mode
  (add-hook 'toml-mode-hook #'eldoc-toml-mode)))

(config "YAML"
 (packages 'yaml-mode)
 (after 'yaml-mode
  (bind-key "C-c p" #'qol/generate-password 'yaml-mode-map)
  (add-hook 'yaml-mode-hook #'indent-bars-mode)
  (add-hook 'yaml-mode-hook #'tree-sitter-mode)
  (add-hook 'yaml-mode-hook #'flycheck-mode)))

(config "LLVM"
 (packages 'llvm-ts-mode 'demangle-mode 'autodisass-llvm-bitcode)
 (mode (rx ".ll" eos) 'llvm-ts-mode)
 (mode (rx ".bc" eos) 'autodisass-llvm-bitcode)
 (mode (rx bos ".clang-format") 'yaml-mode)
 (mode (rx bos ".clang-tidy") 'yaml-mode)
 (after 'llvm-ts-mode
  (add-hook 'llvm-ts-mode-hook #'demangle-mode)))

(config "Archlinux PKGBUILDs"
 (packages 'pkgbuild-mode)
 (mode (rx bos "PKGBUILD" eos) 'pkgbuild-mode))

(config "Docker"
 (packages 'dockerfile-mode 'docker-compose-mode 'docker)
 (bind-key "C-c D" #'docker))

(config "Web Development"
 (packages 'web-mode 'company 'company-web 'emmet-mode)

 (mode (rx ".html" eos) 'web-mode)
 (mode (rx ".css" eos) 'web-mode)
 (mode (rx ".js" eos) 'web-mode)

 (after 'web-mode
  (setopt
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-enable-current-column-highlight t
   web-mode-enable-current-element-highlight t
   web-mode-auto-close-style 3
   web-mode-enable-auto-expanding t)
  (declvar web-mode)
  (declvar company-backends)
  (setq-mode-local web-mode tab-width 2)
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'emmet-mode)
  (after 'company
   (setq-mode-local web-mode company-backends '((company-css company-web-html)))))

 (after 'emmet-mode
  (diminish 'emmet-mode "Em")
  (setopt emmet-indentation 2))

 (defun init/css-setup-comments ()
  "Setup C-style /* ... */ comments."
  (with-eval-after-load 'newcomment
   (setq-local comment-style 'extra-line)))

 (after 'css-mode
  (add-hook 'css-mode-hook #'init/css-setup-comments)))

(config "General Programming"
 (packages 'jinx 'devdocs 'editorconfig 'lsp-mode)

 (after 'eldoc
  (diminish 'eldoc-mode "Ed")
  (after 'flycheck (eldoc-add-command-completions "flycheck-"))
  (after 'mwim (eldoc-add-command-completions "mwim-"))
  (setopt
   eldoc-documentation-strategy 'eldoc-documentation-compose
   eldoc-idle-delay 0.1))

 (after 'editorconfig (diminish 'editorconfig "Ec"))

 (after 'prog-mode
  (add-hook 'prog-mode-hook #'eldoc-mode)
  (add-hook 'prog-mode-hook #'jinx-mode)
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
  (add-hook 'prog-mode-hook #'whitespace-mode)
  (add-hook 'prog-mode-hook #'editorconfig-mode)
  (add-hook 'prog-mode-hook #'volatile-highlights-mode)
  (bind-key "C-h D" #'devdocs-lookup))

 (declvar devdocs-current-docs)
 (declvar python-mode)
 (declvar rust-mode)
 (declvar c-mode)
 (declvar dockerfile-mode)
 (declvar emacs-lisp-mode)
 (declvar makefile-mode)
 (setq-mode-local python-mode devdocs-current-docs '("python~3.13"))
 (setq-mode-local rust-mode devdocs-current-docs "rust")
 (setq-mode-local c-mode devdocs-current-docs "c")
 (setq-mode-local dockerfile-mode devdocs-current-docs "docker")
 (setq-mode-local emacs-lisp-mode devdocs-current-docs "elisp")
 (setq-mode-local makefile-mode devdocs-current-docs "gnu_make")

 (after 'lsp-mode
  (setopt
   lsp-before-save-edits nil))

 (after 'lsp-semantic-tokens
  (setopt
   lsp-semantic-tokens-enable t))

 (after 'elec-pair
  (setopt
   electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
   electric-pair-preserve-balance nil)))

(config "Configuration Files"
 (after 'conf-mode
  (add-hook 'conf-mode-hook #'electric-pair-local-mode)
  (add-hook 'conf-desktop-mode-hook #'electric-pair-local-mode)
  (add-hook 'conf-mode-hook #'electric-layout-local-mode)
  (add-hook 'conf-desktop-mode-hook #'electric-layout-local-mode)))

(config "Emacs Lisp"
 (packages 'eros 'suggest 'ipretty 'highlight-quoted 'highlight-defined)

 (after 'elisp-mode
  (setopt
   lisp-indent-offset 1
   lisp-indent-function #'common-lisp-indent-function)
  (advice-add 'elisp-completion-at-point :around #'cape-wrap-case-fold)
  (advice-add 'elisp-completion-at-point :around #'cape-wrap-nonexclusive)
  (bind-key "<f6>" #'init/emacs-lisp-expand-current-macro-call 'emacs-lisp-mode-map)
  (add-hook 'emacs-lisp-mode-hook #'init/setup-elisp-capfs)
  (add-hook 'emacs-lisp-mode-hook #'init/buffer-completion-mode)
  (add-hook 'emacs-lisp-mode-hook #'eros-mode)
  (add-hook 'emacs-lisp-mode-hook #'(lambda () (ipretty-mode t)))
  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode)
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

 (defun init/emacs-lisp-expand-current-macro-call ()
  "Expand the current macro expression."
  (interactive)
  (beginning-of-defun)
  (emacs-lisp-macroexpand))

 (defun init/elisp-capfs ()
  (cape-wrap-super
   'elisp-completion-at-point
   #'yasnippet-capf
   #'cape-file
   #'cape-dabbrev))

 (defun init/setup-elisp-capfs ()
  (setq-local completion-at-point-functions
   (list #'init/elisp-capfs)))

 (after 'company
  (setq-mode-local emacs-lisp-mode
   company-backends '((company-capf
                       company-yasnippet
                       company-keywords
                       company-dabbrev-code
                       company-files)))))

(config "HLedger"
 (packages 'hledger-mode 'flycheck-hledger)

 (mode (rx ".journal" eos) 'hledger-mode)
 (mode (rx ".ledger" eos) 'hledger-mode)
 (mode (rx ".hledger" eos) 'hledger-mode)

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

 (after 'hledger-mode
  (bind-key "C-c >" #'init/hledger-move-amount-to-column 'hledger-mode-map)
  (bind-key "C-c x" #'init/hledger-find-next-unaligned 'hledger-mode-map)
  (bind-key "C-c +" 'hledger-increment-entry-date 'hledger-mode-map)
  (setopt
   hledger-invalidate-completions '(on-save on-idle)
   hledger-refresh-completions-idle-delay 5
   hledger-currency-string "")
  (declvar hledger-mode)
  (setq-mode-local hledger-mode
   tab-width 1
   fill-column 100
   comment-fill-column 100)
  (advice-add 'hledger-completion-at-point :around #'cape-wrap-case-fold)
  (advice-add 'hledger-completion-at-point :around #'cape-wrap-nonexclusive)
  (add-hook 'hledger-mode-hook #'flycheck-mode)
  (add-hook 'hledger-mode-hook #'init/hledger-setup-flycheck)
  (add-hook 'hledger-mode-hook #'init/buffer-completion-mode)
  (add-hook 'hledger-mode-hook #'init/setup-hledger-capfs)
  (add-hook 'hledger-mode-hook #'init/hledger-maybe-setup-company nil t)
  (add-hook 'hledger-mode-hook #'volatile-highlights-mode)
  (add-hook 'hledger-mode-hook #'electric-pair-local-mode))

 (defun init/hledger-setup-flycheck ()
  (require 'flycheck-hledger))

 (defun init/hledger-setup-company ()
  (setq-mode-local hledger-mode
   company-backends '((hledger-company company-yasnippet))
   completion-at-point-functions nil))

 (defun init/hledger-maybe-setup-company ()
  (after 'company
   (add-hook 'company-mode-hook #'init/hledger-setup-company nil t)))

 (defun init/hledger-capfs ()
  (cape-wrap-super 'hledger-completion-at-point #'cape-dabbrev #'yasnippet-capf))

 (defun init/setup-hledger-capfs ()
  (setq-local completion-at-point-functions (list #'init/hledger-capfs)))

 (after 'hledger-core
  (setopt
   hledger-currency-string ""
   hledger-comments-column 1
   hledger-jfile "~/Documents/Expenses/Expenses.ledger"))

 (after 'flycheck-hledger (setopt flycheck-hledger-checks '("commodities"))))

;;; General Programming

;; (use-package prog-mode
;;  :ensure nil
;;  :defer t

;;  :preface
;;  (defun init/prog-capfs ()
;;   (let ((capfs (list (quote #'cape-file) (quote #'cape-dabbrev))))
;;    (when (bound-and-true-p yas-minor-mode)
;;     (push (quote #'yasnippet-capf) capfs))
;;    (when (bound-and-true-p lsp-mode)
;;     (push (quote #'lsp-completion-at-point) capfs))
;;    (apply 'cape-wrap-super capfs)))

;;  (defun init/setup-prog-capfs ()
;;   (setq-local completion-at-point-functions
;;    (list #'init/prog-capfs)))

;;  :hook
;;  (prog-mode-hook . init/setup-prog-capfs))

(use-package subword
 :ensure nil
 :defer t
 :diminish "Sw")

(use-package diff-hl
 :ensure t
 :defer t
 :preface (packages 'diff-hl)
 :after prog-mode
 :hook prog-mode-hook)

(use-package paren
 :ensure nil
 :defer t
 :after prog-mode
 :hook (prog-mode-hook . show-paren-mode))

(use-package yasnippet
 :ensure t
 :defer t
 :preface (packages 'yasnippet)
 :after prog-mode
 :hook (prog-mode-hook . yas-minor-mode-on))

(use-package display-line-numbers
 :ensure nil
 :defer t
 :after prog-mode
 :hook prog-mode-hook)

;; TODO enable hl-line-mode & display-line-numbers-mode & whitespace-mode in yaml-mode toml-mode json-mode hledger-mode

(use-package deadgrep
 :ensure t
 :defer t
 :preface (packages 'deadgrep)
 :after prog-mode

 :bind
 (:map prog-mode-map ("M-F" . deadgrep)))

(use-package wgrep
 :ensure t
 :defer t
 :preface (packages 'wgrep))

(use-package wgrep-deadgrep
 :ensure t
 :defer t
 :preface (packages 'wgrep-deadgrep)
 :after deadgrep)

(use-package sideline
 :ensure t
 :defer t
 :preface (packages 'sideline)
 :diminish "Si")

(use-package sideline-blame
 :ensure t
 :defer t
 :preface (packages 'sideline-blame)
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
 :preface (packages 'consult)
 :custom
 (xref-show-xrefs-function #'consult-xref)
 (xref-show-definitions-function #'consult-xref))

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

 :preface (packages 'meson-mode))

(use-package symbol-overlay
 :ensure t
 :defer t
 :after meson-mode
 :hook meson-mode-hook)

(use-package lsp-meson
 :ensure lsp-mode
 :defer t
 :preface (packages 'lsp-mode)
 :after meson-mode

 :hook (meson-mode-hook . lsp)

 :custom
 (lsp-meson-server-executable '("mesonlsp" "--full")))

(use-package lsp-meson
 :ensure lsp-mode
 :defer t
 :preface (packages 'lsp-mode)
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
 :preface (packages 'blamer)

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
 :preface (packages 'magit)
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
 :preface (packages 'magit)
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
 :preface (packages 'diff-hl)

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

(use-package symbol-overlay
 :ensure t
 :defer t
 :preface (packages 'symbol-overlay)
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
 :preface (packages 'crux)

 :bind
 ([remap keyboard-quit] . crux-keyboard-quit-dwim))

;;; Various

(use-package dictionary
 :ensure nil
 :defer t

 :custom
 (dictionary-server "dict.org")
 (dictionary-use-single-buffer t))

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

(use-package symbol-overlay
 :ensure t
 :defer t
 :preface (packages 'symbol-overlay)
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

(use-package whitespace
 :ensure nil
 :defer t
 :after elisp-mode
 :hook emacs-lisp-mode-hook)

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
 :preface (packages 'hl-line)
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
 :preface (packages 'nerd-icons-dired)
 :diminish
 :hook (dired-mode-hook . nerd-icons-dired-mode))

;;; JSON

  ;; TODO
  ;; Add a call to push this in JSON mode
  ;;  (major-mode-remap-alist
  ;; '((json-mode . json-ts-mode))))

(use-package json-mode
 :ensure t
 :defer t
 :preface (packages 'json-mode))

(use-package indent-bars
 :ensure t
 :defer t
 :preface (packages 'indent-bars)
 :after json-ts-mode
 :hook json-ts-mode-hook)

(use-package tree-sitter
 :ensure t
 :defer t
 :preface (packages 'tree-sitter)
 :diminish "Ts"
 :after json-mode
 :hook json-mode-hook)

;;; Emacs Tools

(use-package which-key
 :ensure t
 :defer t
 :preface (packages 'which-key)
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
 :preface (packages 'nerd-icons-completion))

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
 :preface (packages 'marginalia)

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
 :preface (packages 'embark)

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

(use-package multiple-cursors
 :ensure t
 :defer t
 :preface (packages 'multiple-cursors))

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
 :preface (packages 'tree-sitter)
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
 :preface (packages 'tree-sitter-langs)

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
 :preface (packages 'dape)

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
 :preface (packages 'lsp-mode)
 :after cc-mode
 :hook (c-mode-common-hook . lsp))

;; (use-package lsp-completion
;;  :ensure lsp-mode
;;  :defer t
;;  :preface (packages 'lsp-mode)
;;  :config
;;  (advice-add #'lsp-completion-at-point :around #'cape-wrap-case-fold)
;;  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive))

(use-package lsp-clangd
 :ensure lsp-mode
 :defer t
 :preface (packages 'lsp-mode)

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
 :preface (packages 'uv-mode)
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
 :preface (packages 'indent-bars)
 :after python
 :hook python-base-mode-hook

 :custom
 (indent-bars-treesit-support t)
 (indent-bars-width-frac 0.1))

;;; Project Management

(use-package projectile
 :ensure t
 :defer t
 :preface (packages 'projectile)
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
 :preface (packages 'consult-projectile)
 :after projectile
 :bind ("C-x P" . consult-projectile))

(use-package treemacs-projectile
 :ensure t
 :defer t
 :preface (packages 'treemacs-projectile)
 :after (treemacs projectile))

;;; Snippets

(use-package yasnippet
 :ensure t
 :defer t
 :preface (packages 'yasnippet)
 :diminish (yas-minor-mode . "Ys")

 :init
 (add-to-list 'yas-snippet-dirs "~/Workspace/dots/emacs/snippets")

 :config
 (unbind-key "TAB" yas-minor-mode-map))

(use-package yasnippet-snippets
 :ensure t
 :defer t
 :preface (packages 'yasnippet-snippets)
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
 :preface (packages 'yasnippet-capf)
 :custom
 (yasnippet-capf-lookup-by 'name)
 :config
 (advice-add 'yasnippet-capf :around #'cape-wrap-nonexclusive))

;;; Ledger

(use-package whitespace
 :ensure nil
 :defer t
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package symbol-overlay
 :ensure t
 :defer t
 :preface (packages 'symbol-overlay)
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package yasnippet
 :ensure t
 :defer t
 :preface (packages 'yasnippet)
 :after hledger-mode
 :hook (hledger-mode-hook . yas-minor-mode-on))

(use-package display-fill-column-indicator
 :ensure t
 :defer t
 :preface (packages 'display-fill-column-indicator)
 :after hledger-mode
 :hook hledger-mode-hook)

(use-package hl-line
 :ensure t
 :defer t
 :preface (packages 'hl-line)
 :after hledger-mode
 :hook hledger-mode-hook)

;;; Rust

(config "Rust Programming"
 (packages 'rust-mode)
 (packages 'lsp-mode)

 (autoload 'lsp-rust-analyzer-expand-macro "lsp-rust")
 (autoload 'lsp-rust-analyzer-join-lines   "lsp-rust")

 (after 'rust-mode
  (setopt
   rust-indent-offset 2
   rust-load-optional-libraries nil
   rust-format-on-save t)

  (hook-progn 'rust-mode-hook (electric-quote-local-mode -1))
  (hook-globals
   'rust-mode-hook
   #'electric-pair-local-mode
   #'init/buffer-completion-mode
   #'lsp
   #'subword-mode)

  (declvar rust-mode-map)
  (bind-keys
   :map rust-mode-map
   ("<f5>" . rust-dbg-wrap-or-unwrap)
   ("<f6>" . lsp-rust-analyzer-expand-macro)
   ("<f7>" . lsp-rust-analyzer-join-lines))

  (after 'emacs
   (setq-mode-local rust-mode fill-column 110))

  (after 'newcomment
   (setq-mode-local rust-mode comment-fill-column 100))

  (after 'company
   (setq-mode-local rust-mode
    company-backends '((company-capf
                        company-yasnippet
                        company-files))))

  (after 'corfu
   (setq-mode-local rust-mode
    corfu-auto t
    corfu-auto-delay 0.4
    corfu-auto-prefix 1
    corfu-auto-trigger ".&"))

  (after 'lsp-rust
   (setq-mode-local rust-mode
    ;; lsp-rust-analyzer-max-inlay-hint-length 50
    ;; lsp-rust-unstable-features t
    lsp-rust-analyzer-cargo-run-build-scripts t
    lsp-rust-analyzer-checkonsave-features "all"
    lsp-rust-analyzer-cargo-load-out-dirs-from-check t
    lsp-rust-analyzer-proc-macro-enable t
    lsp-rust-racer-completion nil
    lsp-rust-build-bin t
    lsp-rust-build-lib t
    lsp-rust-clippy-preference "on"
    lsp-rust-analyzer-display-chaining-hints t
    lsp-rust-analyzer-display-parameter-hints t
    lsp-rust-analyzer-display-closure-return-type-hints t
    lsp-rust-analyzer-display-lifetime-elision-hints-enable "always"
    lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t
    lsp-rust-analyzer-binding-mode-hints t
    lsp-rust-analyzer-closure-capture-hints t
    lsp-rust-analyzer-closure-return-type-hints "always"
    lsp-rust-analyzer-discriminants-hints "fieldless"
    lsp-rust-analyzer-expression-adjustment-hints "reborrow"
    lsp-rust-analyzer-implicit-drops t
    lsp-rust-analyzer-display-reborrow-hints "mutable"
    lsp-rust-all-features t
    lsp-rust-all-targets t
    lsp-rust-full-docs t
    lsp-rust-analyzer-cargo-watch-command "clippy"))

  (after 'lsp-mode
   (setq-mode-local rust-mode
    lsp-format-buffer-on-save t))))

;;; LSP

(config "LSP Mode Settings"
 (packages 'lsp-mode)

 (after 'lsp-mode
  (setopt
   ;; lsp-signature-auto-activate t
   lsp-signature-render-documentation t
   lsp-eldoc-render-all t
   lsp-eldoc-enable-hover t)

 (after 'lsp-completion
  (setopt
   ;; Use company-capf in case of company, otherwise corfu will take care of things.
   lsp-completion-provider (if (eq *init/completion-system* :corfu) :none :capf)
   lsp-completion-show-detail t
   lsp-completion-show-kind t))))

(use-package lsp-mode
 :ensure t
 :defer t
 :preface (packages 'lsp-mode)
 :diminish "Ls"

 :config
 ;; Unmark after formatting.
 (advice-add 'lsp-format-region :after #'keyboard-quit)

 :bind
 (:map lsp-mode-map
  ("C-c h" . lsp-describe-thing-at-point)
  ("M-RET" . lsp-execute-code-action)
  ("<f8>"  . lsp-inlay-hints-mode)
  ([remap er/expand-region] . lsp-extend-selection))

 ;; :preface
 ;; (defun init/lsp-capfs ()
 ;;  (cape-wrap-super
 ;;   #'cape-file
 ;;   #'cape-dabbrev
 ;;   #'yasnippet-capf
 ;;   #'lsp-completion-at-point))

 ;; (defun init/setup-lsp-capfs ()
 ;;  (setq-local completion-at-point-functions
 ;;   (list #'init/lsp-capfs)))

 :hook
 (lsp-mode-hook . (lambda () (setq-local lsp-enable-relative-indentation t)))
 ;; (lsp-mode-hook . init/setup-lsp-capfs)

 :custom
 (lsp-progress-prefix "  Progress: ")
 (lsp-headerline-breadcrumb-enable t)
 (lsp-restart 'auto-restart)
 (lsp-enable-snippet t)
 (lsp-keymap-prefix "C-c")
 (lsp-idle-delay 0.1)
 (lsp-file-watch-threshold nil)
 (lsp-enable-indentation t)
 (lsp-auto-configure t)
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
 :preface (packages 'lsp-ui)

 :bind
 (:map lsp-mode-map
  ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
  ([remap xref-find-references] . lsp-ui-peek-find-references)
  ("M-I" . lsp-ui-peek-find-implementation)
  ("C-c D" . lsp-ui-doc-show)))

(use-package lsp-ui-imenu
 :ensure lsp-ui
 :defer t
 :preface (packages 'lsp-ui)

 :custom
 (lsp-ui-imenu-auto-refresh t)
 (lsp-ui-imenu-auto-refresh-delay 0.1)
 (lsp-ui-imenu-buffer-position 'left)
 (lsp-ui-imenu-window-fix-width t))

(use-package consult-lsp
 :ensure t
 :defer t
 :preface (packages 'consult-lsp)
 :after lsp-mode

 :bind
 (:map lsp-mode-map
  ("C-c ! d" . consult-lsp-diagnostics)
  ("C-c s s" . consult-lsp-symbols)))

(use-package lsp-ui-flycheck
 :ensure lsp-ui
 :defer t
 :preface (packages 'lsp-ui)

 :bind
 (:map lsp-mode-map
  ("C-c ! L" . lsp-ui-flycheck-list)))

(use-package lsp-ui
 :ensure t
 :defer t
 :preface (packages 'lsp-ui)

 :bind
 (:map lsp-mode-map
  ("C-c s S" . lsp-ui-find-workspace-symbol)))

(use-package lsp-ui-doc
 :ensure lsp-ui
 :defer t
 :preface (packages 'lsp-ui)

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
 :preface (packages 'lsp-ui)

 :custom
 (lsp-ui-peek-list-width 40)
 (lsp-ui-peek-always-show t))

(use-package lsp-ui-sideline
 :ensure lsp-ui
 :defer t
 :preface (packages 'lsp-ui)

 :custom
 (lsp-ui-sideline-enable nil))

;;; Treemacs

(use-package treemacs
 :ensure t
 :defer t
 :preface (packages 'treemacs)

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
 :preface (packages 'lsp-treemacs)
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
 :preface (packages 'treemacs-magit)
 :after (treemacs magit))

(use-package treemacs-nerd-icons
 :ensure t
 :demand
 :preface (packages 'treemacs-nerd-icons)
 :after treemacs

 :config
 (treemacs-load-theme "nerd-icons"))

;; Print startup stats.
(message "Startup in %s (%d GC runs that took %fs)" (emacs-init-time) gcs-done gc-elapsed)

(provide 'init)
;;; init.el ends here

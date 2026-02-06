;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-and-compile
 (push "~/Workspace/dots/emacs/" load-path)
 (require 'init-macros))

(config "Configuration options"
 (defvar *init/completion-system* :corfu "Which completion system to use."))

(config "Quality of Life"
 (package 'crux)
 (define-key global-map [remap keyboard-quit] #'crux-keyboard-quit-dwim)

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
 (package 'no-littering)
 (eval-and-compile (require 'no-littering))
 (no-littering-theme-backups))

(config "Dictionary"
 (after 'dictionary
  (setopt
   dictionary-server "dict.org"
   dictionary-use-single-buffer t)))

(config "Navigating Text"
 (package 'move-text)
 (move-text-default-bindings)

 (package 'mwim)
 (define-key global-map
  [remap move-beginning-of-line] #'mwim-beginning-of-code-or-line-or-comment)
 (define-key global-map
  [remap move-end-of-line] #'mwim-end-of-code-or-line)

 (after 'paren
  (setopt
   show-paren-when-point-in-periphery t
   show-paren-when-point-inside-paren t
   show-paren-style 'mixed
   show-paren-highlight-openparen t
   show-paren-context-when-offscreen 'overlay)))

(config "Grepping"
 (package 'deadgrep)
 (package 'wgrep)
 (package 'wgrep-deadgrep)
 (define-key global-map (kbd "M-F") #'deadgrep))

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

 (define-key global-map (kbd "C-<f11>") #'init/scroll-other-window)
 (define-key global-map (kbd "C-<f12>") #'init/scroll-other-window-down)
 (define-key global-map (kbd "<mouse-4>") #'previous-line)
 (define-key global-map (kbd "<mouse-5>") #'next-line))

(config "Selecting Text"
 (cua-selection-mode t)

 (package 'expand-region)
 (define-key global-map (kbd "C-=") #'er/expand-region)

 (package 'surround)
 (define-key global-map (kbd "M-'") #'surround-mark-inner)
 (define-key global-map (kbd "M-\"") #'surround-insert))

(config "Editing Text"
 (after 'text-mode
  (add-hook 'text-mode-hook #'jinx-mode))

 (define-key global-map (kbd "C-p") #'casual-editkit-main-tmenu)
 (define-key global-map (kbd "C-c d") #'duplicate-dwim)

 (package 'volatile-highlights)
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

 (package 'speedrect)
 (after 'speedrect (diminish 'speedrect-mode "Sr"))
 (speedrect-mode)

 (after 'whitespace
  (diminish 'whitespace-mode "Ws")
  (setopt
   whitespace-line-column fill-column
   show-trailing-whitespace nil
   whitespace-action '(cleanup auto-cleanup)
   whitespace-style nil)
  (face whitespace-tab :foreground "lavender" :background "white smoke"))

 (after 'autorevert
  (diminish 'autorevert-mode "Ar")
  (setopt
   auto-revert-mode-text " Ar"
   auto-revert-interval 1
   auto-revert-avoid-polling t
   buffer-auto-revert-by-notification t)))

(config "Search & Replace"
 (package 'visual-replace)
 (define-key global-map (kbd "M-%") #'visual-replace-thing-at-point)
 (define-key global-map (kbd "M-^") #'visual-replace-selected)
 (define-key global-map (kbd "M-*") #'visual-replace)

 (after 'isearch
  (setopt
   isearch-allow-motion t
   isearch-motion-changes-direction t
   isearch-lazy-count t
   isearch-lazy-highlight t
   lazy-count-prefix-format "(%s/%s) "
   search-whitespace-regexp ".*?"))

 (define-key global-map (kbd "C-p") #'casual-isearch-tmenu)

 (package 'ctrlf)
 (after 'ctrlf
  (setopt
   ctrlf-default-search-style 'fuzzy
   ctrlf-auto-recenter t))
 (ctrlf-mode))

(config "Undo & Redo"
 (package 'vundo)
 (define-key global-map (kbd "C-x u") #'vundo)
 (after 'vundo
  (setopt vundo-glyph-alist vundo-unicode-symbols))
 (after 'emacs (setopt undo-limit (* 1024 1024))))

(config "Filling Text"
 (package 'unfill)
 (after 'fill
  (define-key global-map [remap fill-paragraph] #'unfill-toggle))

 ;; fill.el
 (after 'emacs
  (setopt
   colon-double-space t
   default-justification 'left))

 (after 'emacs (setopt fill-column 90))
 (after 'newcomment (setopt comment-fill-column 80)))

(config "Capitalizing"
 (after 'emacs
  (define-key global-map [remap capitalize-word] #'capitalize-dwim)
  (define-key global-map [remap downcase-word] #'downcase-dwim)
  (define-key global-map [remap upcase-word] #'upcase-dwim)))

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
  (declvar winner-mode-map)
  (define-key winner-mode-map (kbd "C-c <left>") nil t)
  (define-key winner-mode-map (kbd "C-c <right>") nil t)
  (define-key winner-mode-map (kbd "C-x w u") #'winner-undo)
  (define-key winner-mode-map (kbd "C-x w r") #'winner-redo))

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

 (define-key global-map (kbd "<f12>") #'delete-other-windows)

 (after 'emacs (setopt resize-mini-windows t)))

(config "Buffer Management"
 (package 'buffer-move)
 (define-key global-map (kbd "C-x m") #'buf-move)

 (after 'ibuffer
  (declvar ibuffer-mode-map)
  (define-key ibuffer-mode-map (kbd "C-p") #'casual-ibuffer-tmenu)
  (define-key ibuffer-mode-map (kbd "F") #'casual-ibuffer-filter-tmenu)
  (define-key ibuffer-mode-map (kbd "s") #'casual-ibuffer-sortby-tmenu))

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
 (package 'casual)
 (package 'casual-suite)

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
  (define-key Info-mode-map (kbd "C-p") #'casual-info-tmenu))

 (package 'transient)
 (after 'transient (setopt transient-default-level 7))
 (after 'woman (setopt woman-fill-column 100)))

(config "User Interface"
 (package 'nerd-icons)
 (after 'tooltip (setopt tooltip-use-echo-area t))
 (after 'display-line-numbers
  (setopt
   display-line-numbers-grow-only t
   display-line-numbers-width-start t)))

(config "Modeline"
 (package 'diminish)
 (after 'uniquify (setopt uniquify-buffer-name-style 'forward)))

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

(config "Regular Expressions"
 (after 're-builder
  (declvar reb-mode-map)
  (declvar reb-lisp-mode-map)
  (define-key reb-mode-map (kbd "C-p") #'casual-re-builder-tmenu)
  (define-key reb-lisp-mode-map (kbd "C-p") #'casual-re-builder-tmenu)))

(config "Symbol handling and Multiple Cursors"
 (package 'symbol-overlay)
 (package 'symbol-overlay-mc)
 (package 'casual-symbol-overlay)
 (after 'symbol-overlay
  (diminish 'symbol-overlay-mode "So")
  (setopt symbol-overlay-idle-time 0.1)
  (declvar symbol-overlay-mode-map)
  (define-key symbol-overlay-mode-map (kbd "C-p") #'casual-symbol-overlay-tmenu)
  (define-key symbol-overlay-mode-map (kbd "M-a") #'symbol-overlay-mc-mark-all)
  (define-key symbol-overlay-mode-map (kbd "M->") #'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-<") #'symbol-overlay-jump-prev))

 (package 'multiple-cursors)
 (define-key global-map (kbd "C-c C-v") #'mc/edit-lines)
 (define-key global-map (kbd "C->") #'mc/mark-next-like-this)
 (define-key global-map (kbd "C-<") #'mc/mark-previous-like-this)
 (define-key global-map (kbd "C-S-<mouse-1>") #'mc/toggle-cursor-on-click)
 (after 'multiple-cursors-core (setopt mc/always-run-for-all t)))

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
  (define-key global-map [remap other-window] #'init/find-file-other-window)))

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

(config "Dynamic Expansion"
 (after 'abbrev (diminish 'abbrev-mode "Ab"))

 (after 'dabbrev
  ;; Replace dabbrev-expand with hippie-expand
  (define-key global-map [remap dabbrev-expand] #'hippie-expand))

 (after 'hippie-exp
  (setopt
   hippie-expand-try-functions-list '(try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-expand-line-all-buffers
                                      try-expand-dabbrev-visible
                                      try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol
                                      try-expand-list-all-buffers))))

(config "Minibuffer"
 (package 'hotfuzz)
 (package 'orderless)
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

 (after 'orderless
  (declvar orderless-matching-styles)
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

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

 (package 'marginalia)
 (package 'nerd-icons-completion)
 (after 'marginalia
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

 (defun init/marginalia-mode ()
  (unless (bound-and-true-p marginalia-mode)
   (marginalia-mode)))

 (after 'emacs
  (add-hook 'minibuffer-setup-hook #'init/marginalia-mode)
  (define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle))
 (after 'simple
  (define-key completion-list-mode-map (kbd "M-A") #'marginalia-cycle)))

(config "Minibuffer Completion"
 (package 'vertico)
 (after 'vertico
  (declvar vertico-map)
  (define-key vertico-map "M-RET" #'minibuffer-force-complete-and-exit)
  (define-key vertico-map "M-TAB" #'minibuffer-complete)
  (define-key vertico-map "RET" #'vertico-directory-enter)
  (define-key vertico-map "DEL" #'vertico-directory-delete-char)
  (define-key vertico-map "M-DEL" #'vertico-directory-delete-word)
  (setopt
   vertico-cycle t
   vertico-resize nil))
 (vertico-mode)

 (after 'rfn-eshadow
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

 (after 'window
  (define-key global-map [remap switch-to-buffer] #'consult-buffer))

 (after 'imenu
  (define-key global-map [remap imenu] #'consult-imenu))

 (package 'consult)
 (after 'consult
  (declvar consult-narrow-map)
  (define-key consult-narrow-map (kbd "C-?") 'consult-narrow-help)
  (setopt
   consult-preview-key "M-."
   consult-project-function (lambda (_) (projectile-project-root))))

 (defun init/consult-grep-or-git-grep ()
  "Run grep in non-project buffers and git-grep in project buffers."
  (interactive)
  (if (and (fboundp 'projectile-project-root) (projectile-project-root))
   (consult-git-grep)
   (consult-grep)))

 (define-key global-map (kbd "M-Y") #'consult-yank-pop)
 (define-key global-map (kbd "M-g I") #'consult-imenu-multi)
 (define-key global-map (kbd "C-x S") #'consult-line)
 (define-key global-map (kbd "M-G") #'init/consult-grep-or-git-grep)
 (define-key global-map (kbd "M-D") #'consult-fd))

(config "Registers"
 (after 'consult-register
  (advice-add #'consult-register :after #'init/recenter))

 (after 'register
  (define-key global-map [remap jump-to-register] #'consult-register)
  (define-key global-map [remap point-to-register] #'consult-register-store)
  (setopt
   register-use-preview t)))

(config "Bookmarks"
 (after 'bookmark
  (declvar bookmark-bmenu-mode-map)
  (define-key bookmark-bmenu-mode-map (kbd "C-p") #'casual-bookmarks-tmenu)))

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
 (package 'easysession)
 (after 'easysession (setopt easysession-save-mode-lighter-show-session-name t))
 (define-key global-map (kbd "C-c u") #'easysession-save)
 (define-key global-map (kbd "C-c U") #'easysession-load))

(config "Spell Checking"
 (package 'jinx)
 (after 'jinx
  (diminish 'jinx-mode "Jx")
  (declvar jinx-mode-map)
  (define-key jinx-mode-map (kbd "M-$") #'jinx-correct)
  (define-key jinx-mode-map (kbd "C-M-$") #'jinx-languages)))

(config "Window Faces"
 (autoload 'face-remap-remove-relative "face-remap")
 (defface init/dedicated-mode-line '((t :height 0.2)) "Dedicated Window Modeline Face")
 (defvar-local init/dedicated-mode-line-cookie nil))

(config "Utilities"
 (package 'which-key)
 (after 'which-key
  (diminish 'which-key-mode)
  (setopt
   ;; which-key-idle-delay 0.5
   which-key-show-docstrings nil
   which-key-add-column-padding 3
   which-key-max-description-length nil
   which-key-max-display-columns nil))
 (which-key-mode)

 (package 'embark)
 (package 'embark-consult)
 (after 'embark
  (setopt
   prefix-help-command #'embark-prefix-help-command
   embark-mixed-indicator-both t
   embark-mixed-indicator-delay 0))

 (define-key global-map (kbd "C-.") #'embark-act)
 (define-key global-map (kbd "C-;") #'embark-dwim)
 (define-key global-map (kbd "C-h B") #'embark-bindings)

 (define-key minibuffer-local-map (kbd "C-'") #'embark-collect)
 (define-key minibuffer-local-map (kbd "C-x E") #'embark-export)
 (define-key minibuffer-local-map (kbd "C-x B") #'embark-become))

(config "In-buffer Completion"
 (defun init/buffer-completion-mode ()
  "Activate in-buffer completion system."
  (cond
   ((eq *init/completion-system* :company)
    (company-mode))
   ((eq *init/completion-system* :corfu)
    (corfu-mode))
   (t (error "init.el: Unknown completion system requested"))))

 (config "Corfu"
  (package 'corfu)
  (package 'nerd-icons-corfu)
  (after 'corfu
   ;; Show icons in corfu popups.
   (declvar corfu-margin-formatters)
   (push #'nerd-icons-corfu-formatter corfu-margin-formatters)
   (setopt
    corfu-preview-current nil
    corfu-auto t
    corfu-auto-delay 0.4
    corfu-auto-prefix 1
    corfu-auto-trigger ".&"
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
  (package 'company)
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

  (package'company-posframe)
  (after 'company-posframe
   (diminish 'company-posframe-mode)
   (declvar company-posframe-show-params)
   (declvar company-posframe-quickhelp-show-params)
   (nconc company-posframe-show-params '(:border-width 1))
   (nconc company-posframe-quickhelp-show-params '(:border-width 1))
   (setopt company-posframe-quickhelp-x-offset 2)))

 (config "Cape"
  (package 'cape)
  (define-key global-map (kbd "C-c p") #'cape-prefix-map)

  (after 'cape
   (advice-add 'cape-file :around #'cape-wrap-nonexclusive)
   (advice-add 'cape-dabbrev :around #'cape-wrap-nonexclusive))))

(config "Syntax Highlighting"
 (package 'tree-sitter)
 (package 'tree-sitter-langs)

 (defun init/tree-sitter-langs-install-grammars ()
  (tree-sitter-langs-install-grammars t))

 (after 'tree-sitter
  (diminish 'tree-sitter-mode "Ts")
  (add-hook 'tree-sitter-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'tree-sitter-mode-hook #'init/tree-sitter-langs-install-grammars))
 (after 'tree-sitter-hl
  (face tree-sitter-hl-face:property :inherit font-lock-keyword-face))
 (after 'tree-sitter-langs-build
  (declvar tree-sitter-langs-grammar-dir)
  (setopt tree-sitter-langs-git-dir
   (file-name-concat tree-sitter-langs-grammar-dir "git")))

 (after 'treesit
  (setopt
   treesit-language-source-alist
   '((bash   . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c      . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp    . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (json   . ("https://github.com/tree-sitter/tree-sitter-json.git"))
     (toml   . ("https://github.com/ikatyang/tree-sitter-toml.git"))
     (yaml   . ("https://github.com/ikatyang/tree-sitter-yaml.git")))))

 (after 'jit-lock
  (setopt
   jit-lock-stealth-time 0.1
   ;; A little more than what can fit on the screen.
   jit-lock-chunk-size 4000
   jit-lock-antiblink-grace nil)))

(config "Syntax Checkers and Error Lists"
 (after 'simple
  (setopt
   ;; Recenter after jump to next error.
   next-error-recenter '(4)
   next-error-message-highlight t))

 (defun init/delete-ancillary-window (window)
  (unless (eq window (selected-window))
   (delete-window window)))

 (defface init/flycheck-errors-text '((t :height 0.8)) "Flycheck errors text")
 (defvar-local init/flycheck-errors-text-cookie nil)

 (defun init/setup-flycheck-errors-window (window)
  (with-current-buffer (window-buffer window)
   (add-hook 'window-selection-change-functions #'init/delete-ancillary-window nil t))
  (with-selected-window window
   (face-remap-remove-relative init/dedicated-mode-line-cookie)
   (face-remap-remove-relative init/flycheck-errors-text-cookie)
   (setq init/dedicated-mode-line-cookie
    (face-remap-add-relative 'mode-line-active 'init/dedicated-mode-line))
   (setq init/flycheck-errors-text-cookie
    (face-remap-add-relative 'default 'init/flycheck-errors-text))
   (declvar flycheck-error-list-mode-map)
   (define-key flycheck-error-list-mode-map [remap keyboard-quit]
    #'(lambda () (interactive) (delete-window window)))))

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

 (package 'flycheck)
 (package 'consult-flycheck)
 (autoload 'flycheck-error-list-make-last-column "flycheck")
 (after 'flycheck
  (defconst flycheck-error-list-format
   `[("File" 20)
     ("Line" 5 flycheck-error-list-entry-< :right-align nil)
     ("Col" 5 nil :right-align nil)
     ("Level" 16 flycheck-error-list-entry-level-<)
     ("ID" 32 t)
     (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)]
   "Table format for the error list.")

  (declvar flycheck-mode-map)
  (define-key flycheck-mode-map (kbd "C-c ! L") #'consult-flycheck)
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)

  (advice-add 'flycheck-next-error :after #'init/recenter)
  (advice-add 'flycheck-previous-error :after #'init/recenter)
  (advice-add 'flycheck-error-list-goto-error :after #'init/recenter)

  (setopt
   flycheck-help-echo-function nil
   flycheck-checker-error-threshold nil
   flycheck-mode-line-prefix "Fc"
   flycheck-check-syntax-automatically '(idle-change mode-enabled save new-line))))

(config "Snippets"
 (package 'yasnippet)
 (package 'yasnippet-snippets)

 (defvar *init/yasnippet-snippets-initialized* nil)
 (defun init/initialize-yasnippet-snippets ()
  "Initialize yasnippet snippets if they have not already been."
  (unless *init/yasnippet-snippets-initialized*
   (yasnippet-snippets-initialize)
   (setq *init/yasnippet-snippets-initialized* t)))

 (after 'yasnippet
  (diminish 'yas-minor-mode "Ys")
  (declvar yas-minor-mode-map)
  (define-key yas-minor-mode-map (kbd "TAB") nil t)
  (add-hook 'yas-minor-mode-hook #'init/initialize-yasnippet-snippets))
 (add-to-list 'yas-snippet-dirs "~/Workspace/dots/emacs/snippets"))

(config "Dired"
 (package 'nerd-icons-dired)

 (after 'dired-async (diminish 'dired-async-mode "As"))

 (defun init/dired-setup ()
  "Setup dired requires."
  (require 'dired-x)
  (require 'wdired)
  (require 'image-dired))

 (autoload 'dired-hide-details-mode "dired")
 (after 'dired
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
  (add-hook 'dired-mode-hook #'init/dired-setup)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'context-menu-mode)
  (add-hook 'dired-mode-hook #'dired-async-mode)
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (declvar dired-mode-map)
  (define-key dired-mode-map (kbd "C-p") #'casual-dired-tmenu)
  (setopt
   dired-mouse-drag-files t
   dired-listing-switches "-l -h --group-directories-first"
   dired-hide-details-hide-symlink-targets nil
   dired-recursive-copies 'always
   dired-recursive-deletes 'always
   dired-dwim-target t)))

(config "Project Management"
 (package 'projectile)
 (autoload 'projectile-project-root "projectile")
 (after 'projectile
  (diminish 'projectile-mode "Pr")
  (setopt
   ;; projectile-indexing-method 'hybrid
   ;; projectile-require-project-root nil
   projectile-project-search-path '(("~/Workspace" . 3))
   projectile-sort-order 'recently-active
   projectile-auto-cleanup-known-projects t
   projectile-enable-caching nil
   projectile-auto-discover t))
 (define-key global-map (kbd "C-x p") 'projectile-command-map)
 (after 'emacs
  ;; startup.el
  (add-hook 'after-init-hook #'projectile-mode)))

(config "Version Control"

 (after 'vc (setopt vc-make-backup-files t))

 (autoload 'ediff-setup-windows-plain "ediff-wind")
 (after 'ediff-wind
  (setopt
   ;; ediff-split-window-function #'split-window-right
   ediff-split-window-function #'split-window-horizontally
   ediff-window-setup-function #'ediff-setup-windows-plain))

 (package 'blamer)
 (after 'blamer
  (setopt
   ;; blamer-idle-time 0
   blamer-commit-formatter ": %s"
   blamer-datetime-formatter "%s"
   blamer-max-commit-message-length 60))

 (package 'magit)
 (define-key global-map (kbd "C-x g") #'magit-status)

 (after 'magit-process
  (add-hook 'magit-process-mode-hook #'goto-address-mode))

 (defun init/disable-line-numbers ()
  "Disable display-line-numbers-mode."
  (display-line-numbers-mode -1))

 (autoload 'magit-format-file-nerd-icons "magit-diff")
 (autoload 'magit-restore-window-configuration "magit-mode")
 (autoload 'magit-display-buffer-fullframe-status-v1 "magit-mode")
 (autoload 'diff-hl-magit-post-refresh "diff-hl")
 (after 'magit-mode
  (add-hook 'magit-mode-hook #'init/disable-line-numbers)
  (add-hook 'magit-mode-hook #'init/magit-after-save-refresh-status)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  (setopt
   magit-log-section-commit-count 20
   ;; magit-auto-revert-tracked-only nil
   ;; magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
   magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
   magit-bury-buffer-function #'magit-restore-window-configuration
   magit-repository-directories '(("~/Workspace" . 3))
   magit-format-file-function #'magit-format-file-nerd-icons))

 (autoload 'magit-insert-worktrees "magit-worktree")
 (autoload 'magit-insert-xref-buttons "magit-worktree")
 (autoload 'magit-insert-local-branches "magit-worktree")
 (after 'magit-status
  (add-hook 'magit-status-sections-hook #'magit-insert-worktrees)
  (add-hook 'magit-status-sections-hook #'magit-insert-xref-buttons)
  (add-hook 'magit-status-sections-hook #'magit-insert-local-branches))

 (autoload 'magit-after-save-refresh-status "magit-mode")
 (defun init/magit-after-save-refresh-status ()
  "Refresh magit after save."
  (add-hook 'after-save-hook #'magit-after-save-refresh-status nil t))

 (defun init/magit-load-nerd-icons (&rest args)
  (if (require 'nerd-icons nil t)
   (apply args)
   (package 'nerd-icons)))

 (after 'magit-diff
  (advice-add 'magit-format-file-nerd-icons :around #'init/magit-load-nerd-icons)
  (advice-add 'magit-diff-visit-file :after #'init/recenter)
  (setopt
   magit-revision-show-gravatars t
   magit-revision-fill-summary-line fill-column))

 (package 'diff-hl)
 (after 'diff-hl
  (setopt
   diff-hl-flydiff-delay 1
   diff-hl-draw-borders nil
   diff-hl-update-async t)
  (add-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)
  (add-hook 'diff-hl-mode-hook #'diff-hl-show-hunk-mouse-mode)))

(config "General Programming"
 (after 'eldoc
  (diminish 'eldoc-mode "Ed")
  (after 'flycheck (eldoc-add-command-completions "flycheck-"))
  (after 'mwim (eldoc-add-command-completions "mwim-"))
  (setopt
   eldoc-documentation-strategy 'eldoc-documentation-compose
   eldoc-echo-area-use-multiline-p nil
   eldoc-echo-area-prefer-doc-buffer t))

 (defun init/setup-eldoc-window (window)
  (with-selected-window window
   (define-key special-mode-map [remap keyboard-quit]
    #'(lambda () (interactive) (delete-window window)))))

 (after 'window
  (push `(,(rx bos "*eldoc" (1+ nonl) "*" eos)
          (display-buffer-reuse-mode-window display-buffer-in-side-window)
          (mode . special-mode)
          (side . right)
          (slot . 1)
          (dedicated . t)
          (window-parameters . ((mode-line-format . "")))
          (body-function . init/setup-eldoc-window))
   display-buffer-alist))

 (after 'prog-mode
  (define-key prog-mode-map (kbd "C-x D") #'eldoc 'prog-mode-map))

 (package 'editorconfig)
 (after 'editorconfig (diminish 'editorconfig-mode "Ec"))

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
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  (add-hook 'prog-mode-hook #'show-paren-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (define-key global-map (kbd "C-h D") #'devdocs-lookup)
  (define-key global-map (kbd "C-c b") #'blamer-mode))

 (package 'devdocs)
 (after 'devdocs
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
  (setq-mode-local makefile-mode devdocs-current-docs "gnu_make"))

 (after 'elec-pair
  (setopt
   electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit
   electric-pair-preserve-balance nil))

 (after 'subword (diminish 'subword-mode "Sw"))

 (after 'xref
  (add-hook 'xref-after-return-hook #'recenter)
  (setopt
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref))

 (package 'sideline)
 (package 'sideline-blame)
 (after 'sideline
  (diminish 'sideline-mode "Si")
  (setopt
   sideline-backends-right '(sideline-blame)))

 (after 'sideline-blame (setopt sideline-blame-commit-format "- %s"))

 (package 'indent-bars)
 (after 'indent-bars
  (setopt
   indent-bars-treesit-support t
   indent-bars-width-frac 0.1))

 (after 'files
  (setopt
   safe-local-variable-values
   '((comment-style . multi-line)
     (backward-delete-char-untabify-method . nil)
     (electric-indent-inhibit . nil)
     (lsp-enable-indentation . nil)
     (lsp-enable-on-type-formatting . nil)
     (lsp-enable-semantic-highlighting . nil)))))

(config "Debugging"
 (autoload 'debugger-quit "debug")
 (after 'debug
  (declvar debugger-mode-map)
  (define-key debugger-mode-map (kbd"C-g") #'debugger-quit))

 (defun init/make-selected-window-dedicated ()
  (set-window-dedicated-p (selected-window) t))

 (after 'gdb-mi
  (setopt
   gdb-many-windows t
   gdb-use-separate-io-buffer t)
  (advice-add 'gdb-setup-windows :after #'init/make-selected-window-dedicated))

 (after 'gud
  (add-hook 'gud-mode-hook #'gud-tooltip-mode)
  (setopt gdb-restore-window-configuration-after-quit t))

 (package 'dape)
 (autoload 'dape-breakpoint-global-mode "dape")
 (autoload 'dape-breakpoint-load "dape")
 (autoload 'dape-breakpoint-save "dape")
 (autoload 'dape-info "dape")
 (after 'dape
  (add-hook 'dape-stopped-hook #'dape-breakpoint-save)
  (add-hook 'dape-breakpoint-global-mode-hook #'dape-breakpoint-load)
  (add-hook 'dape-start-hook #'save-some-buffers)
  (add-hook 'dape-start-hook #'dape-info)
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  (setopt
   dape-buffer-window-arrangement 'right
   dape-inlay-hints t
   dape-cwd-fn 'projectile-project-root
   dape-default-breakpoints-file
    (no-littering-expand-var-file-name "dape-breakpoints")))

 (after 'lsp-mode
  (add-hook 'lsp-mode-hook #'dape-breakpoint-global-mode)))

(config "LSP"
 (package 'lsp-mode)

 (after 'lsp-mode
  (diminish 'lsp-mode "Ls")
  (setopt
   lsp-before-save-edits nil))

 (after 'lsp-lens
  (diminish 'lsp-lens-mode "Lns"))

 (after 'lsp-semantic-tokens
  (setopt
   lsp-semantic-tokens-enable t))

 ;; Improve LSP performance by trying to parse elisp objects from emacs-lsp-booster.
 (defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
    (let ((bytecode (read (current-buffer))))
     (when (byte-code-function-p bytecode)
      (funcall bytecode))))
   (apply old-fn args)))

 (after 'lsp-mode
  (advice-add (if (progn (require 'json)
                   (fboundp 'json-parse-buffer))
               'json-parse-buffer
               'json-read)
   :around #'lsp-booster--advice-json-parse))

 ;; Improve LSP performance by using emacs-lsp-booster.
 (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
   ;; for check lsp-server-present?
   (if (and (not test?)
        ;; see lsp-resolve-final-command, it would add extra shell wrapper
        (not (file-remote-p default-directory))
        (declvar lsp-use-plists)
        lsp-use-plists
        ;; native json-rpc
        (not (functionp 'json-rpc-connection))
        (executable-find "emacs-lsp-booster"))
    (progn
     ;; resolve command from exec-path (in case not found in $PATH)
     (when-let ((command-from-exec-path (executable-find (car orig-result))))
      (setcar orig-result command-from-exec-path))
     (message "Using emacs-lsp-booster for %s!" orig-result)
     (cons "emacs-lsp-booster" orig-result))
    orig-result)))

 (after 'lsp-mode
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)))

(config "Translation Files"
 (package 'po-mode))

(config "Sed Files"
 (package 'sed-mode))

(config "Configuration Files"
 (after 'conf-mode
  (add-hook 'conf-mode-hook #'electric-pair-local-mode)
  (add-hook 'conf-desktop-mode-hook #'electric-pair-local-mode)
  (add-hook 'conf-mode-hook #'electric-layout-local-mode)
  (add-hook 'conf-desktop-mode-hook #'electric-layout-local-mode)
  (add-hook 'conf-mode-hook #'diff-hl-mode)
  (add-hook 'conf-desktop-mode-hook #'diff-hl-mode)
  (add-hook 'conf-mode-hook #'show-paren-mode)
  (add-hook 'conf-desktop-mode-hook #'show-paren-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-desktop-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'hl-line-mode)
  (add-hook 'conf-desktop-mode-hook #'hl-line-mode)
  (add-hook 'conf-mode-hook #'jinx-mode)
  (add-hook 'conf-desktop-mode-hook #'jinx-mode)
  (add-hook 'conf-mode-hook #'whitespace-mode)
  (add-hook 'conf-desktop-mode-hook #'whitespace-mode)))

(config "Markdown"
 (package 'markdown-mode)
 (after 'markdown-mode
  (declvar markdown-mode)
  (setq-mode-local markdown-mode fill-column 79)
  (add-hook 'markdown-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'markdown-mode-hook #'hl-line-mode)))

(config "JSON"
 (package 'json-mode)
 (after 'json-mode
  (add-hook 'json-mode-hook #'indent-bars-mode)
  (add-hook 'json-mode-hook #'tree-sitter-mode)
  (add-hook 'json-mode-hook #'hl-line-mode)
  (add-hook 'json-mode-hook #'display-line-numbers-mode)
  (add-hook 'json-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'json-mode-hook #'whitespace-mode)))

(config "TOML"
 (package 'toml-mode)
 (after 'toml-mode
  (add-hook 'toml-mode-hook #'eldoc-toml-mode)
  (add-hook 'toml-mode-hook #'hl-line-mode)
  (add-hook 'toml-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'toml-mode-hook #'display-line-numbers-mode)
  (add-hook 'toml-mode-hook #'whitespace-mode))
 (package 'eldoc-toml)
 (after 'eldoc-toml (diminish 'eldoc-toml)))

(config "YAML"
 (package 'yaml-mode)
 (after 'yaml-mode
  (declvar yaml-mode-map)
  (define-key yaml-mode-map (kbd "C-c p") #'qol/generate-password)
  (add-hook 'yaml-mode-hook #'indent-bars-mode)
  (add-hook 'yaml-mode-hook #'tree-sitter-mode)
  (add-hook 'yaml-mode-hook #'flycheck-mode)
  (add-hook 'yaml-mode-hook #'hl-line-mode)
  (add-hook 'yaml-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'yaml-mode-hook #'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook #'whitespace-mode)))

(config "LLVM"
 (package 'llvm-ts-mode)
 (package 'demangle-mode)
 (package 'autodisass-llvm-bitcode)
 (mode (rx ".ll" eos) #'llvm-ts-mode)
 (mode (rx ".clang-format" eos) #'yaml-mode)
 (mode (rx ".clang-tidy" eos) #'yaml-mode)
 (mode (rx ".clangd" eos) #'yaml-mode)
 (require 'autodisass-llvm-bitcode)
 (after 'llvm-ts-mode
  (add-hook 'llvm-ts-mode-hook #'demangle-mode)))

(config "Archlinux PKGBUILDs"
 (package 'pkgbuild-mode)
 (mode (rx bos "PKGBUILD" eos) #'pkgbuild-mode)
 (after 'tree-sitter-langs
  (declvar tree-sitter-major-mode-language-table)
  (puthash 'pkgbuild-mode 'bash tree-sitter-major-mode-language-table)))

(config "Docker"
 (package 'dockerfile-mode)
 (package 'docker-compose-mode)
 (package 'docker)
 (define-key global-map (kbd "C-c D") #'docker))

(config "Makefiles"
 (after 'make-mode
  (add-hook 'makefile-mode-hook #'whitespace-mode)))

(config "Web Development"
 (package 'web-mode)

 (mode (rx ".html" eos) #'web-mode)
 (mode (rx ".css" eos) #'web-mode)
 (mode (rx ".js" eos) #'web-mode)

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
  (setq-mode-local web-mode tab-width 2)
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'emmet-mode)

  (package 'company-web)
  (after 'company
   (declvar company-backends)
   (setq-mode-local web-mode company-backends '((company-css company-web-html)))))

 (package 'emmet-mode)
 (after 'emmet-mode
  (diminish 'emmet-mode "Em")
  (setopt emmet-indentation 2))

 (defun init/css-setup-comments ()
  "Setup C-style /* ... */ comments."
  (with-eval-after-load 'newcomment
   (setq-local comment-style 'extra-line)))

 (after 'css-mode
  (add-hook 'css-mode-hook #'init/css-setup-comments)))

(config "Meson"
 (package 'meson-mode)
 (after 'meson-mode
  (add-hook 'meson-mode-hook #'symbol-overlay-mode)
  (add-hook 'meson-mode-hook #'lsp))
 (after 'lsp-meson (setopt lsp-meson-server-executable '("mesonlsp" "--full"))))

(config "Shell Scripting"
 (mode (rx ".bashrc.user" eos) #'sh-mode)

 (defun init/make-file-executable ()
  "Makes the file executable on save."
  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p nil t))

 (after 'sh-script
  (add-hook 'sh-mode-hook #'init/make-file-executable)
  (add-hook 'sh-mode-hook #'tree-sitter-mode)
  (add-hook 'bash-ts-mode-hook #'init/make-file-executable)
  (setopt
   sh-basic-offset 2
   sh-indentation 2)))

(config "Emacs Lisp"
 (package 'eros)
 (package 'suggest)
 (package 'ipretty)
 (package 'highlight-quoted)
 (package 'highlight-defined)

 (after 'elisp-mode
  (setopt
   lisp-indent-offset 1
   lisp-indent-function #'common-lisp-indent-function)
  (advice-add 'elisp-completion-at-point :around #'cape-wrap-case-fold)
  (advice-add 'elisp-completion-at-point :around #'cape-wrap-nonexclusive)
  (define-key emacs-lisp-mode-map
   (kbd "<f6>") #'init/emacs-lisp-expand-current-macro-call)
  (add-hook 'emacs-lisp-mode-hook #'init/setup-elisp-capfs)
  (add-hook 'emacs-lisp-mode-hook #'init/buffer-completion-mode)
  (add-hook 'emacs-lisp-mode-hook #'eros-mode)
  (add-hook 'emacs-lisp-mode-hook #'(lambda () (ipretty-mode t)))
  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode)
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)
  (add-hook 'emacs-lisp-mode-hook #'symbol-overlay-mode)
  (add-hook 'emacs-lisp-mode-hook #'whitespace-mode))

 (defun init/emacs-lisp-expand-current-macro-call ()
  "Expand the current macro expression."
  (interactive)
  (beginning-of-defun)
  (emacs-lisp-macroexpand))

 (defun init/elisp-capfs ()
  (cape-wrap-super
   'elisp-completion-at-point
   #'yasnippet-capf
   ;; #'cape-dabbrev
   #'cape-file))

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
 (package 'hledger-mode)

 (mode (rx ".journal" eos) #'hledger-mode)
 (mode (rx ".ledger" eos) #'hledger-mode)
 (mode (rx ".hledger" eos) #'hledger-mode)

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
  (define-key hledger-mode-map (kbd "C-c >") #'init/hledger-move-amount-to-column)
  (define-key hledger-mode-map (kbd "C-c x") #'init/hledger-find-next-unaligned)
  (define-key hledger-mode-map (kbd "C-c +") 'hledger-increment-entry-date)

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
  (add-hook 'hledger-mode-hook #'electric-pair-local-mode)
  (add-hook 'hledger-mode-hook #'whitespace-mode)
  (add-hook 'hledger-mode-hook #'symbol-overlay-mode)
  (add-hook 'hledger-mode-hook #'yas-minor-mode)
  (add-hook 'hledger-mode-hook #'display-fill-column-indicator-mode)
  (add-hook 'hledger-mode-hook #'hl-line-mode))

 (package 'flycheck-hledger)
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
  (cape-wrap-super
   'hledger-completion-at-point
   ;; #'cape-dabbrev
   #'yasnippet-capf))

 (defun init/setup-hledger-capfs ()
  (setq-local completion-at-point-functions (list #'init/hledger-capfs)))

 (after 'hledger-core
  (setopt
   hledger-currency-string ""
   hledger-comments-column 1
   hledger-jfile "~/Documents/Expenses/Expenses.ledger"))

 (after 'flycheck-hledger (setopt flycheck-hledger-checks '("commodities"))))

(config "Python"
 (package 'uv-mode)

 (after 'python
  (setq-mode-local python-mode fill-column 79)
  (add-hook 'python-base-mode-hook #'uv-mode-auto-activate-hook)
  (add-hook 'python-base-mode-hook #'indent-bars-mode)
  (add-hook 'python-base-mode-hook #'lsp))

 (after 'lsp-pylsp
  (setopt
   lsp-pylsp-plugins-autopep8-enabled t
   lsp-pylsp-plugins-black-enabled t
   lsp-pylsp-plugins-isort-enabled t
   lsp-pylsp-plugins-jedi-completion-fuzzy t
   lsp-pylsp-plugins-mypy-dmypy t
   lsp-pylsp-plugins-mypy-enabled t
   lsp-pylsp-plugins-mypy-report-progress t
   lsp-pylsp-plugins-pycodestyle-enabled t
   lsp-pylsp-plugins-pyflakes-enabled t
   lsp-pylsp-plugins-pylint-enabled t
   lsp-pylsp-plugins-rope-autoimport-code-actions-enabled t
   lsp-pylsp-plugins-rope-autoimport-completions-enabled t
   lsp-pylsp-plugins-rope-autoimport-enabled t
   lsp-pylsp-plugins-rope-completion-enabled t
   lsp-pylsp-plugins-ruff-enabled t
   lsp-pylsp-plugins-ruff-preview t
   lsp-pylsp-plugins-yapf-enabled t)))

(config "Rust"
 (package 'rust-mode)
 (package 'rustic))

(config "C/C++"
 (autoload 'lsp-clangd-find-other-file "lsp-clangd")

 (after 'cc-mode
  (setopt c-doc-comment-style '((c-mode    . gtkdoc) (c++-mode  . doxygen)))
  (declvar c-mode-base-map)
  (define-key c-mode-base-map (kbd "<f2>") #'lsp-clangd-find-other-file)
  (define-key c-mode-base-map (kbd "(") #'nil t))

 (after 'cc-cmds
  ;; Unmark region even when c-indent-line-or-region doesn't indent anything.
  (advice-add 'c-indent-line-or-region :after #'keyboard-quit))

 (defun init/cc-setup-comments ()
  "Setup C-style /* ... */ comments."
  (with-eval-after-load 'newcomment
   (setq-local comment-style 'extra-line)))

 (after 'cc-vars
  (setopt
   c-mark-wrong-style-of-comment t
   c-default-style '((other . "user"))
   c-basic-offset 2
   c-tab-always-indent 'complete)
  (add-hook 'c-mode-common-hook #'init/cc-setup-comments)
  (add-hook 'c-mode-common-hook #'lsp))

 (after 'lsp-clangd
  (declvar lsp-clients-clangd-args)
  (push "--enable-config" lsp-clients-clangd-args)
  (push "--all-scopes-completion=0" lsp-clients-clangd-args)
  (push "--query-driver=/**/*" lsp-clients-clangd-args)
  (push "--all-scopes-completion" lsp-clients-clangd-args)
  (push "--log=error" lsp-clients-clangd-args)
  (push "--background-index=0" lsp-clients-clangd-args)
  (push "--clang-tidy" lsp-clients-clangd-args)
  (push "--completion-style=detailed" lsp-clients-clangd-args)
  (push "--function-arg-placeholders" lsp-clients-clangd-args)
  (push "--header-insertion=never" lsp-clients-clangd-args)
  ;; This sometimes breaks LSP completion.
  (push "--header-insertion-decorators=0" lsp-clients-clangd-args)
  (push "--limit-references=0" lsp-clients-clangd-args)
  (push "--limit-results=0" lsp-clients-clangd-args)
  (push "--rename-file-limit=0" lsp-clients-clangd-args)
  (push "-j=16" lsp-clients-clangd-args)
  (push "--malloc-trim" lsp-clients-clangd-args)
  (push "--pch-storage=memory" lsp-clients-clangd-args)))

;;; C and C++ Programming

;; (use-package lsp-completion
;;  :ensure lsp-mode
;;  :defer t
;;  :preface (package 'lsp-mode)
;;  :config
;;  (advice-add #'lsp-completion-at-point :around #'cape-wrap-case-fold)
;;  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive))

;;; Project Management

(use-package treemacs-projectile
 :ensure t
 :defer t
 :preface (package 'treemacs-projectile)
 :after (treemacs projectile))

;;; Snippets

(use-package yasnippet-capf
 :ensure t
 :defer t
 :preface (package 'yasnippet-capf)
 :custom
 (yasnippet-capf-lookup-by 'name)
 :config
 (advice-add 'yasnippet-capf :around #'cape-wrap-nonexclusive))

;;; Rust

(config "Rust Programming"
 (package 'rust-mode)
 (package 'lsp-mode)

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
  (define-key rust-mode-map (kbd "<f5>") #'rust-dbg-wrap-or-unwrap)
  (define-key rust-mode-map (kbd "<f6>") #'lsp-rust-analyzer-expand-macro)
  (define-key rust-mode-map (kbd "<f7>") #'lsp-rust-analyzer-join-lines)

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
 (package 'lsp-mode)

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
 :preface (package 'lsp-mode)
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

;; Print startup stats.
(message "Startup in %s (%d GC runs that took %fs)" (emacs-init-time) gcs-done gc-elapsed)

(provide 'init)
;;; init.el ends here

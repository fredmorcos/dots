;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; No Littering --------------------------------------------------------------------------

;; Directories -- TODO REMOVE THESE
(defconst user-home-dir (expand-file-name "~/"))
(defconst user-dict-en (concat user-home-dir ".aspell.en.pws"))
(defconst emacs-user-dir (expand-file-name user-emacs-directory))
(defconst emacs-elpa-dir (concat emacs-user-dir "elpa"))
(defconst emacs-var-dir (concat emacs-user-dir "var/"))
(defconst emacs-recentf-file (concat emacs-var-dir "recentf"))
(defconst emacs-saveplace-file (concat emacs-var-dir "saveplace"))
(defconst emacs-savehist-file (concat emacs-var-dir "savehist"))
(defconst emacs-package-qs-file (concat emacs-var-dir "package-qs"))
(defconst emacs-projectile-cache-file (concat emacs-var-dir "projectile-cache"))
(defconst emacs-projectile-projects-file (concat emacs-var-dir "projectile-projects"))
(defconst emacs-prescient-save-file (concat emacs-var-dir "prescient-save"))
(defconst emacs-bookmarks-file (concat emacs-var-dir "bookmarks"))
(defconst emacs-tmp-dir (concat temporary-file-directory "emacs/"))
(defconst emacs-autosaves-dir (concat emacs-tmp-dir "autosaves"))
(defconst emacs-autosaves-pat (concat emacs-autosaves-dir "/\\1"))
(defconst emacs-autosave-list-prefix (concat emacs-tmp-dir "auto-save-list/saves-"))
(defconst emacs-backups-dir (concat emacs-tmp-dir "backups"))
(defconst emacs-backups-pat (concat emacs-backups-dir "/"))
(make-directory emacs-var-dir t)
(make-directory emacs-autosaves-dir t)
(make-directory emacs-backups-dir t)

(use-package no-littering
 :commands
 no-littering-theme-backups
 no-littering-expand-etc-file-name
 no-littering-expand-var-file-name

 :config
 (no-littering-theme-backups))

;; Functions -----------------------------------------------------------------------------

(use-package emacs
 :ensure nil

 :config
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
 (put 'diary            'disabled t))

;; Text Editing --------------------------------------------------------------------------

(use-package qol
 :ensure nil
 :defer t

 :load-path "/home/fred/Workspace/dots/emacs/"

 :bind
 (("C-x j"    . qol/insert-buffer-name)
  ("C-x e"    . qol/replace-escapes)
  ("{"        . qol/insert-pair-curly)
  ("{"        . qol/insert-pair-curly)
  ("("        . qol/insert-pair-parens)
  ("'"        . qol/insert-pair-quote)
  ("\""       . qol/insert-pair-double-quotes)
  ("`"        . qol/insert-pair-backtick)))

(use-package misc
 :ensure nil
 :defer t

 :bind
 (("C-x c" . duplicate-dwim)))

(use-package cua-base
 :defer t
 :ensure nil

 :init
 (cua-selection-mode t))

(use-package move-text
 :defer t

 :init
 (move-text-default-bindings))

(use-package emacs
 :ensure nil

 :custom
 (tab-always-indent 'complete)
 (tab-first-completion 'word-or-paren-or-punct))

(use-package simple
 :ensure nil

 :custom
 (indent-tabs-mode nil))

(use-package unfill
 :bind
 ([remap fill-paragraph] . unfill-toggle))

;; Auto-save -----------------------------------------------------------------------------

(use-package emacs
 :ensure nil

 :custom
 (auto-save-list-file-prefix emacs-autosave-list-prefix))

;; UX ------------------------------------------------------------------------------------

(use-package windmove
 :ensure nil

 :init
 (windmove-default-keybindings)
 (windmove-delete-default-keybindings))

(use-package goto-addr
 :defer t
 :ensure nil

 :init
 ;; Make URLs clickable
 (global-goto-address-mode))

(use-package emacs
 :ensure nil

 :defines
 warning-suppress-types

 :init
 ;; Suppress certain annoying warnings.
 (add-to-list 'warning-suppress-types '(defvaralias)))

;; Dynamic Expansion ---------------------------------------------------------------------

(use-package hippie-exp
 :ensure nil

 :bind
 ;; Replace dabbrev-expand with hippie-expand
 ([remap dabbrev-expand] . hippie-expand)

 :custom
 (hippie-expand-try-functions-list
  '(try-expand-dabbrev-visible
    try-expand-line
    try-expand-dabbrev
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

;; Other ---------------------------------------------------------------------------------

(eval-when-compile
 (defconst emacs-dots-dir "/home/fred/Workspace/dots/emacs/")
 (push emacs-dots-dir load-path))
(require 'init-macros)

(im/after emacs
 (im/key-remap dabbrev-expand hippie-expand)

 ;; Completion
 (setq-default completion-ignore-case t)
 (setq-default read-buffer-completion-ignore-case t)

 ;; Fill
 (setq-default fill-column 90)

 ;; History/savehist
 (setq-default history-delete-duplicates t)
 (setq-default history-length 150)

 ;; External Processes
 (setq-default read-process-output-max (* 1024 1024))

  ;; Scrolling
 (setq-default scroll-conservatively 104)
 (setq-default scroll-margin 3)
 (setq-default hscroll-margin 3)
 (setq-default hscroll-step 1)
 (setq-default auto-hscroll-mode 'current-line)
 (setq-default fast-but-imprecise-scrolling t))

(im/key-interactive "<f10>" (scroll-other-window 1))
(im/key-interactive "<f11>" (scroll-other-window-down 1))

(im/after files
 (setq-default confirm-kill-processes nil)
 (setq-default auto-save-file-name-transforms `((".*" ,emacs-autosaves-pat t)))
 (setq-default auto-save-default t)
 (setq-default backup-directory-alist `((".*" . ,emacs-backups-pat)))
 (setq-default backup-inhibited nil)
 (setq-default make-backup-files t)
 (setq-default delete-old-versions t)
 (setq-default mode-require-final-newline 'visit-save)
 (setq-default require-final-newline 'visit-save)
 (setq-default load-prefer-newer t)
 (setq-default coding-system-for-read 'utf-8-unix)
 (setq-default coding-system-for-write 'utf-8-unix)
;; (setq-default major-mode-remap-alist
;;  '((c-mode . c-ts-mode)
;;    (c++-mode . c++-ts-mode)
;;    (c-or-c++-mode . c-or-c++-ts-mode)
;;    (rust-mode . rust-ts-mode)
;;    (sh-mode . bash-ts-mode)
;;    (toml-mode . toml-ts-mode)
;;    (json-mode . json-ts-mode)
;;    (cmake-mode . cmake-ts-mode)
;;    (python-mode . python-ts-mode)
;;    (dockerfile-mode . dockerfile-ts-mode))))
)

(im/after bookmarks
 (setq-default bookmark-file emacs-bookmarks-file))

(im/after saveplace
 (setq-default save-place-file emacs-saveplace-file))
(save-place-mode)

(im/after savehist
 (setq-default savehist-file emacs-savehist-file))
(savehist-mode)

(im/after recentf
 ;; (setq-default recentf-auto-cleanup 'never)
 (setq-default recentf-save-file emacs-recentf-file)
 (setq-default recentf-max-menu-items 50)
 (setq-default recentf-max-saved-items 100)
 (setq-default recentf-exclude `(,emacs-elpa-dir ,emacs-var-dir)))
;; (im/hook kill-emacs-hook recentf-cleanup "recentf")
(recentf-mode)

(im/after help
 (setq-default help-window-select t))

(im/after window
 (setq-default switch-to-buffer-in-dedicated-window 'pop)
 (setq-default switch-to-buffer-obey-display-actions t)
 (setq-default split-height-threshold 160)
 (setq-default even-window-sizes 'width-only)
 (im/disable-popup "\\`\\*Compile-Log\\*.*\\'")
 (im/disable-popup "\\`\\*Native-compile-Log\\*.*\\'")
 (im/disable-popup "\\`\\*Async-native-compile-log\\*.*\\'")
 (im/disable-popup "\\`\\*Warnings\\*.*\\'")
 (setq-default switch-to-prev-buffer-skip-regexp '("\\`\\*.*\\'")))

;; Window
(im/key "<f12>"       delete-other-windows)
(im/key "<M-S-right>" next-buffer)
(im/key "<M-S-left>"  previous-buffer)

(im/after xref
 (setq-default xref-backend-functions '()))

(im/after fill
 (setq-default colon-double-space t)
 (setq-default default-justification 'left))

(im/after mouse
 (setq-default mouse-yank-at-point t))

(im/after simple
 ;; Hide commands in M-x that do not work in the current mode
 (setq-default read-extended-command-predicate #'command-completion-default-include-p)
 (setq-default undo-limit (* 1024 1024))
 (setq-default suggest-key-bindings 10)
 (setq-default save-interprogram-paste-before-kill t)
 (setq-default backward-delete-char-untabify-method 'hungry)
 (setq-default next-error-message-highlight t)
 (im/after files
  (im/hook before-save-hook delete-trailing-whitespace)))

(im/key "<mouse-4>" previous-line)
(im/key "<mouse-5>" next-line)

(im/after uniquify
 (setq-default uniquify-buffer-name-style 'forward))

(im/after tooltip
 (setq-default tooltip-use-echo-area t))

(im/after dictionary
 (setq-default dictionary-server "dict.org")
 (setq-default dictionary-use-single-buffer t))

(im/after woman
 (setq-default woman-fill-column 100))

(im/after vc
 (setq-default vc-make-backup-files t))

(im/after newcomment
 (setq-default comment-fill-column 80))

(im/after ediff-wind
 (setq-default ediff-split-window-function #'split-window-horizontally)
 (setq-default ediff-window-setup-function  'ediff-setup-windows-plain))

(im/after elec-pair
 (setq-default electric-pair-pairs '((?\[ . ?\]))))

(im/after display-line-numbers
 (setq-default display-line-numbers-grow-only t)
 (setq-default display-line-numbers-width-start t))

(im/after abbrev
 (im/dim abbrev-mode "Ab"))

(im/after whitespace
 (im/dim whitespace-mode "Ws")
 (setq-default whitespace-line-column 90)
 (setq-default show-trailing-whitespace nil)
 (setq-default whitespace-action '(cleanup))
 (setq-default whitespace-style
  '(face tabs lines-tail empty tab-mark indentation indentation::tab indentation::space
    space-after-tab space-after-tab::tab space-after-tab::space space-before-tab
    space-before-tab::tab space-before-tab::space whitespace-missing-newline-at-eof)))

(im/after proced
 (setq-default proced-auto-update-flag t)
 (setq-default proced-auto-update-interval 1)
 (setq-default proced-tree-flag t))

(im/after make-mode
 (im/hook makefile-mode-hook whitespace-mode))

(im/pkg symbol-overlay
 (im/after symbol-overlay
  (im/dim symbol-overlay-mode "Sy")
  (im/key-local "M->" symbol-overlay-jump-next symbol-overlay-mode-map)
  (im/key-local "M-<" symbol-overlay-jump-prev symbol-overlay-mode-map)
  (setq-default symbol-overlay-idle-time 0.1)))

(im/after elisp-mode
 (setq-default lisp-indent-offset 1)
 (setq-default lisp-indent-function #'common-lisp-indent-function)
 (im/hook emacs-lisp-mode-hook symbol-overlay-mode)
 (im/hook emacs-lisp-mode-hook whitespace-mode))

(im/mode "emacs" emacs-lisp-mode)
(im/mode ".config/emacs/init" emacs-lisp-mode)

(im/after eldoc
 (im/dim eldoc-mode "Ed")
 (setq-default eldoc-documentation-strategy 'eldoc-documentation-compose))

(im/after paren
 (setq-default show-paren-when-point-inside-paren t)
 (setq-default show-paren-style 'mixed)
 (setq-default show-paren-highlight-openparen t)
 (setq-default show-paren-context-when-offscreen 'overlay))

(im/pkg casual-dired)

(im/after dired
 (require 'dired-x)
 (require 'wdired)
 (require 'image-dired)
 (require 'casual-dired)
 (setq-default dired-mouse-drag-files t)
 (setq-default dired-listing-switches "-l --group-directories-first")
 (setq-default dired-hide-details-hide-symlink-targets nil)
 (im/hook dired-mode-hook dired-hide-details-mode "dired")
 (im/hook dired-mode-hook auto-revert-mode)
 (im/hook dired-mode-hook hl-line-mode)
 (im/hook dired-mode-hook context-menu-mode)
 (im/hook dired-mode-hook dired-async-mode)
 (im/hookn dired-mode-hook
  (setq-local mouse-1-click-follows-link 'double))
 (im/key-local "C-o" casual-dired-tmenu dired-mode-map))

(im/after autorevert
 (im/dim autorevert-mode "Ar")
 (setq-default auto-revert-interval 1)
 (setq-default auto-revert-avoid-polling t)
 (setq-default buffer-auto-revert-by-notification t)
 (setq-default auto-revert-mode-text " Ar"))

(im/after isearch
 (setq-default isearch-lazy-count t)
 (setq-default isearch-lazy-highlight t))

(im/after subword
 (im/dim subword-mode "Sw"))

(im/after flyspell
 (im/dim flyspell-mode "Fs")
 (setq-default ispell-program-name "aspell")
 (setq-default ispell-extra-args '("--sug-mode=ultra"))
 (setq-default ispell-local-dictionary "en_US"))

(im/after text-mode
 (im/hook text-mode-hook spell-fu-mode))

(im/after sh-script
 (setq-default sh-basic-offset 2)
 (setq-default sh-indentation 2)
 (im/hookn sh-mode-hook
  (im/hook after-save-hook executable-make-buffer-file-executable-if-script-p)))

(defmacro im/setup-c-style-comments ()
 "Setup C-style /* ... */ comments."
 `(im/after newcomment
   (setq-local comment-style 'extra-line)))

(im/after css-mode
 (im/hookn css-mode-hook (im/setup-c-style-comments)))

(im/after cc-mode
 (im/key-disable "(" c-mode-base-map)
 (setq-default c-doc-comment-style
  '((java-mode . javadoc)
    (c-mode    . gtkdoc)
    (c++-mode  . doxygen)))
 (im/hook c-mode-common-hook lsp))

(im/after c-ts-mode
 (im/hook c-ts-base-mode-hook lsp))

(im/after cc-vars
 (setq-default c-mark-wrong-style-of-comment t)
 (setq-default c-default-style '((other . "user")))
 (setq-default c-basic-offset 2)
 (im/hookn c-mode-common-hook (im/setup-c-style-comments)))

(im/after python
 (im/hook python-mode-hook lsp)
 (im/hook python-ts-mode-hook lsp)
 (im/hookn python-ts-mode-hook
  (setq-local fill-column 79)))

(im/after jit-lock
 (setq-default jit-lock-stealth-time 1)
 (setq-default jit-lock-chunk-size 5000)
 (setq-default jit-lock-antiblink-grace 1))

(im/after gdb-mi
 (setq-default gdb-many-windows t)
 (setq-default gdb-use-separate-io-buffer t)
 (advice-add 'gdb-setup-windows :after
  (lambda () (set-window-dedicated-p (selected-window) t))))

(im/after gud
 (im/hook gud-mode-hook gud-tooltip-mode)
 (setq-local gdb-restore-window-configuration-after-quit t))

(im/pkg markdown-mode)
(im/pkg crux)
(im/pkg indent-guide)
(im/pkg sed-mode)
(im/pkg po-mode)

(im/pkg toml-mode)
(im/pkg eldoc-toml
 (im/after eldoc-toml
  (im/dim eldoc-toml-mode))
 (im/after toml-mode
  (im/hook toml-mode-hook eldoc-toml-mode)))

(im/pkg cmake-mode)
(im/pkg eldoc-cmake
 (im/after cmake-mode
  (im/hook cmake-mode-hook eldoc-cmake-enable "eldoc-cmake")))

(im/pkg json-mode
 (im/hook json-mode-hook indent-guide-mode)
 (im/hook json-mode-hook tree-sitter-mode))

(im/pkg systemd
 (im/hook systemd-mode-hook company-mode))

(im/pkg highlight-defined
 (im/hook emacs-lisp-mode-hook highlight-defined-mode))

(im/pkg highlight-quoted
 (im/hook emacs-lisp-mode-hook highlight-quoted-mode))

(im/pkg eros
 (im/hook emacs-lisp-mode-hook eros-mode))

(im/pkg suggest)

(im/pkg ipretty
 (ipretty-mode))

(im/pkg elsa)

(im/pkg flycheck-elsa
 (im/hook emacs-lisp-mode-hook flycheck-elsa-setup))

(im/after org
 (setq-default org-startup-truncated nil)
 (setq-default org-startup-indented t)
 (setq-default org-todo-keywords
  `((sequence ,(char-to-string 9744) ,(char-to-string 9745))
    (sequence "TODO" "DONE")))
 (setq-default org-hide-leading-stars t)
 (setq-default org-ellipsis "…")
 (setq-default org-hide-emphasis-markers t)
 (setq-default org-pretty-entities nil)
 (setq-default org-fontify-whole-heading-line t)
 (setq-default org-fontify-done-headline t)
 (setq-default org-property-format "%s %s")
 (setq-default org-insert-heading-respect-content t)
 (setq-default org-catch-invisible-edits 'show-and-error)
 (setq-default org-auto-align-tags nil)
 (setq-default org-tags-column 0)
 (setq-default org-special-ctrl-a/e t)
 (setq-default org-special-ctrl-k t)
 (setq-default org-special-ctrl-o t)
 (im/key-local "C-c p" qol/generate-password org-mode-map "qol")
 (im/hook org-mode-hook org-bullets-mode)
 ;; (im/hook org-mode-hook spell-fu-mode)
 (im/hookn org-mode-hook
  (setq-local left-margin-width 2)
  (setq-local right-margin-width 2)
  (setq-local scroll-margin 0)))

(im/pkg org-bullets
 (im/after org-bullets
  (setq-default org-bullets-bullet-list
   `(,(char-to-string 8857)
     ,(char-to-string 8627)
     ,(char-to-string 8627)
     ,(char-to-string 8627)))))

(im/pkg which-key
 (im/after which-key
  (im/dim which-key-mode)
  (setq-default which-key-idle-delay 0.5)
  (setq-default which-key-show-docstrings nil)
  (setq-default which-key-add-column-padding 3)
  (setq-default which-key-max-description-length nil)
  (setq-default which-key-max-display-columns nil))
 (which-key-mode))

(im/pkg ivy
 (im/after ivy
  (im/dim ivy-mode)
  (im/key-local "<RET>" ivy-alt-done ivy-minibuffer-map "ivy")
  (setq-default ivy-wrap t)
  (setq-default ivy-use-selectable-prompt t)
  (setq-default ivy-use-virtual-buffers t)
  (setq-default ivy-count-format "(%d/%d) ")
  (setq-default ivy-virtual-abbreviate 'abbreviate)
  (setq-default ivy-initial-inputs-alist nil)
  (setq-default ivy-extra-directories nil)
  (setq-default ivy-re-builders-alist
   '((t . ivy--regex-ignore-order) (t . ivy--regex-plus))))
 (ivy-mode))

(im/pkg ctrlf
 (im/after ctrlf
  (setq-default ctrlf-default-search-style 'fuzzy)
  (setq-default ctrlf-auto-recenter t))
 (ctrlf-mode 1))

(im/pkg counsel
 (im/after counsel
  (im/key-local "M-Y" counsel-yank-pop counsel-mode-map)
  (im/dim counsel-mode)
  (put 'counsel-find-symbol 'no-counsel-M-x t))
 (counsel-mode))

(im/pkg ivy-rich
 (im/after ivy-rich
  (setq-default ivy-rich-path-style 'abbrev))
 (ivy-rich-mode))

;; (im/pkg swiper
;;  (im/key-remap isearch-forward  swiper-isearch)
;;  (im/key-remap isearch-backward swiper-isearch-backward)
;;  (im/key "C-c C-s" swiper-thing-at-point)
;;  (im/after swiper
;;   (setq-default swiper-include-line-number-in-search t)
;;   (setq-default swiper-action-recenter t)))

;; (im/pkg embark
;;  (im/after flyspell
;;   ;; Embark reserves this keybinding.
;;   (im/key-disable "C-." flyspell-mode-map))
;;  (im/key "C-." embark-act)
;;  (im/after embark
;;   (setq-default prefix-help-command #'embark-prefix-help-command)))

(im/pkg marginalia
 (marginalia-mode))

(im/pkg hotfuzz
 (im/after minibuffer
  (setq-default completion-styles '(hotfuzz))))

(im/pkg prescient
 (im/after prescient
  (setq-default prescient-save-file emacs-prescient-save-file)
  (setq-default prescient-sort-full-matches-first t)
  (eval-when-compile (defvar prescient-filter-method))
  (push 'literal-prefix prescient-filter-method)
  (push 'prefix prescient-filter-method)
  (push 'anchored prescient-filter-method)
  (im/after minibuffer
   (push 'prescient completion-styles)))
 (im/autoload prescient-persist-mode "prescient")
 (prescient-persist-mode +1))

(im/pkg ivy-prescient
 (ivy-prescient-mode))

(im/pkg orderless
 (im/after orderless
  (eval-when-compile (defvar orderless-matching-styles))
  (push 'orderless-initialism orderless-matching-styles)
  (push 'orderless-prefixes orderless-matching-styles))
 (im/after minibuffer
  (push 'orderless completion-styles)))

(im/after minibuffer
 (push 'substring completion-styles)
 (push 'flex completion-styles)
 (setq-default read-file-name-completion-ignore-case t)
 ;; (setq-default completion-category-defaults nil)
 ;; (setq-default completion-cycle-threshold 4)
 (setq-default completions-format 'one-column)
 ;; (setq-default completions-max-height 20)
 (setq-default completions-detailed t)
 ;; (setq-default set-message-functions '(set-multi-message))
 (im/after consult
  (setq-default completion-in-region-function #'consult-completion-in-region)))

(im/pkg flyspell-correct-ivy
 (im/after flyspell
  (im/key-local "C-;" flyspell-correct-wrapper flyspell-mode-map)
  (setq-default flyspell-correct-interface #'flyspell-correct-ivy)))

(im/pkg mwim
 (im/key-remap move-beginning-of-line mwim-beginning-of-code-or-line-or-comment)
 (im/key-remap move-end-of-line mwim-end-of-code-or-line))

(im/pkg expand-region
 (im/key "C-=" er/expand-region))

(im/pkg transient
 (im/after transient
  (setq-default transient-history-file (concat emacs-var-dir "transient-history"))
  (setq-default transient-default-level 7)))

(im/pkg blamer
 (im/after blamer
  (setq-default blamer-idle-time 0)
  (setq-default blamer-commit-formatter ": %s")
  (setq-default blamer-datetime-formatter "%s")
  (setq-default blamer-max-commit-message-length 60))
 (im/after prog-mode
  (im/key-local "C-c b" blamer-mode prog-mode-map)))

(im/pkg magit
 (im/key "C-x g" magit-status)
 (im/after magit-mode
  (setq-default magit-log-section-commit-count 20)
  (setq-default magit-auto-revert-tracked-only nil)
  ;; (setq-default magit-display-buffer-function
  ;;  'magit-display-buffer-same-window-except-diff-v1)
  (setq-default magit-display-buffer-function
   'magit-display-buffer-fullframe-status-v1)
  (setq-default magit-bury-buffer-function 'magit-restore-window-configuration)
  (setq-default magit-repository-directories '(("~/Workspace" . 3)))
  (im/hook after-save-hook magit-after-save-refresh-status "magit"))
 (im/after magit-diff
  (setq-default magit-revision-show-gravatars t)
  (setq-default magit-revision-fill-summary-line fill-column)))

(im/pkg projectile
 (im/after projectile
  (im/key-local "C-x p" projectile-command-map projectile-mode-map "projectile")
  (im/dim projectile-mode "Pr")
  (setq-default projectile-cache-file emacs-projectile-cache-file)
  (setq-default projectile-known-projects-file emacs-projectile-projects-file)
  (setq-default projectile-project-search-path '("~/Workspace"))
  (setq-default projectile-sort-order 'recently-active)
  (setq-default projectile-enable-caching nil)
  (setq-default projectile-require-project-root nil)))

(im/pkg counsel-projectile
 (im/after counsel-projectile
  (im/key-local "M-G" counsel-projectile-git-grep projectile-mode-map))
 (counsel-projectile-mode))

(im/pkg deadgrep
 (im/key "M-F" deadgrep)
 (im/after deadgrep
  ;; (im/key-local "<f5>" deadgrep-edit-mode deadgrep-mode-map "deadgrep")
  ;; (im/key-local "<f5>" deadgrep-mode deadgrep-edit-mode-map "deadgrep")))
  (require 'wgrep-deadgrep)))

(im/pkg wgrep-deadgrep)

(im/pkg yasnippet-snippets
 (im/after yasnippet
  (yasnippet-snippets-initialize)))

(im/pkg yasnippet
 (im/after yasnippet
  (im/dim yas-minor-mode "Ys")
  (im/after company
   (im/hookn yas-minor-mode-hook (im/company-add-backend 'company-yasnippet)))))

(im/pkg diff-hl
 (im/after diff-hl
  (setq-default diff-hl-draw-borders nil)
  (setq-default diff-hl-flydiff-delay 0.1))
 (im/after magit-mode
  (im/hook magit-pre-refresh-hook diff-hl-magit-pre-refresh "diff-hl")
  (im/hook magit-post-refresh-hook diff-hl-magit-post-refresh "diff-hl")))

(im/pkg multiple-cursors
 (im/key "C-c C-v"       mc/edit-lines)
 (im/key "C->"           mc/mark-next-like-this)
 (im/key "C-<"           mc/mark-previous-like-this)
 (im/key "C-S-<mouse-1>" mc/add-cursor-on-click)
 (im/after multiple-cursors-core
  (setq-default mc/always-run-for-all t)))

(im/pkg volatile-highlights
 (im/after volatile-highlights
  (im/dim volatile-highlights-mode "Vh"))
 (volatile-highlights-mode))

(im/pkg yaml-mode
 (im/after yaml-mode
  (im/key-local "C-c p" im/generate-password yaml-mode-map "qol")
  (im/hook yaml-mode-hook flycheck-mode)
  (im/hook yaml-mode-hook tree-sitter-mode))
 (im/mode "clang-format" yaml-mode))

(im/mode ".ll" llvm-mode "llvm-mode")

(im/pkg autodisass-llvm-bitcode)
(im/mode ".bc" autodisass-llvm-bitcode "autodisass-llvm-bitcode")

(im/pkg demangle-mode
 (im/after llvm-mode
  (im/hook llvm-mode-hook demangle-mode)))

(im/pkg hledger-mode
 (im/after hledger-mode
  (setq-default hledger-currency-string "EUR")
  (setq-default hledger-current-overlay t)
  (setq-default hledger-comments-column 1)
  (im/hookn hledger-mode-hook
   (setq-local tab-width 1)
   (im/after flycheck
    (eval-when-compile (require 'flycheck-hledger))))
  (im/hook hledger-mode-hook whitespace-mode)
  (im/hook hledger-mode-hook symbol-overlay-mode)
  (im/hook hledger-mode-hook flycheck-mode))
 (im/mode ".journal" hledger-mode)
 (im/mode ".ledger"  hledger-mode))

(im/pkg flycheck-hledger)

(im/pkg flycheck
 (im/after flycheck
  (im/autoload flycheck-next-error "flycheck")
  (im/autoload flycheck-previous-error "flycheck")
  (im/key-local "M-n" flycheck-next-error     flycheck-mode-map "flycheck")
  (im/key-local "M-p" flycheck-previous-error flycheck-mode-map "flycheck")
  (setq-default flycheck-checker-error-threshold nil)
  (setq-default flycheck-mode-line-prefix "Fc")
  (setq-default flycheck-check-syntax-automatically
   '(idle-change new-line mode-enabled idle-buffer-switch))
  (setq-default flycheck-idle-change-delay 0.25)
  (setq-default flycheck-idle-buffer-switch-delay 0.25)
  (im/hook flycheck-mode-hook flycheck-posframe-mode)))

(im/pkg flycheck-posframe
 (im/after flycheck-posframe
  ;; (flycheck-posframe-configure-pretty-defaults)
  (setq-default flycheck-posframe-position 'window-bottom-left-corner)
  (setq-default flycheck-posframe-border-width 1)
  (setq-default flycheck-posframe-prefix
   (concat " " (char-to-string 8618) " Info: "))
  (setq-default flycheck-posframe-warnings-prefix
   (concat " " (char-to-string 9888) " Warning: "))
  (setq-default flycheck-posframe-error-prefix
   (concat " " (char-to-string 10540) " Error: "))
  (im/after company
   (im/hook flycheck-posframe-inhibit-functions company--active-p "company")
   (im/hook flycheck-posframe-inhibit-functions
    (lambda (&rest _) (bound-and-true-p company-backend))))))

(im/pkg consult-flycheck
 (im/after flycheck
  (im/key-local "C-c ! a" consult-flycheck flycheck-mode-map)))

(im/pkg company
 (im/after company
  (im/dim company-mode "Co")
  (setq-default company-backends '((company-capf)))
  (setq-default company-idle-delay 0.5)
  (setq-default company-keywords-ignore-case t)
  (setq-default company-minimum-prefix-length 2)
  (setq-default company-selection-wrap-around t)
  (setq-default company-tooltip-align-annotations t)
  (im/key-local "<tab>" company-indent-or-complete-common company-mode-map "company")))

(defun im/company-add-backend (backend)
 "Add BACKEND to local copy of `company-backends'."
 (eval-when-compile (defvar company-backends))
 (im/autoload qol/append "qol")
 (qol/append (car company-backends) backend))

(im/pkg company-posframe
 (im/after company-posframe
  (im/dim company-posframe-mode)
  (setq-default company-posframe-quickhelp-x-offset 2))
 (im/after company
  (im/hook company-mode-hook company-posframe-mode "company-posframe")))

(im/pkg company-prescient
 (im/after company
  (im/hook company-mode-hook company-prescient-mode)))

(im/pkg tree-sitter-langs
 (im/after tree-sitter-mode
  (im/hook tree-sitter-mode-hook tree-sitter-langs-install-grammars)))

(im/pkg tree-sitter
 (im/after tree-sitter
  (im/dim tree-sitter-mode "Ts")
  (im/hook tree-sitter-mode-hook tree-sitter-hl-mode)))

;; (im/pkg scopeline
;;  (im/after tree-sitter
;;   (im/hook tree-sitter-mode-hook scopeline-mode))
;;  (im/after scopeline
;;   (im/dim scopeline-mode "Sl")
;;   (setq-default scopeline-min-lines 10)))

(im/after prog-mode
 (im/hook prog-mode-hook diff-hl-mode)
 (im/hook prog-mode-hook eldoc-mode)
 (im/hook prog-mode-hook show-paren-mode)
 (im/hook prog-mode-hook flyspell-prog-mode)
 (im/hook prog-mode-hook flycheck-mode)
 (im/hook prog-mode-hook yas-minor-mode)
 (im/hook prog-mode-hook company-mode)
 (im/hook prog-mode-hook electric-pair-mode)
 (im/hook prog-mode-hook electric-layout-mode)
 (im/hook prog-mode-hook display-line-numbers-mode)
 (im/hook prog-mode-hook hl-line-mode)
 (im/hook prog-mode-hook bug-reference-prog-mode))

(im/after conf-mode
 (im/hook conf-desktop-mode-hook diff-hl-mode)
 (im/hook conf-desktop-mode-hook show-paren-mode)
 (im/hook conf-desktop-mode-hook flyspell-prog-mode)
 (im/hook conf-desktop-mode-hook electric-pair-mode)
 (im/hook conf-desktop-mode-hook electric-layout-mode)
 (im/hook conf-desktop-mode-hook display-line-numbers-mode)
 (im/hook conf-desktop-mode-hook hl-line-mode))

(im/pkg spell-fu
 (im/autoload spell-fu-dictionary-add "spell-fu")
 (im/autoload spell-fu-get-ispell-dictionary "spell-fu")
 (im/autoload spell-fu-get-personal-dictionary "spell-fu")
 (im/after spell-fu
  (im/hookn spell-fu-mode-hook
   (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
   (spell-fu-dictionary-add (spell-fu-get-personal-dictionary "en-personal" user-dict-en))
   (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "de")))
  (setq-default spell-fu-faces-exclude '(link org-link))))

(im/pkg meson-mode
 (im/after meson-mode
  (im/after company
   (im/hookn meson-mode-hook
    (im/company-add-backend 'company-dabbrev-code)))
  (im/hook meson-mode-hook symbol-overlay-mode)
  (im/hook meson-mode-hook company-mode)))

(im/pkg rust-mode
 (im/after rust-mode
  (im/key-local "<f5>" rust-dbg-wrap-or-unwrap            rust-mode-map "rust-utils")
  (im/key-local "<f6>" lsp-rust-analyzer-expand-macro     rust-mode-map "lsp-rust")
  (im/key-local "<f7>" lsp-rust-analyzer-join-lines       rust-mode-map "lsp-rust")
  (im/key-local "<f8>" lsp-rust-analyzer-inlay-hints-mode rust-mode-map "lsp-rust")
  (setq-default rust-indent-offset 2)
  (setq-default rust-load-optional-libraries nil)
  (setq-default rust-format-on-save t)
  (im/hookn rust-mode-hook (electric-quote-local-mode -1))
  (im/hook rust-mode-hook subword-mode)
  (im/hook rust-mode-hook lsp)))

(im/after rust-ts-mode
 (im/key-local "<f5>" rust-dbg-wrap-or-unwrap            rust-ts-mode-map "rust-utils")
 (im/key-local "<f6>" lsp-rust-analyzer-expand-macro     rust-ts-mode-map "lsp-rust")
 (im/key-local "<f7>" lsp-rust-analyzer-join-lines       rust-ts-mode-map "lsp-rust")
 (im/key-local "<f8>" lsp-rust-analyzer-inlay-hints-mode rust-ts-mode-map "lsp-rust")
 (setq-default rust-ts-mode-indent-offset 2)
 (im/hookn rust-ts-mode-hook (electric-quote-local-mode -1))
 (im/hook rust-ts-mode-hook subword-mode)
 (im/hook rust-ts-mode-hook lsp))

(im/pkg lsp-mode
 (im/after lsp-mode
  (im/dim lsp-mode "Ls")
  (im/key-local "C-c f" lsp-format-buffer           lsp-mode-map "lsp-mode")
  (im/key-local "C-c g" lsp-format-region           lsp-mode-map "lsp-mode")
  (im/key-local "C-c r" lsp-rename                  lsp-mode-map "lsp-mode")
  (im/key-local "C-c h" lsp-describe-thing-at-point lsp-mode-map "lsp-mode")
  (im/key-local "C-="   lsp-extend-selection        lsp-mode-map "lsp-mode")
  (im/key-local "M-RET" lsp-execute-code-action     lsp-mode-map "lsp-mode")
  (setq-default lsp-progress-prefix "  Progress: ")
  (setq-default lsp-completion-show-detail t)
  (setq-default lsp-completion-show-kind t)
  (setq-default lsp-completion-provider :none)
  (setq-default lsp-headerline-breadcrumb-enable t)
  (setq-default lsp-restart 'auto-restart)
  (setq-default lsp-enable-snippet t)
  (setq-default lsp-keymap-prefix "C-c")
  (setq-default lsp-idle-delay 0.1)
  (setq-default lsp-file-watch-threshold nil)
  (setq-default lsp-enable-semantic-highlighting t)
  (setq-default lsp-enable-indentation t)
  (setq-default lsp-enable-on-type-formatting nil)
  (setq-default lsp-before-save-edits nil)
  (setq-default lsp-auto-configure t)
  (setq-default lsp-signature-auto-activate t)
  (setq-default lsp-signature-render-documentation nil)
  (setq-default lsp-eldoc-enable-hover t)
  (setq-default lsp-eldoc-render-all nil)
  (setq-default lsp-modeline-code-actions-enable nil)
  (setq-default lsp-modeline-diagnostics-enable t)
  (setq-default lsp-log-io nil)
  (setq-default lsp-keep-workspace-alive nil)
  (setq-default lsp-enable-imenu nil)
  (im/after which-key
   (im/hook lsp-mode-hook lsp-enable-which-key-integration "lsp-mode")))
 (im/after lsp-lens
  (im/dim lsp-lens-mode)
  (setq-default lsp-lens-mode nil)
  (setq-default lsp-lens-enable nil))
 (im/after lsp-headerline
  (setq-default lsp-headerline-breadcrumb-icons-enable nil))
 (im/after lsp-semantic-tokens
  (setq-default lsp-semantic-tokens-apply-modifiers t))
 (im/after lsp-rust
  ;; (setq-default lsp-rust-analyzer-max-inlay-hint-length 50)
  ;; (setq-default lsp-rust-unstable-features t)
  (setq-default lsp-rust-analyzer-checkonsave-features "all")
  (setq-default lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (setq-default lsp-rust-analyzer-proc-macro-enable t)
  (setq-default lsp-rust-racer-completion nil)
  (setq-default lsp-rust-build-bin t)
  (setq-default lsp-rust-build-lib t)
  (setq-default lsp-rust-clippy-preference "on")
  (setq-default lsp-rust-analyzer-server-display-inlay-hints t)
  (setq-default lsp-rust-analyzer-display-chaining-hints t)
  (setq-default lsp-rust-analyzer-display-parameter-hints t)
  (setq-default lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-enable "always")
  (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (setq-default lsp-rust-analyzer-binding-mode-hints t)
  (setq-default lsp-rust-analyzer-display-reborrow-hints "mutable")
  (setq-default lsp-rust-all-features t)
  (setq-default lsp-rust-all-targets t)
  (setq-default lsp-rust-full-docs t)
  (setq-default lsp-rust-analyzer-cargo-watch-command "clippy"))
 (im/after lsp-clangd
  (setq-default lsp-clients-clangd-args
   '("--header-insertion-decorators"
     "--all-scopes-completion"
     "--clang-tidy"
     "--completion-style=detailed"
     "--header-insertion=iwyu"
     ;; Breaks clangd-14
     ; "--header-insertion-decorators"
     "--inlay-hints"
     "-j=8"
     "--malloc-trim"
     "--pch-storage=memory"
     "--background-index"
     "--function-arg-placeholders"
     "--limit-references=0"
     "--limit-results=0"))
  (im/after cc-mode
   (im/autoload lsp-clangd-find-other-file "lsp-clangd")
   (im/key-local "<f2>" lsp-clangd-find-other-file c-mode-base-map))
  (im/after c-ts-mode
   (im/autoload lsp-clangd-find-other-file "lsp-clangd")
   (im/key-local "<f2>" lsp-clangd-find-other-file c-ts-base-mode-map))))

(im/pkg lsp-ivy
 (im/after lsp-mode
  (im/key-local "C-c x" lsp-ivy-workspace-symbol lsp-mode-map)))

(im/pkg treemacs
 (im/key "<f9>" treemacs-select-window)
 (im/after treemacs-customization
  (setq-default treemacs-width 40)
  (setq-default treemacs-indentation 1))
 (im/after treemacs
  (setq-default treemacs-select-when-already-in-treemacs 'move-back))
  ;; (setq-default treemacs-indent-guide-mode t))
 (im/after treemacs-interface
  (im/key "<f12>" treemacs-delete-other-windows "treemacs-interface"))
 ;; (im/after treemacs-header-line
 ;;  (setq-default treemacs-indicate-top-scroll-mode t))
 (im/after treemacs-mode
  (im/hook treemacs-mode-hook treemacs-tag-follow-mode "treemacs-tag-follow-mode")
  (im/hook treemacs-mode-hook treemacs-fringe-indicator-mode "treemacs-fringe-indicator")
  (im/hook treemacs-mode-hook treemacs-filewatch-mode "treemacs-filewatch-mode")
  ;; (im/hook treemacs-mode-hook treemacs-indicate-top-scroll-mode "treemacs-header-line")
  ;; (im/autoload treemacs-indent-guide-mode "treemacs-visuals")
  ;; (im/hookn treemacs-mode-hook (treemacs-indent-guide-mode))
  (im/autoload treemacs-git-mode "treemacs-async")
  (im/hookn treemacs-mode-hook (treemacs-git-mode 'deferred))
  (im/hook treemacs-mode-hook
   treemacs-git-commit-diff-mode
   "treemacs-git-commit-diff-mode")))

(im/pkg lsp-treemacs
 (im/after lsp-mode
  (im/key-local "C-c e" lsp-treemacs-errors-list    lsp-mode-map)
  (im/key-local "C-c s" lsp-treemacs-symbols        lsp-mode-map)
  (im/key-local "C-c c" lsp-treemacs-call-hierarchy lsp-mode-map)
  (im/key-local "C-c t" lsp-treemacs-type-hierarchy lsp-mode-map)
  (im/hook lsp-mode-hook lsp-treemacs-sync-mode)))

(im/pkg treemacs-projectile)

(im/pkg lsp-ui
 (im/after lsp-ui-doc
  (setq-default lsp-ui-doc-enable t)
  (setq-default lsp-ui-doc-show-with-cursor nil)
  (setq-default lsp-ui-doc-show-with-mouse t)
  (setq-default lsp-ui-doc-alignment 'frame)
  (setq-default lsp-ui-doc-header t)
  (setq-default lsp-ui-doc-include-signature t)
  (setq-default lsp-ui-doc-max-height 30)
  (setq-default lsp-ui-doc-use-webkit t))
 (im/after lsp-ui-peek
  (setq-default lsp-ui-peek-list-width 40)
  (setq-default lsp-ui-peek-always-show t))
 (im/after lsp-ui-sideline
  (setq-default lsp-ui-sideline-enable nil))
 (im/after lsp-ui
  (im/key-local "M-."   lsp-ui-peek-find-definitions    lsp-ui-mode-map "lsp-ui-peek")
  (im/key-local "M-?"   lsp-ui-peek-find-references     lsp-ui-mode-map "lsp-ui-peek")
  (im/key-local "M-I"   lsp-ui-peek-find-implementation lsp-ui-mode-map "lsp-ui-peek")
  (im/key-local "C-c d" lsp-ui-doc-show                 lsp-ui-mode-map "lsp-ui-doc")
  (im/key-local "C-c l" lsp-ui-flycheck-list            lsp-ui-mode-map "lsp-ui-flycheck")))

(im/pkg web-mode
 (im/mode ".html" web-mode)
 (im/mode ".css" web-mode)
 (im/mode ".js" web-mode)
 (im/after web-mode
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-enable-current-column-highlight t)
  (setq-default web-mode-enable-current-element-highlight t)
  (setq-default web-mode-auto-close-style 3)
  (setq-default web-mode-enable-auto-expanding t)
  (im/hook web-mode-hook lsp)
  (im/hookn web-mode-hook (setq-local tab-width 2))))

(im/pkg company-web
 (im/after web-mode
  (im/after company
   (im/hookn web-mode-hook
    (im/company-add-backend 'company-css)
    (im/company-add-backend 'company-web-html)))))

(im/pkg emmet-mode
 (setq-default emmet-indentation 2)
 (im/after web-mode
  (im/hook web-mode-hook emmet-mode)))

(im/pkg dockerfile-mode)
(im/pkg pkgbuild-mode)

(im/pkg vterm
 (im/after vterm
  (setq-default vterm-max-scrollback 100000)))

(im/pkg sideline
 (im/after sideline
  (im/dim sideline-mode "Si")
  (setq-default sideline-delay 0.1)))

(im/pkg sideline-blame
 (im/after sideline
  (setq-default sideline-backends-right '(sideline-blame))
  (setq-default sideline-blame-commit-format "- %s")))

(im/pkg buffer-move
 (im/key "C-x m" buf-move))

;; (im/pkg dirvish
;;  (dirvish-override-dired-mode))

;; (im/pkg popper
;;  (im/after popper
;;   (setq-default popper-reference-buffers '("\\*deadgrep.*$" "\\*Occur\\*$"))
;;   (im/autoload popper-group-by-projectile "popper")
;;   (setq-default popper-group-function #'popper-group-by-projectile)
;;   (im/key "C-`" popper-cycle "popper")
;;   (im/key-local "C-~" popper-toggle-type popper-mode-map "popper"))
;;  (popper-mode)
;;  (popper-echo-mode))

(im/pkg surround
 (require 'surround)
 (im/key "M-'" surround-mark-inner)
 (im/key "M-\"" surround-insert))

;; Print startup stats.
(message "Startup in %s (%d GC runs)" (emacs-init-time) gcs-done)

(provide 'init)
;;; init.el ends here

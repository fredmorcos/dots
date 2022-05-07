;;; init --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
 (defconst emacs-dots-dir "/home/fred/Workspace/dots/emacs/")
 (push emacs-dots-dir load-path))

(require 'init-macros)

(fm/key "C-x j"    fm/insert-buffer-name "qol")

(fm/key "C-x e"    fm/replace-escapes "qol")
(fm/key "<M-up>"   fm/move-line-up    "qol")
(fm/key "<M-down>" fm/move-line-down  "qol")

(fm/key "{"  fm/insert-pair-curly         "qol")
(fm/key "("  fm/insert-pair-parens        "qol")
(fm/key "'"  fm/insert-pair-quote         "qol")
(fm/key "\"" fm/insert-pair-double-quotes "qol")
(fm/key "`"  fm/insert-pair-backtick      "qol")

;; Directories.
(defconst emacs-user-dir (expand-file-name user-emacs-directory))
(defconst emacs-elpa-dir (concat emacs-user-dir "elpa"))
(defconst emacs-var-dir (concat emacs-user-dir "var/"))
(defconst emacs-recentf-file (concat emacs-var-dir "recentf"))
(defconst emacs-saveplace-file (concat emacs-var-dir "saveplace"))
(defconst emacs-savehist-file (concat emacs-var-dir "savehist"))
(defconst emacs-package-qs-file (concat emacs-var-dir "package-qs"))
(defconst emacs-projectile-cache-file (concat emacs-var-dir "projectile-cache"))
(defconst emacs-prescient-save-file (concat emacs-var-dir "prescient-save"))
(defconst emacs-tmp-dir (concat temporary-file-directory "emacs/"))
(defconst emacs-autosaves-dir (concat emacs-tmp-dir "autosaves"))
(defconst emacs-autosaves-pat (concat emacs-autosaves-dir "/\\1"))
(defconst emacs-autosave-list-prefix (concat emacs-tmp-dir "auto-save-list/.saves-"))
(defconst emacs-backups-dir (concat emacs-tmp-dir "backups"))
(defconst emacs-backups-pat (concat emacs-backups-dir "/"))
(make-directory emacs-var-dir t)
(make-directory emacs-autosaves-dir t)
(make-directory emacs-backups-dir t)

;; Do not show a message in the echo area after startup.
(fset 'display-startup-echo-area-message 'ignore)

;; Startup.
(setq-default inhibit-startup-screen t)
(setq-default inhibit-startup-message t)
(setq-default inhibit-startup-buffer-menu t)
(setq-default initial-scratch-message nil)
(setq-default initial-major-mode 'fundamental-mode)

;; Auto-save.
(setq-default auto-save-list-file-prefix emacs-autosave-list-prefix)

;; subr - Respond to yes/no questions using Y/N.
(if (< emacs-major-version 28)
 (defalias 'yes-or-no-p 'y-or-n-p)
 (setq-default use-short-answers t))

;; Windmove.
(windmove-default-keybindings)
(windmove-delete-default-keybindings)

;; Common User Access.
(cua-selection-mode 1)

;; Cursor.
(fm/after frame
 (blink-cursor-mode -1))

;; Indent.
(setq-default tab-always-indent 'complete)

;; Bindings.
(setq-default column-number-indicator-zero-based nil)

;; Modeline
(setq-default mode-line-compact 'long)

;; Fill-column indicator
(global-display-fill-column-indicator-mode)

;; Make URLs clickable
(global-goto-address-mode)

(fm/after emacs
 ;; Avoid graphical dialog boxes.
 (setq-default use-dialog-box nil)

 ;; Completion.
 (setq-default completion-ignore-case t)
 (setq-default read-buffer-completion-ignore-case t)

 ;; Fill.
 (setq-default fill-column 90)

 ;; Indent.
 (setq-default indent-tabs-mode nil)

 ;; History/savehist.
 (setq-default history-delete-duplicates t)
 (setq-default history-length 150)

 ;; Scrolling.
 (setq-default scroll-conservatively 4)
 (setq-default scroll-margin 3)
 (setq-default hscroll-margin 3)
 (setq-default hscroll-step 1)
 (setq-default auto-hscroll-mode 'current-line)
 (setq-default fast-but-imprecise-scrolling t)

 ;; External Processes.
 (setq-default read-process-output-max (* 1024 1024)))

(fm/key-interactive "<f10>" (scroll-other-window 1))
(fm/key-interactive "<f11>" (scroll-other-window-down 1))

(fm/after files
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
 (setq-default coding-system-for-write 'utf-8-unix))

(fm/after saveplace
 (setq-default save-place-file emacs-saveplace-file))
(save-place-mode)

(fm/after savehist
 (setq-default savehist-file emacs-savehist-file))
(savehist-mode)

(fm/after recentf
 (setq-default recentf-auto-cleanup 'never)
 (setq-default recentf-save-file emacs-recentf-file)
 (setq-default recentf-max-menu-items 50)
 (setq-default recentf-max-saved-items 100)
 (setq-default recentf-exclude `(,emacs-elpa-dir ,emacs-var-dir)))
(fm/hook kill-emacs-hook recentf-cleanup "recentf")
(recentf-mode)

(fm/after help
 (setq-default help-window-select t))

(fm/after window
 (setq-default split-height-threshold 160)
 (setq-default even-window-sizes 'width-only)
 (fm/disable-popup "\\`\\*Compile-Log\\*.*\\'")
 (fm/disable-popup "\\`\\*Native-compile-Log\\*.*\\'")
 (fm/disable-popup "\\`\\*Async-native-compile-Log\\*.*\\'")
 (fm/disable-popup "\\`\\*Warnings\\*.*\\'"))

(fm/key "<f12>"       delete-other-windows)
(fm/key "<M-S-right>" next-buffer)
(fm/key "<M-S-left>"  previous-buffer)

(fm/after xref
 (setq-default xref-backend-functions '()))

(fm/after fill
 (setq-default colon-double-space t)
 (setq-default default-justification 'left))

(fm/after mouse
 (setq-default mouse-yank-at-point t))

(fm/after simple
 (setq-default undo-limit (* 1024 1024))
 (setq-default suggest-key-bindings 10)
 (setq-default save-interprogram-paste-before-kill t)
 (setq-default backward-delete-char-untabify-method 'hungry)
 (setq-default next-error-message-highlight t)
 (fm/after files
  (fm/hook before-save-hook delete-trailing-whitespace)))

(fm/key "<mouse-4>" previous-line)
(fm/key "<mouse-5>" next-line)

(fm/after uniquify
 (setq-default uniquify-buffer-name-style 'forward))

(fm/after vc
 (setq-default vc-make-backup-files t))

(fm/after newcomment
 (setq-default comment-fill-column 80))

(fm/after ediff-wind
 (setq-default ediff-split-window-function #'split-window-horizontally)
 (setq-default ediff-window-setup-function  'ediff-setup-windows-plain))

(fm/after elec-pair
 (setq-default electric-pair-pairs '((?\[ . ?\]))))

(fm/after display-line-numbers
 (setq-default display-line-numbers-grow-only t)
 (setq-default display-line-numbers-width-start t))

(fm/after abbrev
 (fm/dim abbrev-mode "Ab"))

(fm/after whitespace
 (fm/dim whitespace-mode "Ws")
 (setq-default whitespace-line-column 90)
 (setq-default show-trailing-whitespace nil)
 (setq-default whitespace-action '(cleanup))
 (setq-default whitespace-style '(face tabs lines-tail empty
  tab-mark indentation indentation::tab indentation::space
  space-after-tab space-after-tab::tab space-after-tab::space
  space-before-tab space-before-tab::tab
  space-before-tab::space whitespace-missing-newline-at-eof)))

(fm/after make-mode
 (fm/hook makefile-mode-hook whitespace-mode))

(fm/pkg symbol-overlay
 (fm/after symbol-overlay
  (fm/dim symbol-overlay-mode "Sy")
  (fm/key-local "M->" symbol-overlay-jump-next symbol-overlay-mode-map)
  (fm/key-local "M-<" symbol-overlay-jump-prev symbol-overlay-mode-map)
  (setq-default symbol-overlay-idle-time 0.1)))

(fm/after elisp-mode
 (setq-default lisp-indent-offset 1)
 (setq-default lisp-indent-function #'common-lisp-indent-function)
 (fm/hook emacs-lisp-mode-hook symbol-overlay-mode)
 (fm/hook emacs-lisp-mode-hook whitespace-mode))

(fm/mode "emacs" emacs-lisp-mode)
(fm/mode ".config/emacs/init" emacs-lisp-mode)

(fm/mode "Passwords.txt" text-mode)
(fm/mode "Passwords_old.txt" text-mode)

(fm/after eldoc
 (fm/dim eldoc-mode "Ed")
 (setq-default eldoc-documentation-strategy 'eldoc-documentation-compose))

(fm/after paren
 (setq-default show-paren-when-point-inside-paren t)
 (setq-default show-paren-style 'mixed)
 (setq-default show-paren-highlight-openparen t))

(fm/after dired
 (setq-default dired-listing-switches "-l --group-directories-first")
 (setq-default dired-hide-details-hide-symlink-targets nil)
 (fm/hook dired-mode-hook dired-hide-details-mode "dired"))

(fm/after autorevert
 (fm/dim autorevert-mode "Ar")
 (setq-default auto-revert-interval 1)
 (setq-default auto-revert-avoid-polling t)
 (setq-default buffer-auto-revert-by-notification t)
 (setq-default auto-revert-mode-text " Ar"))

(fm/after dired
 (fm/hook dired-mode-hook auto-revert-mode))

(fm/after subword
 (fm/dim subword-mode "Sw"))

(fm/after flyspell
 (fm/dim flyspell-mode "Fs")
 (setq-default ispell-program-name "aspell")
 (setq-default ispell-extra-args '("--sug-mode=ultra")))

(fm/after text-mode
 (fm/hook text-mode-hook flyspell-mode))

(fm/after sh-script
 (fm/hookn sh-mode-hook
  (fm/hook after-save-hook executable-make-buffer-file-executable-if-script-p)))

(defmacro fm/setup-c-style-comments ()
 "Setup C-style /* ... */ comments."
 `(fm/after newcomment
   (setq-local comment-style 'extra-line)))

(fm/after css-mode
 (fm/hookn css-mode-hook (fm/setup-c-style-comments)))

(fm/after cc-mode
 (fm/key-disable "(" c-mode-base-map)
 (setq-default c-doc-comment-style
  '((java-mode . javadoc)
    (c-mode    . gtkdoc)
    (c++-mode  . doxygen)))
 ;; (fm/hook c-mode-common-hook tree-sitter-mode)
 (fm/hook c-mode-common-hook lsp))

(fm/after cc-vars
 (setq-default c-mark-wrong-style-of-comment t)
 (setq-default c-default-style '((other . "user")))
 (setq-default c-basic-offset 2)
 (fm/hookn c-mode-common-hook (fm/setup-c-style-comments)))

(fm/after jit-lock
 (setq-default jit-lock-stealth-time 1)
 (setq-default jit-lock-chunk-size 5000)
 (setq-default jit-lock-antiblink-grace 1))

(fm/after gdb-mi
 (setq-default gdb-many-windows t)
 (setq-default gdb-use-separate-io-buffer t)
 (advice-add 'gdb-setup-windows :after
  (lambda () (set-window-dedicated-p (selected-window) t))))

(fm/after gud
 (fm/hook gud-mode-hook gud-tooltip-mode)
 (setq-local gdb-restore-window-configuration-after-quit t))

(fm/pkg toml-mode)
(fm/pkg markdown-mode)
(fm/pkg crux)
(fm/pkg indent-guide)
(fm/pkg sed-mode)
(fm/pkg po-mode)

(fm/pkg json-mode
 (fm/hook json-mode-hook indent-guide-mode))

(fm/pkg systemd
 (fm/hook systemd-mode-hook company-mode))

(fm/pkg org-bullets
 (setq-default org-bullets-bullet-list '(" ")))

(fm/after org
 (setq-default org-cycle-separator-lines 0)
 (setq-default org-startup-folded 'content)
 (setq-default org-ellipsis "…")
 (setq-default org-hide-leading-stars t)
 (setq-default org-hide-emphasis-markers t)
 (setq-default org-fontify-whole-heading-line t)
 (setq-default org-fontify-done-headline t)
 (setq-default org-startup-indented t)
 (setq-default org-property-format "%s %s")
 (fm/key-local "C-c p" fm/generate-password org-mode-map "qol")
 (fm/hook org-mode-hook org-bullets-mode)
 (fm/hookn org-mode-hook
  (setq-local left-margin-width 2)
  (setq-local right-margin-width 2)
  (setq-local scroll-margin 0)
  (setq-local cursor-type 'bar)))

(fm/pkg which-key
 (fm/after which-key
  (fm/dim which-key-mode)
  (setq-default which-key-idle-delay 0.5))
 (which-key-mode))

(fm/pkg ivy
 (fm/after ivy
  (fm/dim ivy-mode)
  (fm/key-local "<RET>" ivy-alt-done ivy-minibuffer-map "ivy")
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

(fm/pkg counsel
 (fm/after counsel
  (fm/key-local "M-Y" counsel-yank-pop counsel-mode-map)
  (fm/dim counsel-mode)
  (put 'counsel-find-symbol 'no-counsel-M-x t))
 (counsel-mode))

(fm/pkg ivy-rich
 (fm/after ivy-rich
  (setq-default ivy-rich-path-style 'abbrev))
 (ivy-rich-mode))

(fm/pkg swiper
 (fm/key "C-s"         swiper-isearch)
 (fm/key "C-c C-s"     swiper-thing-at-point)
 (fm/key "C-r"         swiper-isearch-backward)
 (fm/after swiper
  (setq-default swiper-include-line-number-in-search t)))

(fm/pkg embark
 (fm/after flyspell
  (fm/key-disable "C-." flyspell-mode-map)) ; Embark reserves that keybinding.
 (fm/key "C-." embark-act)
 (fm/after embark
  (setq-default prefix-help-command #'embark-prefix-help-command)))

(fm/pkg marginalia
 (marginalia-mode))

(fm/pkg prescient
 (fm/after prescient
  (setq-default prescient-save-file emacs-prescient-save-file)
  (setq-default prescient-sort-full-matches-first t)
  (eval-when-compile (defvar prescient-filter-method))
  (push 'literal-prefix prescient-filter-method)
  (push 'prefix prescient-filter-method)
  (push 'anchored prescient-filter-method))
 (fm/autoload prescient-persist-mode "prescient")
 (prescient-persist-mode +1))

(fm/pkg ivy-prescient
 (ivy-prescient-mode))

(fm/pkg orderless
 (fm/after orderless
  (eval-when-compile (defvar orderless-matching-styles))
  (push 'orderless-initialism orderless-matching-styles)
  (push 'orderless-prefixes orderless-matching-styles))
 (fm/after minibuffer
  (push 'orderless completion-styles)))

(fm/after minibuffer
 (setq-default read-file-name-completion-ignore-case t)
 (setq-default completion-category-defaults nil)
 (setq-default completion-cycle-threshold 4)
 (setq-default completions-detailed t))

(fm/pkg flyspell-correct-ivy
 (fm/after flyspell
  (fm/key-local "C-;" flyspell-correct-wrapper flyspell-mode-map)
  (setq-default flyspell-correct-interface #'flyspell-correct-ivy)))

(fm/pkg mwim
 (fm/key "C-a" mwim-beginning)
 (fm/key "C-e" mwim-end))

(fm/pkg expand-region
 (fm/key "C-=" er/expand-region))

(fm/pkg transient
 (fm/after transient
  (setq-default transient-history-file (concat emacs-var-dir "transient-history"))
  (setq-default transient-default-level 7)))

(fm/pkg blamer
 (fm/after blamer
  (setq-default blamer-idle-time 0)
  (setq-default blamer-commit-formatter ": %s")
  (setq-default blamer-datetime-formatter "%s")
  (setq-default blamer-max-commit-message-length 60))
 (fm/after prog-mode
  (fm/key-local "C-c b" blamer-mode prog-mode-map)))

(fm/pkg magit
 (fm/key "C-x g" magit-status)
 (fm/after magit-mode
  (setq-default magit-auto-revert-tracked-only nil)
  (setq-default magit-display-buffer-function
   'magit-display-buffer-same-window-except-diff-v1)
  (setq-default magit-repository-directories '(("~/Workspace" . 3)))
  (fm/hook after-save-hook magit-after-save-refresh-status "magit")))

(fm/pkg projectile
 (fm/after projectile
  (fm/key-local "C-x p" projectile-command-map projectile-mode-map "projectile")
  (fm/dim projectile-mode "Pr")
  (setq-default projectile-cache-file emacs-projectile-cache-file)
  (setq-default projectile-project-search-path '("~/Workspace"))
  (setq-default projectile-sort-order '(recently-active))
  (setq-default projectile-enable-caching nil)
  (setq-default projectile-completion-system 'ivy))
 (projectile-mode))

(fm/pkg counsel-projectile
 (fm/after projectile
  (fm/key-local "M-G" counsel-projectile-git-grep projectile-mode-map)))

(fm/pkg deadgrep
 (fm/key "M-F" deadgrep)
 (fm/after deadgrep
  (fm/key-local "<f5>" deadgrep-edit-mode deadgrep-mode-map "deadgrep")
  (fm/key-local "<f5>" deadgrep-mode deadgrep-edit-mode-map "deadgrep")))

(fm/pkg yasnippet-snippets
 (fm/after yasnippet
  (yasnippet-snippets-initialize)))

(fm/pkg yasnippet
 (fm/after yasnippet
  (fm/dim yas-minor-mode "Ys")
  (fm/after company
   (fm/hookn yas-minor-mode-hook (fm/company-add-backend 'company-yasnippet)))))

(fm/pkg diff-hl
 (fm/after diff-hl
  (setq-default diff-hl-draw-borders nil)
  (setq-default diff-hl-flydiff-delay 0.1))
 (fm/after magit-mode
  (fm/hook magit-pre-refresh-hook diff-hl-magit-pre-refresh "diff-hl")
  (fm/hook magit-post-refresh-hook diff-hl-magit-post-refresh "diff-hl")))

(fm/pkg multiple-cursors
 (fm/key "C-c C-v"       mc/edit-lines)
 (fm/key "C->"           mc/mark-next-like-this)
 (fm/key "C-<"           mc/mark-previous-like-this)
 (fm/key "C-S-<mouse-1>" mc/add-cursor-on-click)
 (fm/after multiple-cursors-core
  (setq-default mc/always-run-for-all t)))

(fm/pkg yaml-mode
 (fm/after yaml-mode
  (fm/key-local "C-c p" fm/generate-password yaml-mode-map "qol")
  (fm/hook yaml-mode-hook flycheck-mode)))

(fm/mode "clang-format" yaml-mode)

(fm/mode ".ll" llvm-mode "llvm-mode")

(fm/pkg autodisass-llvm-bitcode)
(fm/mode ".bc" autodisass-llvm-bitcode "autodisass-llvm-bitcode")

(fm/pkg demangle-mode
 (fm/after llvm-mode
  (fm/hook llvm-mode-hook demangle-mode)))

(fm/pkg hledger-mode
 (fm/after hledger-mode
  (setq-default hledger-currency-string "EUR")
  (setq-default hledger-current-overlay t)
  (setq-default hledger-comments-column 1)
  (fm/hookn hledger-mode-hook
   (setq-local tab-width 1)
   (fm/after flycheck
    (eval-when-compile (require 'flycheck-hledger))))
  (fm/hook hledger-mode-hook whitespace-mode)
  (fm/hook hledger-mode-hook symbol-overlay-mode)
  (fm/hook hledger-mode-hook flycheck-mode))
 (fm/mode ".journal" hledger-mode)
 (fm/mode ".ledger"  hledger-mode))

(fm/pkg flycheck-hledger)

(fm/pkg flycheck
 (fm/after flycheck
  (fm/autoload flycheck-next-error "flycheck")
  (fm/autoload flycheck-previous-error "flycheck")
  (fm/key-local "M-n" flycheck-next-error     flycheck-mode-map "flycheck")
  (fm/key-local "M-p" flycheck-previous-error flycheck-mode-map "flycheck")
  (setq-default flycheck-checker-error-threshold nil)
  (setq-default flycheck-mode-line-prefix "Fc")
  (setq-default flycheck-check-syntax-automatically
   '(idle-change new-line mode-enabled idle-buffer-switch))
  (setq-default flycheck-idle-change-delay 0.25)
  (setq-default flycheck-idle-buffer-switch-delay 0.25)
  (fm/hook flycheck-mode-hook flycheck-posframe-mode)))

(fm/pkg flycheck-posframe
 (fm/after flycheck-posframe
  (setq-default flycheck-posframe-position 'window-bottom-left-corner)
  (setq-default flycheck-posframe-border-width 1)
  (setq-default flycheck-posframe-prefix " ↪ Info: ")
  (setq-default flycheck-posframe-warnings-prefix " ⚠ Warning: ")
  (setq-default flycheck-posframe-error-prefix " ⤬ Error: ")
  (fm/after company
   (fm/hook flycheck-posframe-inhibit-functions company--active-p "company")
   (fm/hook flycheck-posframe-inhibit-functions
    (lambda (&rest _) (bound-and-true-p company-backend))))))

(fm/pkg consult-flycheck
 (fm/after flycheck
  (fm/key-local "C-c ! a" consult-flycheck flycheck-mode-map)))

(fm/pkg company
 (fm/after company
  (fm/dim company-mode "Co")
  (setq-default company-backends '((company-capf company-files company-keywords)))
  (setq-default completion-ignore-case t)
  (setq-default company-minimum-prefix-length 1)
  (setq-default company-selection-wrap-around t)
  (fm/key-local "<tab>" company-indent-or-complete-common company-mode-map "company")))

(defun fm/company-add-backend (backend)
 "Add BACKEND to local version of `company-backends'."
 (eval-when-compile (defvar company-backends))
 (let ((backends `((,backend . ,(car company-backends)))))
  (setq-local company-backends backends)))

(fm/pkg company-posframe
 (fm/after company-posframe
  (fm/dim company-posframe-mode))
 (fm/after company
  (fm/hook company-mode-hook company-posframe-mode "company-posframe")))

(fm/pkg company-prescient
 (fm/after company
  (fm/hook company-mode-hook company-prescient-mode)))

(fm/pkg tree-sitter-langs
 (fm/after tree-sitter-mode
  (fm/hook tree-sitter-mode-hook tree-sitter-langs-install-grammars)))

(fm/pkg tree-sitter
 (fm/after tree-sitter
  (fm/dim tree-sitter-mode "Ts")
  (fm/hook tree-sitter-mode-hook tree-sitter-hl-mode)))

(fm/after prog-mode
 (fm/hook prog-mode-hook diff-hl-mode)
 (fm/hook prog-mode-hook eldoc-mode)
 (fm/hook prog-mode-hook show-paren-mode)
 (fm/hook prog-mode-hook flyspell-prog-mode)
 (fm/hook prog-mode-hook flycheck-mode)
 (fm/hook prog-mode-hook yas-minor-mode)
 (fm/hook prog-mode-hook company-mode)
 (fm/hook prog-mode-hook electric-pair-mode)
 (fm/hook prog-mode-hook electric-layout-mode)
 (fm/hook prog-mode-hook display-line-numbers-mode)
 (fm/hook prog-mode-hook hl-line-mode)
 (fm/hook prog-mode-hook bug-reference-prog-mode))

(fm/after conf-mode
 (fm/hook conf-desktop-mode-hook diff-hl-mode)
 (fm/hook conf-desktop-mode-hook show-paren-mode)
 (fm/hook conf-desktop-mode-hook flyspell-prog-mode)
 (fm/hook conf-desktop-mode-hook electric-pair-mode)
 (fm/hook conf-desktop-mode-hook electric-layout-mode)
 (fm/hook conf-desktop-mode-hook display-line-numbers-mode)
 (fm/hook conf-desktop-mode-hook hl-line-mode))

(fm/pkg meson-mode
 (fm/after meson-mode
  (fm/hook meson-mode-hook symbol-overlay-mode)
  (fm/hook meson-mode-hook company-mode)))

(fm/pkg rustic
 (fm/after rustic
  (fm/key-local "<f5>" rust-dbg-wrap-or-unwrap            rustic-mode-map "rust-mode")
  (fm/key-local "<f6>" lsp-rust-analyzer-expand-macro     rustic-mode-map "lsp-rust")
  (fm/key-local "<f7>" lsp-rust-analyzer-join-lines       rustic-mode-map "lsp-rust")
  (fm/key-local "<f8>" lsp-rust-analyzer-inlay-hints-mode rustic-mode-map "lsp-rust")
  (setq-default rustic-lsp-server 'rust-analyzer)
  (setq-default rustic-analyzer-command '("/usr/bin/rust-analyzer"))
  (setq-default rustic-format-on-save nil)
  (setq-default rustic-lsp-format t)
  (setq-default rustic-indent-offset 2)
  (setq-default rustic-always-locate-project-on-open t)
  (fm/hookn rustic-mode-hook (electric-quote-local-mode -1))
  (fm/hook rustic-mode-hook subword-mode)
  (fm/hookn rustic-mode-hook
   (fm/after lsp-mode
    (fm/hookn lsp-mode-hook
     (fm/hook before-save-hook lsp-format-buffer "lsp-mode" t))))))

(fm/pkg lsp-mode
 (fm/after lsp-mode
  (fm/dim lsp-mode "Ls")
  (fm/key-local "C-c f" lsp-format-buffer           lsp-mode-map "lsp-mode")
  (fm/key-local "C-c g" lsp-format-region           lsp-mode-map "lsp-mode")
  (fm/key-local "C-c r" lsp-rename                  lsp-mode-map "lsp-mode")
  (fm/key-local "C-c h" lsp-describe-thing-at-point lsp-mode-map "lsp-mode")
  (fm/key-local "C-="   lsp-extend-selection        lsp-mode-map "lsp-mode")
  (fm/key-local "M-RET" lsp-execute-code-action     lsp-mode-map "lsp-mode")
  (setq-default lsp-progress-prefix "  Progress: ")
  (setq-default lsp-completion-provider :none) ; Company-capf is already set
  (setq-default lsp-headerline-breadcrumb-enable t)
  (setq-default lsp-restart 'auto-restart)
  (setq-default lsp-enable-snippet t)
  (setq-default lsp-keymap-prefix "C-c")
  (setq-default lsp-idle-delay 0.1)
  (setq-default lsp-file-watch-threshold nil)
  (setq-default lsp-enable-semantic-highlighting t)
  (setq-default lsp-enable-indentation t)
  (setq-default lsp-enable-on-type-formatting t)
  (setq-default lsp-before-save-edits nil)
  (setq-default lsp-auto-configure t)
  (setq-default lsp-signature-render-documentation t)
  (setq-default lsp-modeline-code-actions-enable nil)
  (setq-default lsp-log-io nil)
  (setq-default lsp-enable-imenu nil)
  (fm/after which-key
   (fm/hook lsp-mode-hook lsp-enable-which-key-integration "lsp-mode")))
  ;; (fm/hookn lsp-mode-hook
  ;;  (fm/hook before-save-hook lsp-format-buffer "lsp-mode" t)))
 (fm/after lsp-lens
  (fm/dim lsp-lens-mode)
  (setq-default lsp-lens-mode nil))
 (fm/after lsp-headerline
  (setq-default lsp-headerline-breadcrumb-icons-enable nil))
 (fm/after lsp-semantic-tokens
  (setq-default lsp-semantic-tokens-apply-modifiers t))
 (fm/after lsp-vetur
  (setq-default lsp-vetur-emmet "inMarkupAndStylesheetFilesOnly"))
 (fm/after lsp-rust
  (setq-default lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (setq-default lsp-rust-analyzer-proc-macro-enable t)
  (setq-default lsp-rust-analyzer-use-client-watching nil)
  (setq-default lsp-rust-analyzer-server-command "/usr/bin/rust-analyzer")
  (setq-default lsp-rust-racer-completion nil)
  (setq-default lsp-rust-build-bin t)
  (setq-default lsp-rust-build-lib t)
  (setq-default lsp-rust-clippy-preference "on")
  (setq-default lsp-rust-analyzer-server-display-inlay-hints t)
  (setq-default lsp-rust-analyzer-display-chaining-hints t)
  (setq-default lsp-rust-analyzer-display-parameter-hints t)
  (setq-default lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq-default lsp-rust-analyzer-display-reborrow-hints t)
  (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-enable "always")
  (setq-default lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (setq-default lsp-rust-analyzer-server-format-inlay-hints nil)
  (setq-default lsp-rust-all-features t)
  (setq-default lsp-rust-all-targets t)
  ;; (setq-default lsp-rust-build-on-save t)
  (setq-default lsp-rust-unstable-features t)
  (setq-default lsp-rust-full-docs t)
  (setq-default lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq-default lsp-rust-analyzer-max-inlay-hint-length 50)
  (setq-default lsp-rust-analyzer-inlay-param-format "%s")
  (setq-default lsp-rust-analyzer-inlay-type-format "%s"))
 (fm/after lsp-clangd
  (setq-default lsp-clients-clangd-args
   '("--header-insertion-decorators"
     "--all-scopes-completion"
     "--clang-tidy"
     "--completion-style=detailed"
     "--header-insertion=never"
     "--inlay-hints"
     "--limit-results=1000"
     "-j=4"
     "--malloc-trim"
     "--pch-storage=memory"))
  (fm/after cc-mode
   (fm/autoload lsp-clangd-find-other-file "lsp-clangd")
   (fm/key-local "<f2>" lsp-clangd-find-other-file c-mode-base-map))))

(fm/pkg lsp-ivy
 (fm/after lsp-mode
  (fm/key-local "C-c x" lsp-ivy-workspace-symbol lsp-mode-map)))

(fm/pkg lsp-treemacs
 (fm/after lsp-mode
  (fm/key-local "C-c e" lsp-treemacs-errors-list    lsp-mode-map)
  (fm/key-local "C-c s" lsp-treemacs-symbols        lsp-mode-map)
  (fm/key-local "C-c c" lsp-treemacs-call-hierarchy lsp-mode-map)
  (fm/key-local "C-c t" lsp-treemacs-type-hierarchy lsp-mode-map)
  (fm/hook lsp-mode-hook lsp-treemacs-sync-mode))
 (fm/after treemacs-interface
  (fm/key "<f12>" treemacs-delete-other-windows "treemacs-interface"))
 (fm/after treemacs-customization
  (setq-default treemacs-width 70))
 (fm/after treemacs-mode
  (fm/hook treemacs-mode-hook toggle-truncate-lines)))

(fm/pkg lsp-ui
 (fm/after lsp-ui-flycheck
  (setq-default lsp-ui-flycheck-enable t))
 (fm/after lsp-ui-doc
  (setq-default lsp-ui-doc-enable nil)
  (setq-default lsp-ui-doc-alignment 'frame)
  (setq-default lsp-ui-doc-header t)
  (setq-default lsp-ui-doc-include-signature t)
  (setq-default lsp-ui-doc-max-height 30)
  (setq-default lsp-ui-doc-use-webkit t))
 (fm/after lsp-ui-peek
  (setq-default lsp-ui-peek-list-width 30)
  (setq-default lsp-ui-peek-always-show t))
 (fm/after lsp-ui-sideline
  (setq-default lsp-ui-sideline-enable nil))
 (fm/after lsp-ui
  (fm/key-local "M-." lsp-ui-peek-find-definitions    lsp-ui-mode-map "lsp-ui-peek")
  (fm/key-local "M-?" lsp-ui-peek-find-references     lsp-ui-mode-map "lsp-ui-peek")
  (fm/key-local "M-I" lsp-ui-peek-find-implementation lsp-ui-mode-map "lsp-ui-peek")
  (fm/key-local "C-c d"   lsp-ui-doc-show      lsp-ui-mode-map "lsp-ui-doc")
  (fm/key-local "C-c ! l" lsp-ui-flycheck-list lsp-ui-mode-map "lsp-ui-flycheck")))

(fm/pkg web-mode
 (fm/mode ".html" web-mode)
 (fm/mode ".css" web-mode)
 (fm/mode ".js" web-mode)
 (fm/after web-mode
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-enable-current-column-highlight t)
  (setq-default web-mode-enable-current-element-highlight t)
  (setq-default web-mode-auto-close-style 3)
  (setq-default web-mode-enable-auto-expanding t)
  (fm/hook web-mode-hook lsp)
  (fm/hookn web-mode-hook (setq-local tab-width 2))))

(fm/pkg company-web
 (fm/after web-mode
  (fm/after company
   (fm/hookn web-mode-hook
    (fm/company-add-backend 'company-css)
    (fm/company-add-backend 'company-web-html)))))

(fm/pkg emmet-mode
 (setq-default emmet-indentation 2)
 (fm/after web-mode
  (fm/hook web-mode-hook emmet-mode)))

(fm/pkg dockerfile-mode)
(fm/pkg pkgbuild-mode)

;; Print startup stats.
(message "Startup in %s (%d GC runs)" (emacs-init-time) gcs-done)

(provide 'init)
;;; init ends here

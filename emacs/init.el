;;; init --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
 (defconst emacs-dots-dir "/home/fred/Workspace/dots/emacs/")
 (when (not (seq-contains-p load-path emacs-dots-dir))
  (push emacs-dots-dir load-path)))

(require 'init-macros)
(require 'qol)

(fm/key "C-x e"    fm/replace-escapes)
(fm/key "<M-up>"   fm/move-line-up)
(fm/key "<M-down>" fm/move-line-down)

(fm/key "{"  (lambda () (interactive) (fm/insert-pair ?\{ ?\} nil)))
(fm/key "("  (lambda () (interactive) (fm/insert-pair ?\( ?\) t)))
(fm/key "'"  (lambda () (interactive) (fm/insert-pair ?\' ?\' t)))
(fm/key "`"  (lambda () (interactive) (fm/insert-pair ?\` ?\` t)))
(fm/key "\"" (lambda () (interactive) (fm/insert-pair ?\" ?\" t)))

;; Directories
(defconst emacs-extra-dir (concat emacs-dots-dir "extra"))
(push emacs-extra-dir load-path)

(defconst expanded-user-emacs-dir (expand-file-name user-emacs-directory))
(defconst emacs-elpa-dir (concat expanded-user-emacs-dir "elpa"))
(defconst emacs-places-file (concat expanded-user-emacs-dir "places"))
(defconst emacs-recentf-file (concat expanded-user-emacs-dir "recentf"))
(defconst emacs-temp-dir (concat temporary-file-directory "emacs/"))
(defconst emacs-autosaves-dir (concat emacs-temp-dir "autosaves"))
(defconst emacs-autosaves-pattern (concat emacs-autosaves-dir "/\\1"))
(defconst emacs-backups-dir (concat emacs-temp-dir "backups"))
(defconst emacs-backups-pattern (concat emacs-backups-dir "/"))
(make-directory emacs-autosaves-dir t)
(make-directory emacs-backups-dir t)

;; Do not show a message in the echo area after startup.
(fset 'display-startup-echo-area-message 'ignore)

;; subr - Respond to yes/no questions using Y/N.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Common User Access.
(cua-selection-mode 1)

;; startup
(fm/var inhibit-startup-screen t)
(fm/var inhibit-startup-message t)
(fm/var inhibit-startup-buffer-menu t)
(fm/var initial-scratch-message nil)
(fm/var initial-major-mode 'fundamental-mode)

;; scrolling
(fm/var scroll-conservatively 4)
(fm/var scroll-margin 3)
(fm/var hscroll-margin 3)
(fm/var hscroll-step 1)
(fm/var auto-hscroll-mode 'current-line)
(fm/var fast-but-imprecise-scrolling t)

;; external processes
(fm/var read-process-output-max (* 1024 1024))

(fm/after frame
 (fm/var blink-cursor-mode nil))

(fm/after scroll-bar
 (fm/var horizontal-scroll-bar-mode nil))

(fm/after font-lock
 (fm/face font-lock-function-name-face
  :inherit font-lock-builtin-face)
 (fm/face font-lock-keyword-face
  :foreground "MediumSlateBlue")
 (fm/face font-lock-type-face
  :foreground "DarkGreen")
 (fm/face font-lock-variable-name-face
  :foreground "DarkCyan")
 (fm/face font-lock-string-face
  :foreground "OliveDrab")
 (fm/face font-lock-comment-face
  :foreground "DarkMagenta")
 (fm/face font-lock-warning-face
  :foreground "Orange3")
 (fm/face font-lock-constant-face
  :foreground "CornflowerBlue"))

;; saveplace
(fm/var save-place-mode t)
(fm/var save-place t)
(fm/var save-place-file emacs-places-file)

;; savehist
(fm/var savehist-mode t)
(fm/var history-delete-duplicates t)
(fm/var history-length 100)

;; recentf
(fm/var recentf-mode t)
(fm/var recentf-auto-cleanup 'never)
(fm/var recentf-save-file emacs-recentf-file)
(fm/var recentf-max-menu-items 50)
(fm/var recentf-max-saved-items 100)
(fm/var recentf-exclude `(,emacs-elpa-dir))

(fm/hook kill-emacs-hook recentf-cleanup "recentf")

(fm/after files
 (fm/var confirm-kill-processes nil)
 (fm/var auto-save-file-name-transforms `((".*" ,emacs-autosaves-pattern t)))
 (fm/var backup-directory-alist `((".*" . ,emacs-backups-pattern)))
 (fm/var backup-inhibited nil)
 (fm/var make-backup-files t)
 (fm/var delete-old-versions t)
 (fm/var mode-require-final-newline 'visit-save)
 (fm/var require-final-newline 'visit-save)
 (fm/var load-prefer-newer t)
 (fm/var coding-system-for-read 'utf-8-unix)
 (fm/var coding-system-for-write 'utf-8-unix))

(fm/after help
 (fm/var help-window-select t))

(fm/after window
 (fm/var split-height-threshold 160)
 (fm/var even-window-sizes 'width-only))

(fm/key "<M-S-right>" next-buffer)
(fm/key "<M-S-left>"  previous-buffer)

(fm/after mouse
 (fm/var mouse-yank-at-point t))

;; windmove
(windmove-default-keybindings)
(windmove-delete-default-keybindings)

(fm/after simple
 (fm/var undo-limit (* 1024 1024))
 (fm/var suggest-key-bindings 10)
 (fm/var column-number-mode t)
 (fm/var line-number-mode nil)
 (fm/var auto-save-mode t)
 (fm/var save-interprogram-paste-before-kill t)
 (fm/var backward-delete-char-untabify-method 'hungry)
 (fm/key "<mouse-4>" previous-line)
 (fm/key "<mouse-5>" next-line)
 (fm/after files
  (fm/hook before-save-hook delete-trailing-whitespace)))

;; indent
(fm/var indent-tabs-mode nil)

;; xref
(fm/after xref
 (fm/var xref-backend-functions '()))

;; bindings
(fm/var column-number-indicator-zero-based nil)

(fm/after uniquify
 (fm/var uniquify-buffer-name-style 'forward))

(fm/after vc
 (fm/var vc-make-backup-files t))

(fm/after newcomment
 (fm/var comment-fill-column 80))

;; fill
(fm/var fill-column 90)
(fm/var colon-double-space t)
(fm/var default-justification 'left)

(fm/after ediff-wind
 (fm/var ediff-split-window-function #'split-window-horizontally)
 (fm/var ediff-window-setup-function #'ediff-setup-windows-plain))

(fm/after elec-pair
 (fm/var electric-pair-pairs '((?\[ . ?\])))) ;; (?\{ . ?\})

(fm/after display-line-numbers
 (fm/var display-line-numbers-grow-only t)
 (fm/var display-line-numbers-width-start t)
 (fm/face line-number
  :foreground "Gray85")
 (fm/face line-number-current-line
  :foreground "Gray70"))

(fm/after hl-line
 (fm/face hl-line
  :background "Gray95"
  :extend nil))

(fm/after abbrev
 (fm/dim abbrev-mode "Ab" t))

(fm/after whitespace
 (fm/dim whitespace-mode "Ws")
 (fm/var whitespace-line-column 90)
 (fm/var show-trailing-whitespace nil)
 (fm/var whitespace-action '(cleanup))
 (fm/var whitespace-style
  '(face
    tabs
    lines
    empty
    tab-mark
    indentation
    indentation::tab
    indentation::space
    space-after-tab
    space-after-tab::tab
    space-after-tab::space
    space-before-tab
    space-before-tab::tab
    space-before-tab::space)))

(fm/after make-mode
 (fm/hook makefile-mode-hook whitespace-mode))

(fm/after elisp-mode
 (fm/var lisp-indent-offset 1)
 (fm/var lisp-indent-function #'common-lisp-indent-function)
 (fm/hook emacs-lisp-mode-hook whitespace-mode))

(fm/mode "emacs" emacs-lisp-mode)
(fm/mode ".config/emacs/init" emacs-lisp-mode)

(fm/after text-mode
 (fm/hookn text-mode-hook
  (toggle-truncate-lines t)))

(fm/mode "Passwords.txt" text-mode)
(fm/mode "Passwords_old.txt" text-mode)

(fm/after eldoc
 (fm/dim eldoc-mode "Ed"))

(fm/after paren
 (fm/var show-paren-when-point-inside-paren t)
 (fm/var show-paren-style 'mixed)
 (fm/var show-paren-highlight-openparen t)
 (fm/face show-paren-match
  :background "PowderBlue")
 (fm/face show-paren-mismatch
  :background "LightSalmon")
 (fm/face show-paren-match-expression
  :background "Lavender"))

(fm/after dired
 (fm/var dired-listing-switches "-l --group-directories-first")
 (fm/var dired-hide-details-hide-symlink-targets nil)
 (fm/hook dired-mode-hook dired-hide-details-mode "dired"))

(fm/after autorevert
 (fm/dim autorevert-mode "Ar")
 (fm/var auto-revert-interval 1)
 (fm/var auto-revert-avoid-polling t)
 (fm/var buffer-auto-revert-by-notification t)
 (fm/var auto-revert-mode-text " Ar"))

(fm/after dired
 (fm/hook dired-mode-hook auto-revert-mode))

(fm/after subword
 (fm/dim subword-mode "Sw"))

(fm/after flyspell
 (fm/dim flyspell-mode "Fs")
 (fm/var ispell-program-name "aspell")
 (fm/var ispell-extra-args '("--sug-mode=ultra"))
 (fm/face flyspell-duplicate
  :underline "YellowGreen")
 (fm/face flyspell-incorrect
  :underline "Orchid"))

(fm/after text-mode
 (fm/hook text-mode-hook flyspell-mode))

(fm/after sh-script
 (fm/hookn sh-mode-hook
  (fm/hook after-save-hook executable-make-buffer-file-executable-if-script-p)))

(fm/after llvm-mode
 (fm/hookn llvm-mode-hook (toggle-truncate-lines t)))
(fm/mode ".ll" llvm-mode "llvm-mode")

(defmacro fm/setup-c-style-comments ()
 "Setup C-style /* ... */ comments."
 `(fm/after newcomment
   (setq-local comment-style 'extra-line)))

(fm/after css-mode
 (fm/hookn css-mode-hook
  (fm/setup-c-style-comments)))

(fm/after cc-mode
 (fm/key "(" nil c-mode-base-map)
 (fm/hook c-mode-hook tree-sitter-mode)
 (fm/hook c-mode-hook symbol-overlay-mode))

(fm/after cc-vars
 (fm/var c-mark-wrong-style-of-comment t)
 (fm/var c-default-style
  '((other . "user")))
 (fm/hookn c-mode-common-hook
  (fm/setup-c-style-comments)))

;; js-mode
(fm/mode ".hocon" js-mode)
(fm/after js
 (fm/hook js-mode-hook tree-sitter-mode))

(fm/after package
 (fm/var package-archives
  '(("gnu"   . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("org"   . "https://orgmode.org/elpa/"))))

(fm/pkg toml-mode)
(fm/pkg cmake-mode)
(fm/pkg dockerfile-mode)
(fm/pkg markdown-mode)
(fm/pkg crux)
(fm/pkg smex)

(fm/pkg json-mode
 (fm/hook json-mode-hook indent-guide-mode))

(fm/pkg systemd
 (fm/hook systemd-mode-hook company-mode))

(fm/pkg org-bullets
 (fm/var org-bullets-bullet-list '(" ")))

(fm/pkg org
 (fm/after org
  (fm/var org-cycle-separator-lines 0)
  (fm/var org-startup-folded 'content)
  (fm/var org-ellipsis "  ⤵")
  (fm/var org-hide-leading-stars t)
  (fm/var org-hide-emphasis-markers t)
  (fm/var org-fontify-whole-heading-line t)
  (fm/var org-fontify-done-headline t)
  (fm/var org-startup-indented t)
  (fm/var org-property-format "%s %s")
  (fm/face org-document-title
   :inherit variable-pitch
   :height 1.2
   :foreground "MidnightBlue"
   :bold t)
  (fm/face org-target
   :slant italic
   :foreground "Tan"
   :height 0.8)
  (fm/face org-table
   :height 0.8
   :foreground "RoyalBlue") ;; "NavyBlue"
  (fm/face org-ellipsis
   :inherit variable-pitch
   :height 0.7
   :foreground "SteelBlue")
  (fm/face org-level-1
   :inherit (variable-pitch outline-1)
   :foreground "SlateBlue"
   :bold t)
  (fm/face org-level-2
   :inherit (variable-pitch outline-2)
   :foreground "IndianRed3"
   :bold t)
  (fm/face org-level-3
   :inherit (variable-pitch outline-3)
   :foreground "SteelBlue"
   :bold t)
  (fm/face org-level-4
   :inherit (variable-pitch outline-4))
  (fm/face org-todo
   :inherit variable-pitch
   :height 0.8
   :foreground "Maroon"
   :bold t)
  (fm/face org-done
   :inherit variable-pitch
   :height 0.8
   :foreground "ForestGreen"
   :bold t)
  (fm/face org-drawer
   :foreground "Snow3"
   :height 0.8)
  (fm/face org-special-keyword
   :inherit font-lock-keyword-face
   :height 0.8
   :bold t)
  (fm/hook org-mode-hook org-bullets-mode)
  (fm/hookn org-mode-hook
   (setq-local left-margin-width 2)
   (setq-local right-margin-width 2)
   (setq-local scroll-margin 0)
   (setq-local cursor-type 'bar)
   ;; Change the default font used for org-mode to a serif font (Caladea), but keep the
   ;; fixed-pitch fonts so that alignments and indentations stay consistent.
   (face-remap-add-relative 'default
    :family "Caladea")
   (face-remap-add-relative 'fixed-pitch
    :family "Monospace"
    :height 120)
   (face-remap-add-relative 'fixed-pitch-serif
    :family "Monospace"
    :height 120)
   (face-remap-add-relative 'org-indent
    :inherit '(org-hide fixed-pitch)))))

(fm/pkg which-key
 (fm/dim which-key-mode)
 (fm/var which-key-idle-delay 0.2)
 (which-key-mode))

(fm/pkg counsel
 (fm/after counsel
  (fm/dim counsel-mode)
  (put 'counsel-find-symbol 'no-counsel-M-x t))
 (counsel-mode))

(fm/pkg swiper
 (fm/after swiper
  (fm/var swiper-include-line-number-in-search t))
 (fm/key "C-s"         swiper-isearch)
 (fm/key "C-c C-c C-s" swiper-all)
 (fm/key "C-c C-s"     swiper-thing-at-point)
 (fm/key "C-r"         swiper-isearch-backward))

(fm/pkg ivy
 (fm/after ivy
  (fm/dim ivy-mode)
  (fm/var ivy-wrap t)
  (fm/var ivy-use-selectable-prompt t)
  (fm/var ivy-use-virtual-buffers t)
  (fm/var ivy-count-format "(%d/%d) ")
  (fm/var ivy-virtual-abbreviate 'abbreviate)
  (fm/var ivy-initial-inputs-alist nil)
  (fm/var ivy-extra-directories nil)
  (fm/var ivy-re-builders-alist
   '((t . ivy--regex-ignore-order) (t . ivy--regex-plus)))
  (fm/key "<RET>" ivy-alt-done ivy-minibuffer-map "ivy"))
 (ivy-mode))

(fm/pkg ivy-rich
 (fm/after ivy-rich
  (fm/var ivy-rich-path-style 'abbrev))
 (ivy-rich-mode))

(fm/pkg fzf
 (fm/key "M-F" fzf-git-files))

(fm/pkg deadgrep
 (fm/key "M-G" deadgrep))

(fm/pkg mwim
 (fm/key "C-a" mwim-beginning)
 (fm/key "C-e" mwim-end))

(fm/pkg expand-region
 (fm/key "C-=" er/expand-region))

(fm/pkg transient
 (fm/after transient
  (fm/var transient-default-level 7)))

(fm/pkg magit
 (fm/after magit-mode
  (fm/var magit-auto-revert-tracked-only nil)
  (fm/var magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  (fm/var magit-repository-directories '(("~/Workspace" . 3)))
  (fm/hook after-save-hook magit-after-save-refresh-status "magit"))
 (fm/key "C-x g" magit-status))

(fm/pkg indent-guide
 (fm/after indent-guide
  (fm/face indent-guide-face
   :foreground "gray80")))

(fm/pkg projectile
 (fm/after projectile
  (fm/dim projectile-mode "Pr")
  (fm/var projectile-project-search-path '("~/Workspace"))
  (fm/var projectile-sort-order '(recently-active))
  (fm/var projectile-enable-caching t)
  (fm/var projectile-completion-system 'ivy)
  (fm/key "C-x p" projectile-command-map projectile-mode-map "projectile"))
 (projectile-mode))

(fm/pkg counsel-projectile
 (fm/after counsel
  (counsel-projectile-mode)))

(fm/pkg yasnippet-snippets)
(fm/pkg yasnippet
 (fm/after yasnippet
  (fm/dim yas-minor-mode "Ys")
  (defvar yas-snippet-dirs)
  (push (expand-file-name "~/Workspace/dots/emacs/snippets") yas-snippet-dirs))
 (defvar fm/yas-snippets-loaded nil
  "Defined in init file to avoid loading snippets multiple times.")
 (defun fm/yas-minor-mode ()
  "Ensure snippets are loaded then load the yasnippet minor mode."
  (require 'yasnippet)
  (when (not fm/yas-snippets-loaded)
   (declare-function yas-reload-all 'yasnippet)
   (yas-reload-all)
   (setq fm/yas-snippets-loaded t))
  (yas-minor-mode)))

(fm/pkg diff-hl
 (fm/after diff-hl
  (fm/var diff-hl-draw-borders nil)
  (fm/var diff-hl-flydiff-delay 0.1)
  (fm/face diff-hl-delete
   :background "RosyBrown1")
  (fm/face diff-hl-insert
   :background "DarkSeaGreen2")
  (fm/face diff-hl-change
   :background "PowderBlue"))
 (fm/after magit-mode
  (fm/hook magit-pre-refresh-hook diff-hl-magit-pre-refresh "diff-hl")
  (fm/hook magit-post-refresh-hook diff-hl-magit-post-refresh "diff-hl")))

(fm/pkg symbol-overlay
 (fm/after symbol-overlay
  (fm/dim symbol-overlay-mode "Sy")
  (fm/var symbol-overlay-idle-time 0.1)
  (fm/face symbol-overlay-default-face
   :background "HoneyDew2")
  (fm/hookn symbol-overlay-mode-hook
   (fm/key "M->" symbol-overlay-jump-next symbol-overlay-mode-map)
   (fm/key "M-<" symbol-overlay-jump-prev symbol-overlay-mode-map))))

(fm/pkg multiple-cursors
 (fm/after multiple-cursors
  (fm/var mc/always-run-for-all t)
  (fm/face mc/cursor-bar-face
   :background "Gray40"
   :foreground "White")
  (fm/face mc/cursor-face
   :background "Gray50"
   :foreground "White"))
 (fm/key "C-c C-v" mc/edit-lines)
 (fm/key "C->" mc/mark-next-like-this)
 (fm/key "C-<" mc/mark-previous-like-this)
 (fm/key "C-S-<mouse-1>" mc/add-cursor-on-click))

(fm/pkg yaml-mode
 (fm/after yaml-mode
  (fm/key "M-p" fm/generate-password yaml-mode-map)
  (fm/hook yaml-mode-hook flycheck-mode)))

(fm/pkg flycheck-hledger)
(fm/pkg hledger-mode
 (fm/after hledger-mode
  (fm/var hledger-currency-string "EUR")
  (fm/var hledger-current-overlay t)
  (fm/var hledger-comments-column 1)
  (fm/face hledger-description-face
   :inherit font-lock-keyword-face)
  (fm/face hledger-amount-face
   :inherit font-lock-constant-face
   :inverse-video t)
  (require 'flycheck-hledger)
  (fm/hookn hledger-mode-hook
   (toggle-truncate-lines t)
   (setq-local tab-width 1))
  (fm/hook hledger-mode-hook whitespace-mode)
  (fm/hook hledger-mode-hook whitespace-mode)
  (fm/hook hledger-mode-hook symbol-overlay-mode)
  (fm/hook hledger-mode-hook fm/yas-minor-mode)
  (fm/hook hledger-mode-hook flycheck-mode))
 (fm/mode ".journal" hledger-mode)
 (fm/mode ".ledger"  hledger-mode))

(fm/pkg flycheck
 (fm/after flycheck
  (fm/var flycheck-checker-error-threshold nil)
  (fm/var flycheck-mode-line-prefix "Fc")
  (fm/var flycheck-check-syntax-automatically
   '(idle-change
     new-line
     mode-enabled
     idle-buffer-switch))
  (fm/var flycheck-idle-change-delay 0.25)
  (fm/var flycheck-idle-buffer-switch-delay 0.25)
  (fm/face flycheck-error
   :underline "Red1")
  (fm/face flycheck-info
   :underline "ForestGreen")
  (fm/face flycheck-warning
   :underline "DarkOrange")
  (fm/hookn flycheck-mode-hook
   (fm/key "M-n" flycheck-next-error flycheck-mode-map "flycheck")
   (fm/key "M-p" flycheck-previous-error flycheck-mode-map "flycheck"))
  (fm/hook flycheck-mode-hook flycheck-posframe-mode)))

(fm/pkg flycheck-posframe
 (fm/after flycheck-posframe
  (fm/var flycheck-posframe-position 'window-bottom-right-corner)
  (fm/var flycheck-posframe-border-width 1)
  (fm/var flycheck-posframe-warnings-prefix "Warning: ")
  (fm/var flycheck-posframe-error-prefix "Error: ")
  (fm/var flycheck-posframe-prefix "Info: ")
  (fm/face flycheck-posframe-background-face
   :background "CornSilk")
  (fm/face flycheck-posframe-warning-face
   :foreground "DarkOrange")
  (fm/face flycheck-posframe-border-face
   :background "Wheat" :foreground "Wheat")
  (fm/face flycheck-posframe-error-face
   :foreground "DarkRed"))
 (fm/after company
  (fm/hook flycheck-posframe-inhibit-functions company--active-p "company")
  (fm/hook flycheck-posframe-inhibit-functions
   (lambda (&rest _) (bound-and-true-p company-backend)))))

(fm/pkg company
 (fm/after company
  (fm/dim company-mode "Co")
  (setq-default company-backends '(company-capf company-files))
  (fm/var completion-ignore-case t)
  (fm/var company-echo-truncate-lines nil)
  (fm/var company-selection-wrap-around t)
  (fm/var company-tooltip-minimum 10)
  (fm/var company-tooltip-limit 15)
  (fm/var company-tooltip-align-annotations t)
  (fm/var company-idle-delay 0.3)
  (fm/var company-occurence-weight-function 'company-occurrence-prefer-any-closest)
  (fm/var company-frontends
   '(company-echo-metadata-frontend
     company-pseudo-tooltip-frontend))
  (fm/var company-transformers
   '(company-sort-by-occurrence
     company-sort-by-backend-importance
     company-sort-prefer-same-case-prefix))
  (fm/face company-tooltip
   :background "gray95")
  (fm/hook company-mode-hook company-posframe-mode "company-posframe")))

(fm/pkg company-posframe
 (fm/after company-posframe
  (fm/dim company-posframe-mode)
  (fm/var company-posframe-show-params
   '(:internal-border-width 1
     :internal-border-color "gray60"))))

(fm/pkg tree-sitter-langs)
(fm/pkg tree-sitter
 (fm/after tree-sitter
  (fm/dim tree-sitter-mode "Ts")
  (require 'tree-sitter-langs)
  (fm/hook tree-sitter-mode-hook tree-sitter-hl-mode)))

(fm/after prog-mode
 (fm/hook prog-mode-hook diff-hl-mode)
 (fm/hook prog-mode-hook eldoc-mode)
 (fm/hook prog-mode-hook show-paren-mode)
 (fm/hook prog-mode-hook flyspell-prog-mode)
 (fm/hook prog-mode-hook flycheck-mode)
 (fm/hook prog-mode-hook fm/yas-minor-mode)
 (fm/hook prog-mode-hook flyspell-prog-mode)
 (fm/hook prog-mode-hook company-mode)
 (fm/hook prog-mode-hook electric-pair-mode)
 (fm/hook prog-mode-hook electric-layout-mode)
 (fm/hook prog-mode-hook display-line-numbers-mode)
 (fm/hook prog-mode-hook hl-line-mode))

(fm/pkg rustic
 (fm/after rustic
  (fm/var rustic-lsp-server 'rust-analyzer)
  (fm/var rustic-analyzer-command '("/usr/bin/rust-analyzer"))
  (fm/var rustic-format-on-save nil)
  (fm/var rustic-lsp-format t)
  (fm/var rustic-indent-offset 2)
  (fm/var rustic-always-locate-project-on-open t)
  (fm/hookn rustic-mode-hook
   (autoload 'rust-dbg-wrap-or-unwrap "rust-mode")
   (fm/key "<f5>" rust-dbg-wrap-or-unwrap rustic-mode-map "rust-mode")
   (fm/after lsp-rust
    (fm/key "<f6>" lsp-rust-analyzer-expand-macro     rustic-mode-map "lsp-rust")
    (fm/key "<f7>" lsp-rust-analyzer-join-lines       rustic-mode-map "lsp-rust")
    (fm/key "<f8>" lsp-rust-analyzer-inlay-hints-mode rustic-mode-map "lsp-rust"))
   (electric-quote-local-mode -1))
  (fm/hook rustic-mode-hook subword-mode)
  (fm/hook rustic-mode-hook tree-sitter-mode)))

(fm/pkg lsp-mode
 (fm/after lsp-mode
  (fm/dim lsp-mode "Lsp")
  ;; (fm/var lsp-eldoc-render-all t)
  ;; (fm/var lsp-use-plists t)
  (fm/var lsp-completion-provider :none) ; Company-capf is already set
  (fm/var lsp-headerline-breadcrumb-enable t)
  (fm/var lsp-restart 'ignore)
  (fm/var lsp-enable-snippet t)
  (fm/var lsp-keymap-prefix "C-c")
  (fm/var lsp-idle-delay 0.25)
  (fm/var lsp-file-watch-threshold nil)
  ;; (fm/var lsp-enable-semantic-highlighting t)
  (fm/var lsp-enable-indentation t)
  (fm/var lsp-enable-on-type-formatting t)
  (fm/var lsp-before-save-edits nil)
  (fm/var lsp-auto-configure t)
  ;; (fm/var lsp-signature-doc-lines 1)
  ;; (fm/var lsp-signature-auto-activate t)
  (fm/var lsp-signature-render-documentation t)
  (fm/var lsp-modeline-code-actions-enable nil)
  (fm/face lsp-face-highlight-read
   :inherit highlight)
  (fm/face lsp-face-semhl-namespace
   :foreground "CadetBlue")
  (fm/face lsp-face-semhl-enum
   :foreground "MediumPurple")
  (fm/face lsp-face-semhl-struct
   :foreground "BlueViolet")
  (fm/after which-key
   (fm/hook lsp-mode-hook lsp-enable-which-key-integration "lsp-mode"))
  (fm/hookn lsp-mode-hook
   (fm/hook before-save-hook lsp-format-buffer "lsp-mode" t)
   (fm/key "C-c x" lsp-ivy-workspace-symbol)
   (fm/key "C-c f" lsp-format-buffer           lsp-mode-map "lsp-mode")
   (fm/key "C-c r" lsp-rename                  lsp-mode-map "lsp-mode")
   (fm/key "C-c t" lsp-describe-thing-at-point lsp-mode-map "lsp-mode")
   (fm/key "C-="   lsp-extend-selection        lsp-mode-map "lsp-mode")
   (fm/key "M-RET" lsp-execute-code-action     lsp-mode-map "lsp-mode")))
 (fm/after lsp-headerline
  (fm/var lsp-headerline-breadcrumb-icons-enable nil))
 (fm/after lsp-semantic-tokens
  (fm/var lsp-semantic-tokens-apply-modifiers t))
 (fm/after lsp-diagnostics
  (fm/var lsp-diagnostics-attributes
   '((unnecessary :underline "DarkOrange")
     (deprecated :strike-through t))))
 (fm/after lsp-lens-face
  (fm/face lsp-lens-face
   :inherit shadow)
  (fm/face lsp-lens-mouse-face
   :inherit link))
 (fm/after lsp-rust
  (fm/var lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (fm/var lsp-rust-analyzer-proc-macro-enable t)
  (fm/var lsp-rust-analyzer-use-client-watching nil)
  (fm/var lsp-rust-analyzer-server-command "/usr/bin/rust-analyzer")
  (fm/var lsp-rust-racer-completion nil)
  (fm/var lsp-rust-build-bin t)
  (fm/var lsp-rust-build-lib t)
  (fm/var lsp-rust-clippy-preference "on")
  (fm/var lsp-rust-analyzer-server-display-inlay-hints t)
  (fm/var lsp-rust-analyzer-display-chaining-hints t)
  (fm/var lsp-rust-analyzer-display-parameter-hints t)
  (fm/var lsp-rust-all-features t)
  (fm/var lsp-rust-all-targets t)
  (fm/var lsp-rust-build-on-save t)
  (fm/var lsp-rust-unstable-features t)
  (fm/var lsp-rust-full-docs t)
  ;; (fm/var lsp-rust-analyzer-cargo-watch-command "clippy")
  (fm/var lsp-rust-analyzer-max-inlay-hint-length 50)
  (fm/var lsp-rust-analyzer-inlay-type-format "%s")
  (fm/var lsp-rust-analyzer-inlay-type-space-format ": %s")
  (fm/var lsp-rust-analyzer-inlay-chain-format "➔ %s")
  (fm/var lsp-rust-analyzer-inlay-chain-space-format " %s")
  (fm/face lsp-rust-analyzer-inlay-type-face
   :inherit font-lock-type-face
   :height 0.8
   :background "HoneyDew2")
  (fm/face lsp-rust-analyzer-inlay-param-face
   :height 0.8
   :weight semibold
   :foreground "DimGray"
   :background "Azure2")
  (fm/face lsp-rust-analyzer-inlay-chain-face
   :height 0.8
   :weight semibold
   :foreground "DimGray"
   :background "Khaki")))

(fm/pkg lsp-treemacs
 (fm/key "C-c e" lsp-treemacs-errors-list)
 (fm/key "C-c s" lsp-treemacs-symbols))

(fm/pkg lsp-ivy
 (autoload 'lsp-ivy-workspace-symbol "lsp-ivy"))

(fm/pkg lsp-ui
 (fm/after lsp-ui-flycheck
  (fm/var lsp-ui-flycheck-enable t)
  (fm/var lsp-ui-flycheck-list-mode t))
 (fm/after lsp-ui-doc
  (fm/var lsp-ui-doc-enable nil)
  (fm/var lsp-ui-doc-alignment 'frame)
  (fm/var lsp-ui-doc-header t)
  (fm/var lsp-ui-doc-include-signature t)
  (fm/var lsp-ui-doc-max-height 30)
  (fm/face lsp-ui-doc-background
   :background "Gray92")
  (fm/face lsp-ui-doc-header
   :foreground "Gray98"
   :background "RoyalBlue"))
 (fm/after lsp-ui-peek
  (fm/var lsp-ui-peek-list-width 30)
  (fm/var lsp-ui-peek-always-show t)
  (fm/face lsp-ui-peek-list
   :background "Gray96")
  (fm/face lsp-ui-peek-peek
   :background "Gray92")
  (fm/face lsp-ui-peek-selection
   :background "LightSkyBlue1")
  (fm/face lsp-ui-peek-header
   :foreground "Gray98"
   :background "RoyalBlue")
  (fm/face lsp-ui-peek-filename
   :foreground "RoyalBlue"))
 (fm/after lsp-ui-sideline
  (fm/var lsp-ui-sideline-enable nil))
 (fm/after lsp-ui
  (fm/hookn lsp-ui-mode-hook
   (fm/face lsp-ui-doc-border "Gray50"))
  (fm/key "M-."   lsp-ui-peek-find-definitions lsp-ui-mode-map "lsp-ui-peek")
  (fm/key "M-?"   lsp-ui-peek-find-references  lsp-ui-mode-map "lsp-ui-peek")
  (fm/key "C-c h" lsp-ui-doc-glance            lsp-ui-mode-map "lsp-ui-doc")))

(when (string-equal (system-name) "neuron")
 (fm/pkg atom-dark-theme
  (load-theme 'atom-dark t)))

;; Print startup stats.
(message "Startup in %s (%d GC runs)" (emacs-init-time) gcs-done)

(provide 'init)
;;; init ends here

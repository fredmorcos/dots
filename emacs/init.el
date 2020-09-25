;;; init --- Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Directories
(defconst emacs-extra-dir "/home/fred/Workspace/dots/emacs/extra")
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
(push emacs-extra-dir load-path)

;; hooks
(defmacro fm/hook (hook func &optional pkg local)
 "Autoload FUNC from PKG and add it to LOCAL HOOK."
 `(progn
   ,(when pkg `(autoload ',func ,pkg))
   ,(when pkg `(declare-function ,func ,pkg))
   ,(if local
     `(add-hook ',hook #',func 10 t)
     `(add-hook ',hook #',func))))

(defmacro fm/hooks (hooks func &optional pkg local)
 "Autoload FUNC from PKG and it to LOCAL HOOKS."
 `(progn
   ,(when pkg `(autoload ',func ,pkg))
   ,(when pkg `(declare-function ,func ,pkg))
   ,@(let ((exps nil))
      (progn (if local
              (while hooks
               (push `(add-hook ',(pop hooks) #',func 10 t) exps)
               exps)
              (while hooks
               (push `(add-hook ',(pop hooks) #',func) exps)
               exps))
       exps))))

(defmacro fm/hookn (hook &rest body)
 "Hook (lambda () BODY) to HOOK."
 `(add-hook ',hook (lambda () (progn ,@body)) 10))

(defmacro fm/hookn-interactive (hook &rest body)
 "Hook (lambda () BODY) to HOOK."
 `(add-hook ',hook (lambda () (interactive) (progn ,@body)) 10))

;; bind keys
(defmacro fm/key (key func &optional pkg-keymap pkg)
 "Define KEY in PKG-KEYMAP to call FUNC from PKG."
 (cond
  ((not pkg-keymap)
   `(global-set-key (kbd ,key) ,(if func `#',func nil)))
  (t
   `(progn
     (eval-when-compile (defvar ,pkg-keymap))
     ,(when (and func pkg)
       `(autoload ',func ,pkg)
       `(declare-function ,func ,pkg))
     (define-key ,pkg-keymap (kbd ,key) ,(if func `#',func nil))))))

;; diminish
(defun fm/dim-helper (mode text)
 "Diminish MODE to TEXT helper."
 (let ((element (seq-find (lambda (x) (eq (car x) mode)) minor-mode-alist))
       (new-text (if text (concat " " text) "")))
  (if element
   (setf (nth 1 element) new-text)
   (push `(,mode ,new-text) minor-mode-alist))))

(defmacro fm/dim (mode &optional text enforce)
 "Diminish MODE to TEXT, this happens on mode hook unless ENFORCE is set."
 (let ((hook (intern (concat (symbol-name mode) "-hook"))))
  (if enforce
   `(fm/dim-helper ',mode ,text)
   `(fm/hookn ,hook (fm/dim-helper ',mode ,text)))))

;; faces
(defmacro fm/face (face &rest props)
 "Set FACE properties to PROPS."
 `(if (facep ',face)
   (custom-set-faces '(,face ((t ,@props))))
   (if (stringp (car ',props))
    (fm/var ,face (car ',props))
    (fm/var ,face '((t ,@props))))))

;; vars
(defmacro fm/vars (&rest customs)
 "Custom-Set the CUSTOMS list of var-val pairs."
 `(custom-set-variables
   ,@(let ((exps nil))
     (while customs
      (push `(quote (,(pop customs) ,(pop customs))) exps))
     exps)))

(defmacro fm/var (var val)
 "Custom-Set VAR to VAL."
 `(custom-set-variables '(,var ,val)))

;; lazy loading
(defmacro fm/after (pkg &rest body)
 "Execute BODY when PKG is loaded."
 `(eval-after-load ',pkg '(progn ,@body)))

;; modes
(defmacro fm/mode (ext mode &optional pkg)
 "Autoload and enable MODE from PKG for file extension EXT."
 `(progn
   ,(when pkg `(autoload ',mode ,pkg))
   (push '(,(concat "\\" ext "\\'") . ,mode) auto-mode-alist)))

;; qol
(defun fm/replace-escapes ()
 "Replace strange newline escapes with proper UNIX newlines."
 (interactive)
 (goto-char (point-min))
 (while (search-forward "\\n" nil t) (replace-match (char-to-string ?\n) nil t))
 (goto-char (point-min))
 (while (search-forward "\\t" nil t) (replace-match (char-to-string ?\t) nil t))
 (goto-char (point-min))
 (while (search-forward "" nil t) (replace-match "" nil t)))

(fm/key "C-x e" fm/replace-escapes)

(defun fm/move-line-up ()
 "Move a line up."
 (interactive)
 (transpose-lines 1)
 (forward-line -2))

(defun fm/move-line-down ()
 "Move a line down."
 (interactive)
 (forward-line 1)
 (transpose-lines 1)
 (forward-line -1))

(fm/key "<M-up>"   fm/move-line-up)
(fm/key "<M-down>" fm/move-line-down)

(defun fm/generate-password ()
 "Generate a password and insert it."
 (interactive)
 (shell-command "pwgen -c -n -y -s -B -1 34 1" (current-buffer)))

(defun fm/insert-pair (left right &optional region-only)
 "Insert LEFT & RIGHT in or around text if REGION-ONLY is t."
 (if (use-region-p)
  (let ((begin (region-beginning))
        (end (region-end)))
   (progn
    (goto-char begin)
    (insert-char left)
    (goto-char (+ 1 end))
    (insert-char right)))
  (progn
   (insert-char left)
   (when (not region-only)
    (progn
     (insert-char right)
     (backward-char))))))

(fm/key "("  (lambda () (interactive) (fm/insert-pair ?\( ?\) t)))
(fm/key "'"  (lambda () (interactive) (fm/insert-pair ?\' ?\' t)))
(fm/key "\"" (lambda () (interactive) (fm/insert-pair ?\" ?\" t)))

;; nativecomp
(fm/var comp-deferred-compilation t)

;; package
(fm/var package-archives
 '(("gnu"   . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")
   ("org"   . "https://orgmode.org/elpa/")))

(defmacro fm/pkg (pkg &rest body)
 "Install PKG if not already installed and execute BODY."
 `(progn
   (defvar packages-refreshed nil)
   (if (not (package-installed-p ',pkg))
    (progn
     (when (not packages-refreshed)
      (progn
       (package-refresh-contents)
       (setq packages-refreshed t)))
     (package-install ',pkg))
    (push ',pkg package-selected-packages))
   (progn ,@body)))

;; subr
(fm/var read-process-output-max (* 1024 1024))
(defalias 'yes-or-no-p 'y-or-n-p)

;; startup
(fm/var inhibit-startup-screen t)
(fm/var inhibit-startup-message t)
(fm/var inhibit-startup-buffer-menu t)
(fm/var initial-scratch-message nil)
(fm/var initial-major-mode 'fundamental-mode)
(fm/var auto-save-list-file-prefix nil)

;; scroll-bar
(fm/var horizontal-scroll-bar-mode nil)
(fm/var scroll-conservatively 4)
(fm/var scroll-margin 3)
(fm/var hscroll-margin 3)
(fm/var hscroll-step 1)
(fm/var auto-hscroll-mode 'current-line)

;; frame
(fm/var blink-cursor-mode nil)
(fm/var frame-resize-pixelwise t)
(fm/var frame-title-format "%b - emacs")

;; mode-line
(fm/face mode-line-buffer-id :foreground "RoyalBlue")
(fm/face mode-line-highlight :inherit mode-line-emphasis :background "PowderBlue")

;; faces
(fm/face link :foreground "RoyalBlue3" :underline (:color "LightSteelBlue3"))
(fm/face highlight :inverse-video t)
(fm/face error :foreground "Red3")

;; font-lock
(fm/face font-lock-function-name-face :inherit font-lock-builtin-face)
(fm/face font-lock-keyword-face :foreground "MediumSlateBlue")
(fm/face font-lock-type-face :foreground "DarkGreen")
(fm/face font-lock-variable-name-face :foreground "DarkCyan")
(fm/face font-lock-string-face :foreground "OliveDrab")
(fm/face font-lock-comment-face :foreground "DarkMagenta")
(fm/face font-lock-warning-face :foreground "Orange3")
(fm/face font-lock-constant-face :foreground "CornflowerBlue")

;; saveplace
(fm/var save-place t)
(fm/var save-place-mode t)
(fm/var save-place-file emacs-places-file)

;; savehist
(fm/var savehist-mode t)
(fm/var history-delete-duplicates t)
(fm/var history-length 100)

;; recentf
(fm/var recentf-save-file emacs-recentf-file)
(fm/var recentf-max-menu-items 50)
(fm/var recentf-max-saved-items 100)
(fm/var recentf-mode t)
(fm/var recentf-exclude `(,emacs-elpa-dir
                          ,(expand-file-name "~/Oracle")
                          ,(expand-file-name "~/OracleWorkTrees")))
(fm/hook kill-emacs-hook recentf-cleanup "recentf")

;; files
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
(fm/var coding-system-for-write 'utf-8-unix)

;; cua-base
(cua-selection-mode 1)

;; help
(fm/var help-window-select t)

;; window
(fm/var split-height-threshold 160)
(fm/var even-window-sizes 'width-only)

;; mouse
(fm/var mouse-yank-at-point t)

;; windmove
(windmove-default-keybindings)
(windmove-delete-default-keybindings)

;; simple
(fm/var undo-limit (* 1024 1024))
(fm/var suggest-key-bindings 10)
(fm/var column-number-mode t)
(fm/var line-number-mode nil)
(fm/var auto-save-mode t)
(fm/var save-interprogram-paste-before-kill t)
(fm/key "<mouse-4>" previous-line)
(fm/key "<mouse-5>" next-line)
(fm/hook before-save-hook delete-trailing-whitespace)

;; indent
(fm/var indent-tabs-mode nil)

(fm/after xref
 (fm/var xref-backend-functions '()))

(fm/after bindings
 (fm/var column-number-indicator-zero-based nil))

(fm/after uniquify
 (fm/var uniquify-buffer-name-style 'forward))

(fm/after vc
 (fm/var vc-make-backup-files t))

(fm/after newcomment
 (fm/var comment-fill-column 80))

(fm/after fill
 (fm/var fill-column 90)
 (fm/var colon-double-space t)
 (fm/var default-justification 'left))

(fm/after ediff-wind
 (fm/var ediff-split-window-function #'split-window-horizontally)
 (fm/var ediff-window-setup-function #'ediff-setup-windows-plain))

(fm/after elec-pair
 (fm/var electric-pair-pairs '((?\[ . ?\]) (?\{ . ?\}))))
(electric-pair-mode)
(electric-layout-mode)

(fm/after display-line-numbers
 (fm/var display-line-numbers-grow-only t)
 (fm/var display-line-numbers-width-start t)
 (fm/face line-number :foreground "Gray85")
 (fm/face line-number-current-line :foreground "Gray70"))
(fm/after prog-mode
 (fm/hook prog-mode-hook display-line-numbers-mode))

(fm/after hl-line
 (fm/face hl-line :background "Gray95" :extend nil))
(fm/after prog-mode
 (fm/hook prog-mode-hook hl-line-mode))

(fm/after abbrev
 (fm/dim abbrev-mode "Ab" t))

(fm/after whitespace
 (fm/dim whitespace-mode "Ws")
 (fm/var whitespace-line-column 90)
 (fm/var show-trailing-whitespace nil)
 (fm/var whitespace-action '(cleanup))
 (fm/var whitespace-style
  '(face tabs lines empty tab-mark indentation indentation::tab indentation::space
    space-after-tab space-after-tab::tab space-after-tab::space space-before-tab
    space-before-tab::tab space-before-tab::space)))
(fm/after elisp-mode
 (fm/hook emacs-lisp-mode-hook whitespace-mode))
(fm/after make-mode
 (fm/hook makefile-mode-hook whitespace-mode))
(fm/after hledger-mode
 (fm/hook hledger-mode-hook whitespace-mode))

(fm/after elisp-mode
 (fm/var lisp-indent-offset 1)
 (fm/var lisp-indent-function #'common-lisp-indent-function))
(fm/mode "emacs" emacs-lisp-mode)
(fm/mode ".config/emacs/init" emacs-lisp-mode)

(fm/after text-mode
 (fm/hookn text-mode-hook (toggle-truncate-lines t)))
(fm/mode "Passwords.txt" text-mode)
(fm/mode "Passwords_old.txt" text-mode)

(fm/after eldoc
 (fm/dim eldoc-mode "Ed"))

(fm/after prog-mode
 (fm/hook prog-mode-hook eldoc-mode))

(fm/after paren
 (fm/var show-paren-when-point-inside-paren t)
 (fm/var show-paren-style 'mixed)
 (fm/var show-paren-highlight-openparen t)
 (fm/face show-paren-match :background "PowderBlue")
 (fm/face show-paren-mismatch :background "LightSalmon")
 (fm/face show-paren-match-expression :background "Lavender"))
(fm/after prog-mode
 (fm/hook prog-mode-hook show-paren-mode))

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
 (fm/face flyspell-duplicate :underline "YellowGreen")
 (fm/face flyspell-incorrect :underline "Orchid"))
(fm/after text-mode
 (fm/hook text-mode-hook flyspell-mode))
(fm/after prog-mode
 (fm/hook prog-mode-hook flyspell-prog-mode))

(fm/after sh-script
 (fm/hookn sh-mode-hook
  (fm/hook after-save-hook executable-make-buffer-file-executable-if-script-p)))

(fm/after llvm-mode
 (fm/hookn llvm-mode-hook (toggle-truncate-lines t)))
(fm/mode ".ll" llvm-mode "llvm-mode")

(fm/after cc-mode
 ;; (fm/hook c-mode-hook lsp-deferred)
 (fm/hookn c-mode-hook
  (fm/key "(" nil c-mode-map)
  (fm/after newcomment
   (fm/var comment-style 'extra-line))))

(fm/after cc-vars
 (fm/var c-mark-wrong-style-of-comment t)
 (fm/var c-default-style
  '((other . "user"))))

(fm/after face-remap
 (fm/dim buffer-face-mode)
 (fm/face variable-pitch :family "Fira Sans Condensed" :height 130))

;; js-mode
(fm/mode ".hocon" js-mode)

(fm/pkg json-mode)
(fm/pkg toml-mode)
(fm/pkg cmake-mode)
(fm/pkg dockerfile-mode)
(fm/pkg markdown-mode)
(fm/pkg smex)

(fm/pkg systemd
 (fm/hook systemd-mode-hook company-mode))

(fm/pkg bbdb
 (fm/after bbdb
  (fm/var bbdb-file "~/Documents/Important/Contacts")))

(fm/pkg org-bullets
 (fm/var org-bullets-bullet-list '(" "))
 (fm/hook org-mode-hook org-bullets-mode))

(fm/pkg org
 (fm/after org
  (fm/var org-agenda-include-diary t)
  (fm/var org-agenda-files
   `(,(expand-file-name "~/Documents/Important/Agenda.org")
     ,(expand-file-name "~/Documents/Important/Passwords/Passwords.org")))
  (fm/var org-cycle-separator-lines 0)
  (fm/var org-startup-folded nil)
  (fm/var org-ellipsis "  ▾")
  (fm/var org-hide-leading-stars t)
  (fm/var org-hide-emphasis-markers t)
  (fm/var org-fontify-whole-heading-line t)
  (fm/var org-fontify-done-headline t)
  (fm/var org-startup-indented t)
  (fm/var org-property-format "%s %s")
  (fm/face org-document-title :foreground "MidnightBlue" :height 1.4)
  (fm/face org-target :slant italic :foreground "Tan" :height 0.8)
  (fm/face org-table :height 0.8 :foreground "NavyBlue")
  (fm/face org-ellipsis :foreground "SteelBlue")
  (fm/face org-level-1 :family "Fira Sans Condensed"
   :foreground "SlateBlue" :height 1.2 :inherit (outline-1))
  (fm/face org-level-2 :family "Fira Sans Condensed"
   :foreground "IndianRed3" :height 1.1 :inherit (outline-2))
  (fm/face org-level-3 :family "Fira Sans Condensed"
   :foreground "SteelBlue" :inherit (outline-3))
  (fm/face org-todo :foreground "Maroon" :height 0.8 :bold t)
  (fm/face org-done :foreground "ForestGreen" :height 0.8 :bold t)
  (fm/face org-drawer :foreground "Snow3" :height 0.8)
  (fm/face org-special-keyword :inherit font-lock-keyword-face :height 0.8 :bold t)
  (fm/face org-table :foreground "RoyalBlue")
  (fm/key "M-p" fm/generate-password)
  (fm/key "C-c a" org-agenda)
  (fm/hookn org-mode-hook
   (setq-local left-margin-width 2)
   (setq-local right-margin-width 2)
   (setq-local scroll-margin 0))))

(fm/pkg org-variable-pitch
 (fm/after org-variable-pitch
  (fm/dim org-variable-pitch-minor-mode)
  (fm/hookn org-variable-pitch-minor-mode-hook
   (setq-local cursor-type 'bar)))
 (fm/after org
  (fm/hook org-mode-hook org-variable-pitch-minor-mode)))

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
  (fm/var ivy-virtual-abbreviate 'full)
  (fm/var ivy-initial-inputs-alist nil)
  (fm/var ivy-extra-directories nil)
  (fm/var ivy-re-builders-alist
   '((t . ivy--regex-ignore-order) (t . ivy--regex-plus)))
  (fm/key "<RET>" ivy-alt-done ivy-minibuffer-map "ivy"))
 (ivy-mode))

(fm/pkg ivy-rich
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
  (fm/var magit-repository-directories
   '(("~/Workspace" . 3) ("~/Oracle" . 3) ("~/OracleWorkTrees" . 3)))
  (fm/hook after-save-hook magit-after-save-refresh-status "magit"))
 (fm/key "C-x g" magit-status))

(fm/pkg indent-guide
 (fm/after indent-guide
  (fm/face indent-guide-face :foreground "gray80"))
 (fm/after json-mode
  (fm/hook json-mode-hook indent-guide-mode)))

(fm/pkg projectile
 (fm/after projectile
  (fm/dim projectile-mode "Prj")
  (fm/var projectile-project-search-path '("~/Workspace"))
  (fm/var projectile-sort-order '(recently-active))
  (fm/var projectile-enable-caching t)
  (fm/var projectile-completion-system 'ivy)
  (fm/key "C-x p" projectile-command-map projectile-mode-map "projectile"))
 (projectile-mode))

(fm/pkg counsel-projectile
 (fm/after counsel (counsel-projectile-mode)))

(fm/pkg yasnippet
 (fm/after prog-mode
  (fm/hook prog-mode-hook yas-minor-mode))
 (fm/after org
  (fm/hook org-mode-hook yas-minor-mode))
 (fm/after hledger-mode
  (fm/hook hledger-mode-hook yas-minor-mode))
 (fm/after yasnippet
  (fm/dim yas-minor-mode "Ys")
  (fm/hook yas-minor-mode-hook yas-reload-all "yasnippet")
  (defvar yas-snippet-dirs)
  (push (expand-file-name "~/Workspace/dots/emacs/snippets") yas-snippet-dirs)))

(fm/pkg yasnippet-snippets)

(fm/pkg diff-hl
 (fm/after diff-hl
  (fm/var diff-hl-draw-borders nil)
  (fm/var diff-hl-flydiff-delay 0.1)
  (fm/face diff-hl-delete :background "RosyBrown1")
  (fm/face diff-hl-insert :background "DarkSeaGreen2")
  (fm/face diff-hl-change :background "PowderBlue"))
 (fm/after prog-mode
  (fm/hook prog-mode-hook diff-hl-mode))
 (fm/after magit-mode
  (fm/hook magit-post-refresh-hook diff-hl-magit-post-refresh "diff-hl")))

(fm/pkg symbol-overlay
 (fm/after symbol-overlay
  (fm/dim symbol-overlay-mode "Sy")
  (fm/var symbol-overlay-idle-time 0.1)
  (fm/face symbol-overlay-default-face :background "HoneyDew2")
  (fm/hookn symbol-overlay-mode-hook
   (fm/key "M->" symbol-overlay-jump-next symbol-overlay-mode-map)
   (fm/key "M-<" symbol-overlay-jump-prev symbol-overlay-mode-map)))
 (fm/after sh-script
  (fm/hook sh-mode-hook symbol-overlay-mode))
 (fm/after elisp-mode
  (fm/hook emacs-lisp-mode-hook symbol-overlay-mode))
 (fm/after hledger-mode
  (fm/hook hledger-mode-hook symbol-overlay-mode))
 (fm/after cc-mode
  (fm/hook java-mode-hook symbol-overlay-mode))
 (fm/after python
  (fm/hook python-mode-hook symbol-overlay-mode)))

(fm/pkg multiple-cursors
 (fm/after multiple-cursors
  (fm/var mc/always-run-for-all t)
  (fm/face mc/cursor-bar-face :background "Gray40" :foreground "White")
  (fm/face mc/cursor-face :background "Gray50" :foreground "White"))
 (fm/key "C-c C-v" mc/edit-lines)
 (fm/key "C->" mc/mark-next-like-this)
 (fm/key "C-<" mc/mark-previous-like-this)
 (fm/key "C-S-<mouse-1>" mc/add-cursor-on-click))

(fm/pkg hledger-mode
 (fm/after hledger-mode
  (fm/var hledger-currency-string "EUR")
  (fm/var hledger-current-overlay t)
  (fm/var hledger-comments-column 1)
  (fm/face hledger-description-face :inherit font-lock-keyword-face)
  (fm/face hledger-amount-face :inherit font-lock-constant-face :inverse-video t)
  ;; (fm/face hledger-date-face :inherit font-lock-string-face :inverse-video t)
  (fm/hookn hledger-mode-hook
   (toggle-truncate-lines t)
   (setq tab-width 1)))
 (fm/mode ".journal" hledger-mode)
 (fm/mode ".ledger"  hledger-mode))

(fm/pkg flycheck
 (fm/after flycheck
  (fm/var flycheck-checker-error-threshold nil)
  (fm/var flycheck-mode-line-prefix "Fc")
  (fm/var flycheck-check-syntax-automatically
   '(idle-change new-line mode-enabled idle-buffer-switch))
  (fm/var flycheck-idle-change-delay 0.25)
  (fm/var flycheck-idle-buffer-switch-delay 0.25)
  (fm/face flycheck-error :underline "Red1")
  (fm/face flycheck-info :underline "ForestGreen")
  (fm/face flycheck-warning :underline "DarkOrange")
  (fm/hookn flycheck-mode-hook
   (fm/key "M-n" flycheck-next-error flycheck-mode-map "flycheck")
   (fm/key "M-p" flycheck-previous-error flycheck-mode-map "flycheck")))
 (fm/after prog-mode
  (fm/hook prog-mode-hook flycheck-mode))
 (fm/after cc-mode
  (fm/hookn java-mode-hook
   (flycheck-mode -1)))
 (fm/after python
  (fm/hookn python-mode-hook
   (flycheck-mode -1))))

(fm/pkg flycheck-posframe
 (fm/after flycheck-posframe
  (fm/var flycheck-posframe-position 'window-bottom-right-corner)
  (fm/var flycheck-posframe-border-width 1)
  (fm/var flycheck-posframe-warnings-prefix "Warning: ")
  (fm/var flycheck-posframe-error-prefix "Error: ")
  (fm/var flycheck-posframe-prefix "Info: ")
  (fm/face flycheck-posframe-background-face :background "CornSilk")
  (fm/face flycheck-posframe-warning-face :foreground "DarkOrange")
  (fm/face flycheck-posframe-border-face :background "Wheat" :foreground "Wheat")
  (fm/face flycheck-posframe-error-face :foreground "DarkRed"))
 (fm/after flycheck
  (fm/hook flycheck-mode-hook flycheck-posframe-mode))
 (fm/after company
  (fm/hook flycheck-posframe-inhibit-functions company--active-p "company")
  (fm/hook flycheck-posframe-inhibit-functions
   (lambda (&rest _) (bound-and-true-p company-backend)))))

(fm/pkg company
 (fm/after company
  (fm/dim company-mode "Co")
  (setq-default company-backends
   '((company-capf company-keywords company-files company-yasnippet)))
  (fm/var completion-ignore-case t)
  (fm/var company-echo-truncate-lines nil)
  (fm/var company-selection-wrap-around t)
  (fm/var company-tooltip-minimum 10)
  (fm/var company-tooltip-limit 15)
  (fm/var company-tooltip-align-annotations t)
  ;; (fm/var company-idle-delay 0.1)
  (fm/var company-occurence-weight-function 'company-occurrence-prefer-any-closest)
  ;; (fm/var company-auto-commit t)
  ;; (fm/var company-auto-commit-chars '(32 95 41 46 39 47))
  (fm/var company-frontends
   '(company-echo-metadata-frontend company-pseudo-tooltip-frontend))
  (fm/var company-transformers
   '(company-sort-by-occurrence company-sort-by-backend-importance
     company-sort-prefer-same-case-prefix))
  (fm/face company-tooltip :background "gray95"))
 (fm/after prog-mode
  (fm/hook prog-mode-hook company-mode)))

(fm/pkg company-posframe
 (fm/after company-posframe
  (fm/dim company-posframe-mode)
  (fm/var company-posframe-show-params
   '(:internal-border-width 1 :internal-border-color "gray60")))
 (fm/after company
  (fm/hook company-mode-hook company-posframe-mode "company-posframe")))

(fm/pkg rustic
 (fm/after rustic
  (fm/var rustic-lsp-server 'rust-analyzer)
  (fm/var rustic-analyzer-command '("/usr/bin/rust-analyzer"))
  (fm/var rustic-lsp-format t)
  (fm/var rustic-indent-offset 2)
  (fm/var rustic-always-locate-project-on-open t)
  (fm/hookn rustic-mode-hook
   (fm/key "<f5>" rust-dbg-wrap-or-unwrap            rustic-mode-map "rust-mode")
   (fm/after lsp-rust
    (fm/key "<f6>" lsp-rust-analyzer-expand-macro     rustic-mode-map "lsp-rust")
    (fm/key "<f7>" lsp-rust-analyzer-join-lines       rustic-mode-map "lsp-rust")
    (fm/key "<f8>" lsp-rust-analyzer-inlay-hints-mode rustic-mode-map "lsp-rust")
    (fm/key "<f9>" lsp-rust-analyzer-expand-macro     rustic-mode-map "lsp-rust"))
   (electric-quote-local-mode -1))
  (fm/hook rustic-mode-hook subword-mode)))

(fm/pkg lsp-mode
 (fm/after lsp-mode
  ;; (fm/var lsp-use-plists t)
  (fm/var lsp-completion-provider :none) ; Company-capf is already set
  (fm/var lsp-headerline-breadcrumb-enable t)
  (fm/var lsp-restart 'ignore)
  (fm/var lsp-enable-snippet t)
  (fm/var lsp-keymap-prefix "C-c")
  (fm/var lsp-idle-delay 0.25)
  (fm/var lsp-file-watch-threshold nil)
  (fm/var lsp-enable-semantic-highlighting nil)
  (fm/var lsp-enable-indentation t)
  (fm/var lsp-enable-on-type-formatting t)
  (fm/var lsp-before-save-edits nil)
  (fm/var lsp-auto-configure t)
  (fm/var lsp-signature-doc-lines 1)
  (fm/var lsp-signature-auto-activate t)
  (fm/var lsp-signature-render-documentation t)
  (fm/var lsp-modeline-code-actions-enable nil)
  (fm/face lsp-face-highlight-read :inherit highlight)
  (fm/face lsp-face-semhl-namespace :foreground "CadetBlue")
  (fm/face lsp-face-semhl-enum :foreground "MediumPurple")
  (fm/face lsp-face-semhl-struct :foreground "BlueViolet")
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
 (fm/after lsp-diagnostics
  (fm/var lsp-diagnostics-attributes
   '((unnecessary :background "Gray90") (deprecated  :strike-through t))))
 (fm/after lsp-lens-face
  (fm/face lsp-lens-face :inherit shadow)
  (fm/face lsp-lens-mouse-face :inherit link))
 (fm/after lsp-rust
  (fm/var lsp-rust-racer-completion nil)
  (fm/var lsp-rust-build-bin t)
  (fm/var lsp-rust-build-lib t)
  (fm/var lsp-rust-clippy-preference "on")
  (fm/var lsp-rust-analyzer-server-display-inlay-hints t)
  (fm/var lsp-rust-analyzer-display-chaining-hints t)
  (fm/var lsp-rust-analyzer-display-parameter-hints t)
  (fm/var lsp-rust-all-features t)
  (fm/var lsp-rust-all-targets t)
  ;; (fm/var lsp-rust-build-on-save t)
  (fm/var lsp-rust-full-docs t)
  (fm/var lsp-rust-analyzer-cargo-watch-command "clippy")
  (fm/var lsp-rust-analyzer-max-inlay-hint-length 15)
  (fm/var lsp-rust-analyzer-inlay-type-format "%s")
  (fm/var lsp-rust-analyzer-inlay-type-space-format ": %s")
  (fm/var lsp-rust-analyzer-inlay-chain-format "➔ %s")
  (fm/var lsp-rust-analyzer-inlay-chain-space-format " %s")
  (fm/face lsp-rust-analyzer-inlay-type-face
   ;; :height 0.7 :weight semibold :foreground "DimGray" :background "Gray92")
   :inherit font-lock-type-face :height 0.8 :background "HoneyDew2")
  (fm/face lsp-rust-analyzer-inlay-param-face
   :height 0.8 :weight semibold :foreground "DimGray" :background "Azure2")
  (fm/face lsp-rust-analyzer-inlay-chain-face
   :height 0.8 :weight semibold :foreground "DimGray" :background "Khaki"))
 (fm/after lsp-clangd
  (fm/var lsp-clients-clangd-args
   (append lsp-clients-clangd-args
    '("--background-index" "--completion-style=detailed" "--pch-storage=memory"
      "--clang-tidy" "--header-insertion=iwyu" "--header-insertion-decorators"
      "-j=6" "--suggest-missing-includes" "--completion-parse=always")))))

(fm/pkg lsp-ivy
 (autoload 'lsp-ivy-workspace-symbol "lsp-ivy"))

(fm/pkg lsp-ui
 (fm/after lsp-ui-flycheck
  (fm/var lsp-ui-flycheck-enable t)
  (fm/var lsp-ui-flycheck-list-mode t))
 (fm/after lsp-ui-doc
  (fm/var lsp-ui-doc-enable nil)
  (fm/var lsp-ui-doc-alignment 'window)
  (fm/var lsp-ui-doc-header t)
  (fm/var lsp-ui-doc-include-signature t)
  (fm/face lsp-ui-doc-border "Gray70")
  (fm/face lsp-ui-doc-background :background "Gray95")
  (fm/face lsp-ui-doc-header :background "Pale Turquoise"))
 (fm/after lsp-ui-peek
  (fm/face lsp-ui-peek-list :background "Gray95")
  (fm/face lsp-ui-peek-peek :background "Gray95")
  (fm/face lsp-ui-peek-header :foreground "Gray95" :background "Gray40")
  (fm/face lsp-ui-peek-filename :foreground "RoyalBlue"))
 (fm/after lsp-ui-sideline
  (fm/var lsp-ui-sideline-enable nil))
 (fm/after lsp-ui
  (fm/hookn lsp-ui-mode-hook
   (fm/key "M-."   lsp-ui-peek-find-definitions lsp-ui-mode-map "lsp-ui-peek")
   (fm/key "M-?"   lsp-ui-peek-find-references  lsp-ui-mode-map "lsp-ui-peek")
   (fm/key "C-c h" lsp-ui-doc-glance            lsp-ui-mode-map "lsp-ui-doc"))))

(fm/pkg dumb-jump
 (fm/after dumb-jump
  (fm/var dumb-jump-selector 'ivy)
  (fm/var dumb-jump-window 'other))
 (fm/after python
  (fm/hookn python-mode-hook
   (fm/hook xref-backend-functions dumb-jump-xref-activate "dumb-jump" t)))
 (fm/after cc-mode
  (fm/hookn java-mode-hook
   (fm/hook xref-backend-functions dumb-jump-xref-activate "dumb-jump" t))))

(setq file-name-handler-alist nil)
(message "Startup in %s" (emacs-init-time))

(provide 'init)
;;; init ends here

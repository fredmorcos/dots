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

;; package
(package-initialize)
(custom-set-variables
 '(package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/")
                      ("org"   . "https://orgmode.org/elpa/"))))

(defmacro fm/pkg (pkg)
 "Install PKG if not already installed."
 `(progn
   (defvar packages-refreshed nil)
   (if (not (package-installed-p ',pkg))
    (progn
     (when (not packages-refreshed)
      (progn
       (package-refresh-contents)
       (setq packages-refreshed t)))
     (package-install ',pkg))
    (push ',pkg package-selected-packages))))

;; bind keys
(defmacro fm/key (pkg pkg-keymap key func)
 "Define KEY in PKG-KEYMAP to call FUNC from PKG."
 `(progn
   (eval-when-compile (defvar ,pkg-keymap))
   ,(when func `(autoload ',func ,pkg))
   (define-key ,pkg-keymap (kbd ,key) ,(if func `#',func nil))))

;; diminish
(defun fm/diminish-helper (mode text)
 "Diminish MODE to TEXT helper."
 (let ((element (seq-find (lambda (x) (eq (car x) mode)) minor-mode-alist))
       (new-text (if text (concat " " text) "")))
  (if element
   (setf (nth 1 element) new-text)
   (push `(,mode ,new-text) minor-mode-alist))))

(defun fm/diminish (mode &optional text)
 "Diminish MODE to TEXT."
 (let ((hook (intern (concat (symbol-name mode) "-hook"))))
  (symbolp hook)
  (add-hook hook
   (lambda () (progn (fm/diminish-helper mode text))))))

;; qol
(defun fm/replace-escapes ()
 "Replace strange newline escapes with proper UNIX newlines."
 (interactive)
 (goto-char (point-min))
 (while (search-forward "\\n" nil t)
  (replace-match (char-to-string ?\n) nil t))
 (while (search-forward "\\t" nil t)
  (replace-match (char-to-string ?\t) nil t)))

(global-set-key (kbd "C-x e") #'fm/replace-escapes)

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

(global-set-key (kbd "<M-up>")   #'fm/move-line-up)
(global-set-key (kbd "<M-down>") #'fm/move-line-down)

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

;; (fm/key "c-mode" c-mode-map "(" nil)
(global-set-key (kbd "(")  (lambda () (interactive) (fm/insert-pair ?\( ?\) t)))
(global-set-key (kbd "'")  (lambda () (interactive) (fm/insert-pair ?\' ?\' t)))
(global-set-key (kbd "\"") (lambda () (interactive) (fm/insert-pair ?\" ?\" t)))

;; subr
(setq read-process-output-max (* 1024 1024))
(defalias 'yes-or-no-p 'y-or-n-p)

;; electric
(custom-set-variables
 '(electric-pair-pairs '((?\[ . ?\]) (?\{ . ?\})))
 '(electric-layout-mode t)
 '(electric-pair-mode t))

 ;; startup
(setq-default
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-buffer-menu t
 initial-scratch-message nil
 initial-major-mode 'fundamental-mode
 auto-save-list-file-prefix nil)

;; scroll-bar
(setq-default
 horizontal-scroll-bar-mode nil
 scroll-conservatively 4
 hscroll-margin 1
 hscroll-step 1
 auto-hscroll-mode 'current-line)

;; frame
(setq-default
 blink-cursor-mode nil
 frame-resize-pixelwise t
 frame-title-format "%b - emacs")

;; display-line-numbers
(setq-default
 display-line-numbers-grow-only t
 display-line-numbers-width-start t)

(custom-set-faces
 '(line-number              ((t (:foreground "Gray85"))))
 '(line-number-current-line ((t (:foreground "Gray70")))))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; hl-line
(custom-set-faces
 '(hl-line             ((t (:background "Wheat"))))
 '(mode-line-highlight ((t (:background "PowderBlue")))))

(global-hl-line-mode)

;; saveplace
(custom-set-variables
 '(save-place t)
 '(save-place-mode t)
 '(save-place-file emacs-places-file))

;; savehist
(custom-set-variables
 '(savehist-mode t)
 '(history-delete-duplicates t)
 '(history-length 100))

;; recentf
(custom-set-variables
 '(recentf-save-file emacs-recentf-file)
 '(recentf-max-menu-items 50)
 '(recentf-max-saved-items 100)
 '(recentf-mode t)
 '(recentf-exclude `(,emacs-elpa-dir
                     ,(expand-file-name "~/Oracle")
                     ,(expand-file-name "~/OracleWorkTrees"))))

(autoload 'recentf-cleanup "recentf")
(add-hook 'kill-emacs-hook #'recentf-cleanup)

;; files
(custom-set-variables
 '(confirm-kill-processes nil)
 '(auto-save-file-name-transforms `((".*" ,emacs-autosaves-pattern t)))
 '(backup-directory-alist `((".*" . ,emacs-backups-pattern)))
 '(backup-inhibited nil)
 '(make-backup-files t)
 '(delete-old-versions t)
 '(mode-require-final-newline 'visit-save)
 '(require-final-newline 'visit-save)
 '(load-prefer-newer t)
 '(coding-system-for-read 'utf-8-unix)
 '(coding-system-for-write 'utf-8-unix))

;; cua-base
(cua-selection-mode 1)

;; help
(custom-set-variables
 '(help-window-select t))

;; window
(custom-set-variables
 '(split-height-threshold 160)
 '(even-window-sizes 'width-only))

;; windmove
(windmove-default-keybindings)

;; simple
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(custom-set-variables
 '(undo-limit (* 1024 1024))
 '(suggest-key-bindings 10)
 '(column-number-mode t)
 '(line-number-mode nil)
 '(auto-save-mode t)
 '(save-interprogram-paste-before-kill t))

;; bindings
(custom-set-variables
 '(column-number-indicator-zero-based nil))

;; uniquify
(custom-set-variables
 '(uniquify-buffer-name-style 'forward))

;; vc
(custom-set-variables
 '(vc-make-backup-files t))

;; abbrev
(fm/diminish 'abbrev-mode "Ab")

;; newcomment
(custom-set-variables
 '(comment-fill-column 80))

;; fill
(custom-set-variables
 '(fill-column 90)
 '(colon-double-space t)
 '(default-justification 'left))

;; indent
(custom-set-variables
 '(indent-tabs-mode nil))

;; ediff-wind
(custom-set-variables
 '(ediff-split-window-function #'split-window-horizontally)
 '(ediff-window-setup-function #'ediff-setup-windows-plain))

;; whitespace
(custom-set-variables
 '(whitespace-line-column 90)
 '(show-trailing-whitespace nil)
 '(whitespace-action '(cleanup))
 '(whitespace-style
   '(face tabs lines empty tab-mark
     indentation indentation::tab indentation::space
     space-after-tab space-after-tab::tab space-after-tab::space
     space-before-tab space-before-tab::tab space-before-tab::space)))

(fm/diminish 'whitespace-mode "Ws")

(add-hook 'hledger-mode-hook    #'whitespace-mode)
(add-hook 'emacs-lisp-mode-hook #'whitespace-mode)
(add-hook 'makefile-mode-hook   #'whitespace-mode)

;; elisp-mode
(custom-set-variables
 '(lisp-indent-offset 1)
 '(lisp-indent-function #'common-lisp-indent-function))

(push '("\\emacs\\'"              . emacs-lisp-mode) auto-mode-alist)
(push '("\\.config/emacs/init\\'" . emacs-lisp-mode) auto-mode-alist)

;; text-mode
(push '("\\Passwords.txt\\'"     . text-mode) auto-mode-alist)
(push '("\\Passwords_old.txt\\'" . text-mode) auto-mode-alist)

(add-hook 'text-mode-hook (lambda () (toggle-truncate-lines t)))

;; eldoc
(fm/diminish 'eldoc-mode "Ed")
(add-hook 'prog-mode-hook #'eldoc-mode)

;; paren
(custom-set-variables
 '(show-paren-when-point-inside-paren t)
 '(show-paren-style 'mixed)
 '(show-paren-highlight-openparen t))

(custom-set-faces
 '(show-paren-match            ((t (:background "PowderBlue"))))
 '(show-paren-match-expression ((t (:background "Lavender"))))
 '(show-paren-mismatch         ((t (:background "LightSalmon")))))

(add-hook 'prog-mode-hook #'show-paren-mode)

;; dired
(autoload 'dired-hide-details-mode "dired")
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(custom-set-variables
 '(dired-listing-switches "-l --group-directories-first")
 '(dired-hide-details-hide-symlink-targets nil))

;; autorevert
(fm/diminish 'autorevert-mode "Ar")

(custom-set-variables
 '(auto-revert-interval 1)
 '(auto-revert-mode-text " Ar"))

(add-hook 'dired-mode-hook #'auto-revert-mode)

;; subword
(fm/diminish 'subword-mode "Sw")

;; spell
(custom-set-variables
 '(ispell-program-name "aspell")
 '(ispell-extra-args '("--sug-mode=ultra")))

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; org-bullets
(custom-set-variables
 '(org-bullets-bullet-list '("●" "○")))

(add-hook 'org-mode-hook #'org-bullets-mode)

;; org
(custom-set-variables
 '(org-cycle-separator-lines 0)
 '(org-startup-folded nil)
 '(org-ellipsis "   ▾"))

(autoload 'org-indent-mode "org")
(add-hook 'org-mode #'org-indent-mode)

(custom-set-faces
 '(org-ellipsis ((t (:underline nil :foreground "DarkGoldenRod"))))
 '(org-level-1 ((t (:height 1.3 :inherit (outline-1)))))
 '(org-level-2 ((t (:height 1.2 :inherit (outline-2)))))
 '(org-level-3 ((t (:height 1.1 :inherit (outline-3)))))
 '(org-todo ((t (:foreground "Red1" :height 0.9))))
 '(org-done ((t (:foreground "ForestGreen" :height 0.9)))))

;; which-key
(fm/diminish 'which-key-mode)
(which-key-mode)

(custom-set-variables
 '(which-key-idle-delay 0.3))

;; counsel
(fm/diminish 'counsel-mode)
(counsel-mode t)
(put 'counsel-find-symbol 'no-counsel-M-x t)

;; swiper
(global-set-key (kbd "C-s")         #'swiper-isearch)
(global-set-key (kbd "C-c C-c C-s") #'swiper-all)
(global-set-key (kbd "C-c C-s")     #'swiper-thing-at-point)
(global-set-key (kbd "C-r")         #'swiper-isearch-backward)

;; ivy
(fm/diminish 'ivy-mode)
(fm/key "ivy" ivy-minibuffer-map "<RET>" ivy-alt-done)

(custom-set-variables
 '(ivy-re-builders-alist '((t . ivy--regex-ignore-order) (t . ivy--regex-plus)))
 '(ivy-wrap t)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-virtual-abbreviate 'full)
 '(ivy-initial-inputs-alist nil)
 '(ivy-extra-directories nil))

(ivy-mode)

;; ivy-rich
(ivy-rich-mode)

;; fzf
(global-set-key (kbd "M-F") #'fzf-git-files)

;; deadgrep
(global-set-key (kbd "M-G") #'deadgrep)

;; mwim
(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)

;; transient - magit-related
(custom-set-variables
 '(transient-default-level 7))

;; magit
(global-set-key (kbd "C-x g") #'magit-status)
(add-hook 'after-save-hook #'magit-after-save-refresh-status)
(autoload 'magit-after-save-refresh-status "magit")

(custom-set-variables
 '(magit-auto-revert-tracked-only nil)
 '(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
 '(magit-repository-directories '(("~/Workspace" . 3)
                                  ("~/Oracle" . 3)
                                  ("~/OracleWorkTrees" . 3))))

;; expand-region
(global-set-key (kbd "C-=") #'er/expand-region)

;; esup
(autoload 'esup "esup")

;; js
(push '("\\.hocon\\'" . js-mode) auto-mode-alist)

;; llvm-mode
(push '("\\.ll\\'" . llvm-mode) auto-mode-alist)
(add-hook 'llvm-mode-hook (lambda () (toggle-truncate-lines t)))

;; indent-guide
(add-hook 'json-mode-hook #'indent-guide-mode)

(custom-set-faces
 '(indent-guide-face ((t (:foreground "gray80")))))

;; projectile
(fm/diminish 'projectile-mode "Prj")
(eval-when-compile (defvar projectile-mode-map))
(autoload 'projectile-command-map "projectile")
(add-hook 'projectile-mode-hook
 (lambda ()
  (define-key projectile-mode-map (kbd "C-x p") #'projectile-command-map)))
(projectile-mode)

(custom-set-variables
 '(projectile-project-search-path '("~/Workspace"))
 '(projectile-sort-order '(recently-active))
 '(projectile-enable-caching t)
 '(projectile-completion-system 'ivy))

;; counsel-projectile
(counsel-projectile-mode)

;; yasnippet
(fm/diminish 'yasnippet-mode "Ys")
(add-hook 'rustic-mode-hook #'yas-minor-mode)

;; diff-hl
(custom-set-variables
 '(diff-hl-draw-borders nil)
 '(diff-hl-flydiff-delay 0.1))

(autoload 'diff-hl-magit-post-refresh "diff-hl")
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
(add-hook 'prog-mode-hook #'diff-hl-mode)

(custom-set-faces
 '(diff-hl-delete ((t (:background "RosyBrown1"))))
 '(diff-hl-insert ((t (:background "DarkSeaGreen2"))))
 '(diff-hl-change ((t (:background "PowderBlue")))))

;; symbol-overlay
(fm/diminish 'symbol-overlay-mode "Sy")
(eval-when-compile (defvar symbol-overlay-mode-map))
(add-hook 'symbol-overlay-mode-hook
 (lambda ()
  (define-key symbol-overlay-mode-map (kbd "M->") #'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-<") #'symbol-overlay-jump-prev)))

(custom-set-variables
 '(symbol-overlay-idle-time 0.1))

(custom-set-faces
 '(symbol-overlay-default-face ((t (:background "HoneyDew2")))))

(add-hook 'shell-script-mode-hook #'symbol-overlay-mode)
(add-hook 'hledger-mode-hook      #'symbol-overlay-mode)
(add-hook 'emacs-lisp-mode-hook   #'symbol-overlay-mode)

;; multiple-cursors
(global-set-key (kbd "C-c C-v")       #'mc/edit-lines)
(global-set-key (kbd "C->")           #'mc/mark-next-like-this)
(global-set-key (kbd "C-<")           #'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click)

(custom-set-variables
 '(mc/always-run-for-all t))

(custom-set-faces
 '(mc/cursor-bar-face ((t (:background "Gray40" :foreground "White"))))
 '(mc/cursor-face ((t (:background "Gray50" :foreground "White")))))

;; hledger-mode
(push '("\\.journal\\'" . hledger-mode) auto-mode-alist)
(push '("\\.ledger\\'"  . hledger-mode) auto-mode-alist)

(custom-set-variables
 '(hledger-currency-string "EUR")
 '(hledger-current-overlay t)
 '(hledger-comments-column 1))

(add-hook 'hledger-mode-hook
 (lambda ()
  (toggle-truncate-lines t)
  (setq tab-width 1)))

;; flycheck
(eval-when-compile (defvar flycheck-mode-map))
(autoload 'flycheck-next-error     "flycheck")
(autoload 'flycheck-previous-error "flycheck")
(add-hook 'flycheck-mode-hook
 (lambda ()
  (define-key flycheck-mode-map (kbd "M-n") #'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") #'flycheck-previous-error)))

(custom-set-variables
 '(flycheck-checker-error-threshold nil)
 '(flycheck-mode-line-prefix "Fc")
 '(flycheck-check-syntax-automatically '(idle-change new-line
                                         mode-enabled idle-buffer-switch))
 '(flycheck-idle-change-delay 0.1)
 '(flycheck-idle-buffer-switch-delay 0.1))

(add-hook 'prog-mode-hook #'flycheck-mode)

(custom-set-faces
 '(flycheck-error   ((t (:underline "Red1"))))
 '(flycheck-warning ((t (:underline "DarkOrange"))))
 '(flycheck-info    ((t (:underline "ForestGreen")))))

;; flycheck-posframe
(add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)

(custom-set-variables
 '(flycheck-posframe-position 'window-bottom-right-corner)
 '(flycheck-posframe-border-width 1)
 '(flycheck-posframe-warnings-prefix "Warning: ")
 '(flycheck-posframe-error-prefix "Error: ")
 '(flycheck-posframe-prefix "Info: "))

(custom-set-faces
 '(flycheck-posframe-background-face ((t (:background "CornSilk"))))
 '(flycheck-posframe-border-face     ((t (:background "Wheat" :foreground "Wheat"))))
 '(flycheck-posframe-error-face      ((t (:foreground "DarkRed"))))
 '(flycheck-posframe-warning-face    ((t (:foreground "DarkOrange")))))

;; company
(fm/diminish 'company-mode "Co")

(custom-set-variables
 '(company-frontends '(company-echo-metadata-frontend company-pseudo-tooltip-frontend))
 '(completion-ignore-case t)
 '(company-echo-truncate-lines nil)
 '(company-selection-wrap-around t)
 '(company-tooltip-minimum 10)
 '(company-tooltip-limit 20)
 '(company-tooltip-align-annotations t)
 '(company-idle-delay 0.1)
 '(company-occurence-weight-function 'company-occurrence-prefer-any-closest)
 '(company-transformers
   '(company-sort-by-occurrence
     company-sort-by-backend-importance
     company-sort-prefer-same-case-prefix)))

(add-hook 'prog-mode-hook    #'company-mode)
(add-hook 'systemd-mode-hook #'company-mode)

(eval-when-compile (defvar company-backends))
(add-hook 'company-mode-hook
 (lambda () (setq company-backends '((company-capf company-keywords company-files)))))

(custom-set-faces
 '(company-tooltip ((t (:background "gray95")))))

;; company-posframe
(fm/diminish 'company-posframe-mode)

(autoload 'company-posframe-mode "company-posframe")
(add-hook 'company-mode-hook #'company-posframe-mode)

(custom-set-variables
 '(company-posframe-show-params
   (list :internal-border-width 1 :internal-border-color "gray60")))

;; rustic
(autoload 'rust-dbg-wrap-or-unwrap "rust-mode")
(autoload 'lsp-rust-analyzer-expand-macro "lsp-mode")
(autoload 'lsp-rust-analyzer-join-lines "lsp-mode")
(autoload 'lsp-rust-analyzer-inlay-hints-mode "lsp-mode")

(eval-after-load 'rustic-mode
 '(progn
   (eval-when-compile (defvar rustic-mode-map))
   (define-key rustic-mode-map (kbd "<f5>") #'rust-dbg-wrap-or-unwrap)
   (define-key rustic-mode-map (kbd "<f6>") #'lsp-rust-analyzer-expand-macro)
   (define-key rustic-mode-map (kbd "<f7>") #'lsp-rust-analyzer-join-lines)
   (define-key rustic-mode-map (kbd "<f8>") #'lsp-rust-analyzer-inlay-hints-mode)))

(custom-set-variables
 '(rustic-lsp-server 'rust-analyzer)
 '(rustic-analyzer-command '("/usr/bin/rust-analyzer"))
 '(rustic-lsp-format t)
 '(rustic-indent-offset 2)
 '(rustic-always-locate-project-on-open t))

(add-hook 'rustic-mode-hook
 (lambda ()
  (electric-quote-local-mode -1)
  (add-hook 'before-save-hook #'lsp-format-buffer 10 t)))
(add-hook 'rustic-mode-hook #'subword-mode)

;; lsp-mode
(autoload 'lsp-format-buffer "lsp-mode")
(autoload 'lsp-rename "lsp-mode")
(autoload 'lsp-describe-thing-at-point "lsp-mode")
(autoload 'lsp-extend-selection "lsp-mode")
(autoload 'lsp-execute-code-action "lsp-mode")
(autoload 'lsp-enable-which-key-integration "lsp-mode")
(autoload 'lsp-headerline-breadcrumb-mode "lsp-mode")
(autoload 'lsp-rust-analyzer-initialized? "lsp-mode")
(autoload 'lsp-request-async "lsp-mode")
(autoload 'lsp--text-document-identifier "lsp-mode")
(autoload '-let* "lsp-mode")
(autoload 'lsp--range-to-region "lsp-mode")
(autoload 'overlay "lsp-mode")
(autoload 'lsp-ui-peek-find-definitions "lsp-mode")
(autoload 'lsp-ui-peek-find-references "lsp-mode")
(autoload 'lsp-ui-doc-glance "lsp-mode")
(autoload 'lsp-flycheck-enable "lsp-mode")

(defun fm/lsp-ivy-helper ()
 "Call LSP-IVY-WORKSPACE-SYMBOL with symbol at point."
 (interactive)
 (lsp-ivy-workspace-symbol t))

(eval-after-load 'lsp-mode
 '(progn
   (eval-when-compile (defvar lsp-mode-map))
   (define-key lsp-mode-map (kbd "C-c f") #'lsp-format-buffer)
   (define-key lsp-mode-map (kbd "C-c r") #'lsp-rename)
   (define-key lsp-mode-map (kbd "C-c t") #'lsp-describe-thing-at-point)
   (define-key lsp-mode-map (kbd "C-=")   #'lsp-extend-selection)
   (define-key lsp-mode-map (kbd "C-c e") #'lsp-rust-analyzer-expand-macro)
   (define-key lsp-mode-map (kbd "M-RET") #'lsp-execute-code-action)
   (define-key lsp-mode-map (kbd "C-c x") #'fm/lsp-ivy-helper)))

(custom-set-variables
 '(lsp-enable-snippet t)
 '(lsp-keymap-prefix "C-c")
 '(lsp-prefer-flymake nil)
 '(lsp-prefer-capf t)
 '(lsp-file-watch-threshold nil)
 '(lsp-enable-semantic-highlighting t)
 '(lsp-enable-indentation t)
 '(lsp-enable-on-type-formatting t)
 '(lsp-before-save-edits t)
 '(lsp-auto-configure t)

 '(lsp-rust-racer-completion nil)
 '(lsp-rust-build-bin t)
 '(lsp-rust-build-lib t)
 '(lsp-rust-clippy-preference "on")
 '(lsp-rust-server 'rust-analyzer)
 '(lsp-rust-analyzer-server-display-inlay-hints t)
 '(lsp-rust-analyzer-display-chaining-hints t)
 '(lsp-rust-analyzer-display-parameter-hints t)
 '(lsp-rust-all-features t)
 '(lsp-rust-all-targets t)
 '(lsp-rust-build-on-save t)
 ;; '(lsp-rust-full-docs t)
 ;; '(lsp-rust-analyzer-max-inlay-hint-length 10)
 ;; '(lsp-signature-doc-lines 1)
 ;; '(lsp-signature-auto-activate t)
 ;; '(lsp-signature-render-documentation t)
 '(lsp-diagnostics-attributes `((unnecessary :background "Gray90")
                                (deprecated  :strike-through t))))

(add-hook 'lsp-mode-hook #'lsp-flycheck-enable)
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)

(defface fm/lsp-rust-inlay-type-face
 '((t :background "OldLace" :foreground "DarkGray"))
 "Face for inlay type hints (e.g. inferred types)."
 :group 'lsp-rust)

(defface fm/lsp-rust-inlay-param-face
 '((t :background "Azure" :foreground "DarkGray"))
 "Face for inlay parameter hints (e.g. function parameter names at call-site)."
 :group 'lsp-rust)

;; (eval-after-load 'lsp-rust
;;  '(eval-after-load 'lsp-mode
;;    '(defun lsp-rust-analyzer-update-inlay-hints (buffer)
;;      (if (and (lsp-rust-analyzer-initialized?)
;;           (eq buffer (current-buffer)))
;;       (lsp-request-async
;;        "rust-analyzer/inlayHints"
;;        (list :textDocument (lsp--text-document-identifier))
;;        (lambda (res)
;;         (remove-overlays (point-min) (point-max) 'lsp-rust-analyzer-inlay-hint t)
;;         (dolist (hint res)
;;          (-let* (((&hash "range" "label" "kind") hint)
;;                  ((beg . end) (lsp--range-to-region range))
;;                  (overlay (make-overlay beg end)))
;;           (overlay-put overlay 'lsp-rust-analyzer-inlay-hint t)
;;           (overlay-put overlay 'evaporate t)
;;           (cond
;;            ((string= kind "TypeHint")
;;             (overlay-put overlay
;;              'after-string
;;              (concat (propertize ": " 'font-lock-face
;;                       '(:foreground "darkgray"))
;;               (propertize label 'font-lock-face
;;                'fm/lsp-rust-inlay-type-face))))
;;            ((string= kind "ParameterHint")
;;             (overlay-put overlay
;;              'before-string
;;              (concat (propertize label 'font-lock-face
;;                       'fm/lsp-rust-inlay-param-face)
;;               (propertize ": " 'font-lock-face
;;                '(:foreground "darkgray")))))))))
;;        :mode 'tick))
;;      nil)))

(custom-set-faces
 '(lsp-lens-face ((t (:inherit shadow))))
 '(lsp-lens-mouse-face ((t (:inherit link)))))

;; lsp-ui
(eval-after-load 'lsp-ui
 '(eval-after-load 'lsp-mode
   '(progn
     (eval-when-compile (defvar lsp-mode-map))
     (define-key lsp-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
     (define-key lsp-mode-map (kbd "M-?") #'lsp-ui-peek-find-references)
     (define-key lsp-mode-map (kbd "C-c h") #'lsp-ui-doc-glance))))

(custom-set-variables
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-flycheck-list-mode t)

 ;; '(lsp-ui-peek-always-show t)
 ;; '(lsp-ui-peek-show-directory nil)

 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-border "black")
 '(lsp-ui-doc-alignment 'window)
 ;; '(lsp-ui-doc-delay 0.1)
 ;; '(lsp-ui-doc-header t)
 ;; '(lsp-ui-doc-include-signature t)

 ;; '(lsp-ui-sideline-delay 0.1)
 ;; '(lsp-ui-sideline-update-mode 'line)
 ;; '(lsp-ui-sideline-ignore-duplicate t)
 ;; '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-enable nil))

(custom-set-faces
 ;; '(lsp-ui-sideline-code-action ((t (:foreground "Sienna"))))
 ;; '(lsp-ui-sideline-global ((t (:foreground "Gray70"))))
 ;; '(lsp-ui-sideline-symbol-info ((t (:foreground "Gray70" :slant italic))))
 ;; '(lsp-ui-sideline-current-symbol ((t (:foreground "White" :background "Gray75"))))
 ;; '(lsp-ui-sideline-symbol ((t (:foreground "White" :background "Gray75"))))
 '(lsp-ui-doc-background ((t (:background "Gray95"))))
 '(lsp-ui-doc-header ((t (:background "Pale Turquoise"))))
 '(lsp-ui-doc-border ((t (:background "Gray70")))))

;; lsp-ivy
(autoload 'lsp-ivy-workspace-symbol "lsp-ivy")

(setq file-name-handler-alist nil)
;; (message "Startup in %s" (emacs-init-time))

(provide 'init)
;;; init ends here

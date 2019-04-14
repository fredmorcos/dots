;;; .emacs --- Emacs configuration
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t; -*-

;; (call-interactively 'profiler-start)

;; (defvar old-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

(run-with-idle-timer 5 t #'garbage-collect)

(setq vc-handled-backends nil)

(defvar old-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defvar package-thread
  (make-thread
   (lambda ()
     (progn
       (package-initialize)

       ;; ivy, counsel, etc...
       (add-hook
        'ivy-mode-hook
        '(lambda ()
           (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
           (ivy-set-display-transformer 'ivy-switch-buffer
                                        'ivy-rich-switch-buffer-transformer)))

       (global-set-key (kbd "C-s")     #'swiper)
       (global-set-key (kbd "C-r")     #'swiper)
       (global-set-key (kbd "M-x")     #'counsel-M-x)
       (global-set-key (kbd "C-x C-f") #'counsel-find-file)

       (setq-default ivy-initial-inputs-alist nil)

       ;; fuzzy find files
       (global-set-key (kbd "M-p") #'fzf-git-files)
       (global-set-key (kbd "M-P") #'fzf-git-grep)

       ;; emacs lisp
       (push '("\\emacs\\'" . emacs-lisp-mode) auto-mode-alist)
       (add-hook 'emacs-lisp-mode-hook #'checkdoc-minor-mode)
       (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
       (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
       (add-hook 'emacs-lisp-mode-hook #'company-mode)

       ;; symbol-overlay
       (add-hook 'prog-mode-hook #'symbol-overlay-mode)
       (global-set-key (kbd "M->") #'symbol-overlay-jump-next)
       (global-set-key (kbd "M-<") #'symbol-overlay-jump-prev)

       ;; magit
       (global-set-key (kbd "C-x g") 'magit-status)))))

(defvar background-thread
  (make-thread
   (lambda ()
     (progn
       (custom-set-faces
        '(default ((t (:family "Hack" :height 110))))
        '(region ((t (:background "lightsteelblue1"))))
        '(mode-line ((t (:box (:line-width -1 :color "grey75" :style nil)
                              :foreground "gray20"
                              :background "gray80"))))
        '(mode-line-highlight ((t (:box (:line-width 1 :color "grey40" :style nil)))))
        '(symbol-overlay-default-face ((t (:background "honeydew2"))))
        '(line-number ((t (:foreground "grey80"))))
        '(line-number-current-line ((t (:foreground "grey60" :background "cornsilk"))))
        '(hl-line ((t (:background "cornsilk"))))
        '(show-paren-match ((t (:background "powder blue"))))
        '(show-paren-match-expression ((t (:background "powder blue"))))
        '(show-paren-mismatch ((t (:background "light salmon"))))
        '(git-gutter+-added ((t (:foreground "yellow green"))))
        '(dired-subtree-depth-1-face ((t (:background "light blue"))))
        '(dired-subtree-depth-2-face ((t (:background "light green"))))
        '(dired-subtree-depth-3-face ((t (:background "light yellow"))))
        '(dired-subtree-depth-4-face ((t (:background "light blue"))))
        '(dired-subtree-depth-5-face ((t (:background "light green"))))
        '(dired-subtree-depth-6-face ((t (:background "light yellow"))))
        '(rust-question-mark-face ((t (:inherit (font-lock-builtin-face)))))
        '(lsp-ui-doc-background ((t (:background "white smoke"))))
        '(lsp-ui-sideline-code-action ((t (:foreground "orange"))))
        '(lsp-ui-sideline-current-symbol
          ((t (:height 0.99 :weight ultra-bold :box
                       (:line-width -1 :color "dim gray" :style nil)
                       :foreground "dim gray")))))

       (defalias 'yes-or-no-p 'y-or-n-p)
       (windmove-default-keybindings)
       (cua-selection-mode 1)
       (global-hl-line-mode t)
       (add-hook 'before-save-hook #'delete-trailing-whitespace)
       (global-set-key (kbd "C-z") #'bury-buffer)
       ;; (global-auto-revert-mode t)
       (electric-pair-mode)
       (electric-indent-mode)
       ;; (electric-quote-mode)
       (electric-layout-mode)
       (global-display-line-numbers-mode t)

       ;; file positions, backups, etc...
       (defconst emacs-temp-dir (concat temporary-file-directory "emacs/"))
       (defconst emacs-autosaves-dir (concat emacs-temp-dir "autosaves"))
       (defconst emacs-backups-dir (concat emacs-temp-dir "backups"))
       (defconst emacs-places-file (concat user-emacs-directory "places"))
       (defconst emacs-recentf-file (concat user-emacs-directory "recentf"))
       (defconst emacs-autosaves-pattern (concat emacs-autosaves-dir "/\\1"))
       (defconst emacs-backups-pattern (concat emacs-backups-dir "/"))
       (make-directory emacs-autosaves-dir t)
       (make-directory emacs-backups-dir t)

       ;; dired
       (add-hook 'dired-mode-hook #'auto-revert-mode)
       (add-hook 'dired-mode-hook  'dired-hide-details-mode)
       (add-hook
        'dired-mode-hook
        #'(lambda () (local-set-key (kbd "TAB") #'dired-subtree-toggle)))

       ;; flyspell
       (add-hook 'flyspell-mode-hook #'flyspell-buffer)

       ;; org mode
       (add-hook 'org-mode-hook  'org-indent-mode)
       (add-hook 'org-mode-hook  'org-bullets-mode)
       (add-hook 'org-mode-hook #'flyspell-buffer)
       (add-hook
        'org-mode-hook
        #'(lambda ()
            (setq-local fill-column 70)
            (add-hook 'after-save-hook #'flyspell-buffer nil t)))

       ;; latex
       (when (string-equal (system-name) "axon")
         (load "auctex.el")
         (load "preview-latex.el"))
       (add-hook 'LaTeX-mode-hook #'flyspell-mode)

       ;; PET
       (defconst pet-mode-file "~/Workspace/dots/emacs/pet-mode.el")
       (when (file-exists-p pet-mode-file)
         (autoload 'pet-mode pet-mode-file)
         (push '("\\.pet\\'" . pet-mode) auto-mode-alist))

       ;; pkgbuilds
       (autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
       (push '("/PKGBUILD$" . pkgbuild-mode) auto-mode-alist)))))

(thread-join package-thread)

;; company
(require 'company)
(add-hook 'company-mode-hook #'company-box-mode)

;; vdiff
(require 'vdiff)
(define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)

;; iedit
(require 'iedit)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-c C-v")       #'mc/edit-lines)
(global-set-key (kbd "C->")           #'mc/mark-next-like-this)
(global-set-key (kbd "C-<")           #'mc/mark-previous-like-this)
(global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click)

;; yasnippet
(require 'yasnippet)
(defvar yasnippet-thread
  (make-thread
   (lambda ()
     (progn
       (push "~/Workspace/dots/emacs/snippets" yas-snippet-dirs)
       (yas-reload-all)))))

;; git status
(require 'git-gutter+)
(global-git-gutter+-mode)
(add-hook 'magit-refresh-status-hook #'git-gutter+-on-magit-refresh-status)

;; lsp
(require 'lsp)
(require 'lsp-clients)
(require 'lsp-ui)

(define-key lsp-ui-mode-map
  [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map
  [remap xref-find-references] #'lsp-ui-peek-find-references)

;; c/c++
(add-hook 'c++-mode-hook   #'irony-mode)
(add-hook 'c++-mode-hook   #'flycheck-mode)
(add-hook 'c++-mode-hook   #'company-mode)
(add-hook 'c-mode-hook     #'irony-mode)
(add-hook 'c-mode-hook     #'flycheck-mode)
(add-hook 'c-mode-hook     #'company-mode)
(add-hook
 'c-mode-hook
 #'(lambda ()
     (setq-default flycheck-cppcheck-standards '("c11" "posix"))))
(add-hook 'irony-mode-hook #'irony-eldoc)
(add-hook 'irony-mode-hook #'flycheck-irony-setup)
(add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)
(add-hook
 'irony-mode-hook
 #'(lambda ()
     (add-to-list 'company-backends #'company-irony)
     (add-to-list 'company-backends #'company-irony-c-headers)
     (setq-default flycheck-cppcheck-checks '("all"))
     (setq-default flycheck-cppcheck-suppressions '("missingIncludeSystem"))))
(with-eval-after-load 'flycheck
  (with-eval-after-load 'flycheck-irony
    (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)
    (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
    (flycheck-add-next-checker 'irony '(t . c/c++-cppcheck))))

;; hledger
(require 'hledger-mode)
(push '("\\.journal\\'" . hledger-mode) auto-mode-alist)
(push '("\\.ledger\\'" . hledger-mode) auto-mode-alist)
(add-to-list 'company-backends #'hledger-company)
(add-hook 'hledger-mode-hook #'company-mode)

(defun hledger/next-entry ()
  "Move to next entry and pulse."
  (interactive)
  (hledger-next-or-new-entry)
  (hledger-pulse-momentary-current-entry))

(defun hledger/prev-entry ()
  "Move to last entry and pulse."
  (interactive)
  (hledger-backward-entry)
  (hledger-pulse-momentary-current-entry))

(add-hook
 'hledger-mode-hook
 #'(lambda ()
     (define-key hledger-mode-map (kbd "M-p")   #'hledger/prev-entry)
     (define-key hledger-mode-map (kbd "M-n")   #'hledger/next-entry)
     (define-key hledger-mode-map (kbd "C-c j") #'hledger-run-command)
     (define-key hledger-mode-map (kbd "C-c e") #'hledger-jentry)))

(add-hook
 'hledger-mode-hook
 #'(lambda () (toggle-truncate-lines t)))

;; rust
(add-hook 'rust-mode-hook #'lsp)

;; java
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

;; yasnippet
(thread-join yasnippet-thread)
(add-hook 'hledger-mode-hook #'yas-minor-mode)
(add-hook 'rust-mode-hook #'yas-minor-mode)

;; customizations
(custom-set-variables
 '(custom-file "~/Workspace/dots/emacs/custom.el")

 '(url-proxy-services '(("no_proxy" . "127.0.0.1")))
 '(url-privacy-level 'high)

 '(package-check-signature nil)
 '(package-archives
   '(("gnu"   . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
 '(package-selected-packages
   '(use-package
     diminish
     bind-key

     which-key
     fzf
     flx
     avy
     ivy
     ivy-rich
     swiper
     counsel
     amx
     smex
     org-bullets
     git-gutter+
     symbol-overlay
     multiple-cursors
     iedit

     magit
     vdiff
     vdiff-magit

     yasnippet
     yasnippet-snippets

     hledger-mode

     dired-subtree

     f
     ht
     flycheck
     company
     company-lsp
     company-tabnine
     company-box
     lsp-mode
     lsp-ui
     rmsbolt

     toml-mode
     markdown-mode
     json-mode
     gnuplot-mode
     dockerfile-mode
     meson-mode

     yaml-mode
     flycheck-yamllint

     rust-mode

     lsp-java

     irony
     irony-eldoc
     flycheck-irony
     company-irony
     company-irony-c-headers
     cquery
     ccls

     z3-mode
     boogie-friends
     ))

 '(frame-resize-pixelwise t)
 '(font-use-system-font t)
 '(blink-cursor-mode nil)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(inhibit-startup-buffer-menu t)
 '(initial-scratch-message nil)
 '(initial-major-mode 'fundamental-mode)
 '(frame-title-format "%b - emacs")
 '(help-window-select t)
 '(uniquify-buffer-name-style 'forward)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 4)
 '(hscroll-margin 1)
 '(hscroll-step 1)
 '(auto-hscroll-mode 'current-line)
 '(size-indication-mode t)
 '(show-paren-mode t)

 '(load-prefer-newer t)
 '(savehist-mode t)
 '(save-place t)
 '(save-place-mode t)
 '(save-place-file emacs-places-file)
 '(recentf-mode t)
 '(recentf-save-file emacs-recentf-file)
 '(auto-save-mode t)
 '(auto-save-file-name-transforms `((".*" ,emacs-autosaves-pattern t)))
 '(backup-directory-alist `((".*" . ,emacs-backups-pattern)))
 '(backup-inhibited nil)
 '(history-delete-duplicates t)
 '(history-length 30)
 '(delete-old-versions t)
 '(make-backup-files t)
 '(vc-make-backup-files t)

 '(coding-system-for-read 'utf-8-unix)
 '(coding-system-for-write 'utf-8-unix)

 '(indent-tabs-mode nil)
 '(standard-indent 2)
 '(tab-width 2)

 '(fill-column 70)
 '(comment-fill-column 70)
 '(colon-double-space t)
 '(default-justification 'left)

 '(mode-require-final-newline 'visit-save)
 '(require-final-newline 'visit-save)
 '(show-trailing-whitespace nil)
 '(whitespace-action '(cleanup))
 '(whitespace-style
   '(face
     tabs
     lines-tail
     trailing
     space-before-tab
     indentation
     empty
     space-after-tab))

 '(ivy-mode t)
 '(ivy-rich-mode t)
 '(counsel-mode t)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(ivy-display-style 'fancy)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-wrap t)
 '(ivy-regex-ignore-order t)
 '(ivy--regex-ignore-order t)
 '(ivy-virtual-abbreviate 'full)
 '(ivy-rich-switch-buffer-align-virtual-buffer t)
 '(ivy-rich-path-style 'abbrev)
 '(ivy-action-wrap t)

 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(TeX-master nil)

 '(which-key-mode t)

 '(symbol-overlay-idle-time 0.1)

 '(hledger-comments-column 2)
 '(hledger-currency-string "EUR")
 '(hledger-jfile (expand-file-name "~/Expenses/Expenses.ledger"))
 '(hledger-year-of-birth 1987)

 '(org-cycle-separator-lines 0)
 '(org-indent-indentation-per-level 2)
 '(org-startup-folded t)

 '(display-line-numbers-grow-only t)
 '(display-line-numbers-width-start t)

 '(rust-indent-method-chain t)
 '(rust-always-locate-project-on-open t)
 '(rust-indent-where-clause t)
 '(rust-format-on-save t)

 '(lsp-prefer-flymake nil)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-border "light salmon")
 '(lsp-ui-doc-position 'at-point)
 '(lsp-ui-doc-max-width 60)
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-update-mode 'point)

 '(eldoc-echo-area-use-multiline-p t)

 '(sh-indentation 2)
 '(sh-basic-offset 2)

 '(company-auto-complete ''company-explicit-action-p)
 '(company-auto-complete-chars '(32 95 41 46))
 '(company-echo-truncate-lines nil)
 '(company-selection-wrap-around t)
 '(company-tooltip-limit 100)
 '(company-tooltip-minimum 10)
 '(company-tooltip-align-annotations t)
 '(company-idle-delay 0.2)
 '(company-transformers '(company-sort-by-backend-importance))

 '(ediff-split-window-function #'split-window-horizontally)
 '(ediff-window-setup-function #'ediff-setup-windows-plain)
 '(ediff-window-setup-function #'ediff-setup-windows-plain)

 '(dired-listing-switches "-l --group-directories-first")
 '(dired-hide-details-hide-symlink-targets nil))

(setq file-name-handler-alist old-file-name-handler-alist)

;; (setq gc-cons-threshold old-gc-cons-threshold)

;; (call-interactively 'profiler-report)

(provide '.emacs)
;;; .emacs ends here

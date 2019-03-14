;;; .emacs --- Emacs configuration
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t; -*-

(package-initialize)
(set-face-attribute 'default nil :height 120)
(defalias 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(cua-selection-mode 1)
(global-hl-line-mode t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

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

;; file positions, backups, etc...
(require 'saveplace)
(defconst emacs-temp-dir (concat temporary-file-directory "emacs/"))
(defconst emacs-autosaves-dir (concat emacs-temp-dir "autosaves"))
(defconst emacs-backups-dir (concat emacs-temp-dir "backups"))
(defconst emacs-places-file (concat user-emacs-directory "places"))
(defconst emacs-recentf-file (concat user-emacs-directory "recentf"))
(defconst emacs-autosaves-pattern (concat emacs-autosaves-dir "/\\1"))
(defconst emacs-backups-pattern (concat emacs-backups-dir "/"))
(make-directory emacs-autosaves-dir t)
(make-directory emacs-backups-dir t)

;; yasnippet
(require 'yasnippet)
(push "~/Workspace/dots/emacs/snippets" yas-snippet-dirs)
(yas-reload-all)

;; latex
(load "auctex.el")
(load "preview-latex.el")
(add-hook 'LaTeX-mode-hook #'flyspell-mode)

;; PET
(defconst pet-mode-file "~/Workspace/dots/emacs/pet-mode.el")
(when (file-exists-p pet-mode-file)
  (autoload 'pet-mode pet-mode-file)
  (push '("\\.pet\\'" . pet-mode) auto-mode-alist))

;; company
(require 'company)
(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

;; git status
(require 'git-gutter-fringe+)
(global-git-gutter+-mode)

;; auto-reload
(global-auto-revert-mode)

;; linum
(global-linum-mode)

;; hledger
(require 'hledger-mode)
(push '("\\.journal\\'" . hledger-mode) auto-mode-alist)
(push '("\\.ledger\\'" . hledger-mode) auto-mode-alist)
(add-to-list 'company-backends #'hledger-company)
(add-hook 'hledger-mode-hook #'company-mode)
(add-hook 'hledger-mode-hook #'yas-minor-mode)

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

;; pkgbuilds
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(push '("/PKGBUILD$" . pkgbuild-mode) auto-mode-alist)

;; dired
(add-hook 'dired-mode-hook #'auto-revert-mode)
(add-hook 'dired-mode-hook  'dired-hide-details-mode)

;; emacs lisp
(push '("\\emacs\\'" . emacs-lisp-mode) auto-mode-alist)

(add-hook 'emacs-lisp-mode-hook #'checkdoc-minor-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'company-mode)

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

;; lsp
(require 'lsp)
(require 'lsp-clients)
(require 'lsp-ui)

(define-key lsp-ui-mode-map
  [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map
  [remap xref-find-references] #'lsp-ui-peek-find-references)

;; rust
(add-hook 'rust-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'yas-minor-mode)

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
   '(which-key
     fzf
     flx
     avy
     ivy
     ivy-rich
     swiper
     counsel
     smex
     org-bullets

     git-gutter-fringe+
     yasnippet
     yasnippet-snippets

     hledger-mode

     f
     ht
     flycheck
     company
     company-lsp
     company-tabnine
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

     irony
     irony-eldoc
     flycheck-irony
     company-irony
     company-irony-c-headers
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
 ;; '(show-paren-delay 0)
 ;; '(echo-keystrokes 0.1)

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

 ;; '(flyspell-delay 0.2)

 '(hledger-comments-column 2)
 '(hledger-currency-string "EUR")
 '(hledger-jfile (expand-file-name "~/Expenses/Expenses.ledger"))
 '(hledger-year-of-birth 1987)

 '(org-cycle-separator-lines 0)
 '(org-indent-indentation-per-level 2)
 '(org-startup-folded t)

 '(linum-format "%4d ")

 ;; '(rust-indent-offset 2)
 '(rust-indent-method-chain t)
 '(rust-always-locate-project-on-open t)
 '(rust-indent-where-clause t)
 '(rust-format-on-save t)

 '(lsp-prefer-flymake nil)
 ;; '(lsp-auto-guess-root t)

 ;; '(lsp-ui-doc-position 'at-point)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-border "orange red")
 ;; '(lsp-ui-flycheck-list-position 'bottom)

 ;; '(lsp-ui-sideline-enable nil)
 ;; '(lsp-ui-doc-enable nil)
 ;; '(lsp-ui-doc-border "orange red")
 ;; '(lsp-ui-doc-include-signature t)
 ;; '(lsp-ui-doc-position 'at-point)
 ;; '(lsp-ui-flycheck-list-position 'right)

 '(eldoc-echo-area-use-multiline-p t)
 ;; '(eldoc-idle-delay 2)

 '(sh-indentation 2)
 '(sh-basic-offset 2)

 '(company-tabnine-binaries-folder "~/.emacs.d/tabnine")

 '(company-tooltip-align-annotations t)
 ;; '(company-minimum-prefix-length 1)
 ;; '(company-idle-delay 0)
 '(company-show-numbers t)
 '(company-frontends
   '(company-tng-frontend
     company-pseudo-tooltip-unless-just-one-frontend
     company-echo-metadata-frontend
     company-preview-if-just-one-frontend))
 ;; '(company-lsp-enable-recompletion t)

 ;; '(flycheck-display-errors-delay 0.3)

 '(ediff-split-window-function #'split-window-horizontally)
 '(ediff-window-setup-function #'ediff-setup-windows-plain))

(custom-set-faces
 '(mode-line ((t (:box (:line-width -1 :color "grey75" :style nil)
                       :foreground "gray20"
                       :background "gray80"))))
 '(mode-line-highlight ((t (:box (:line-width 1 :color "grey40" :style nil)))))
 '(linum ((t (:foreground "grey80"))))
 '(hl-line ((t (:background "cornsilk"))))
 '(rust-question-mark-face ((t (:inherit (font-lock-builtin-face)))))
 '(lsp-ui-doc-background ((t (:background "white smoke"))))
 '(lsp-ui-sideline-code-action ((t (:foreground "orange"))))
 '(lsp-ui-sideline-current-symbol
   ((t (:height 0.99 :weight ultra-bold :box
                (:line-width -1 :color "dim gray" :style nil)
                :foreground "dim gray")))))

(provide '.emacs)
;;; .emacs ends here

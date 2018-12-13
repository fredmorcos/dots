;;; .emacs --- Emacs configuration
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t; -*-

(package-initialize)
(set-face-attribute 'default nil :height 105)
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

;; latex
(load "auctex.el")
(load "preview-latex.el")

(add-hook 'LaTeX-mode-hook #'flyspell-mode)

;; PET
(defconst pet-mode-file "~/Workspace/dots/emacs/pet-mode.el")
(when (file-exists-p pet-mode-file)
  (autoload 'pet-mode pet-mode-file)
  (push '("\\.pet\\'" . pet-mode) auto-mode-alist))

;; hledger
(push '("\\.journal\\'" . hledger-mode) auto-mode-alist)

(add-hook
 'hledger-mode-hook
 #'(lambda () (toggle-truncate-lines t)))

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
     flx
     avy
     ivy
     ivy-rich
     swiper
     counsel
     smex
     org-bullets

     hledger-mode

     f
     ht
     lsp-mode
     lsp-ui
     flycheck
     company
     company-lsp
     rmsbolt

     toml-mode
     markdown-mode
     json-mode
     yaml-mode
     gnuplot-mode

     rust-mode
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

 '(org-cycle-separator-lines 0)
 '(org-indent-indentation-per-level 2)
 '(org-startup-folded t)

 '(rust-indent-offset 2)
 '(rust-indent-method-chain t)
 ;; '(rust-always-locate-project-on-open t)
 '(rust-indent-where-clause t)

 '(lsp-prefer-flymake nil)
 ;; '(lsp-auto-guess-root t)
 ;; '(lsp-hover-text-function 'lsp--text-document-signature-help)

 ;; '(lsp-ui-doc-position 'at-point)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-border "orange red")
 ;; '(lsp-ui-flycheck-list-position 'bottom)

 ;; '(lsp-enable-eldoc nil)
 ;; '(lsp-hover-text-function 'lsp--text-document-signature-help)
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

 '(company-tooltip-align-annotations t)
 '(company-minimum-prefix-length 1)
 ;; '(company-idle-delay 0)
 ;; '(company-lsp-enable-recompletion t)

 ;; '(flycheck-display-errors-delay 0.3)

 '(ediff-split-window-function #'split-window-horizontally)
 '(ediff-window-setup-function #'ediff-setup-windows-plain))

(custom-set-faces
 '(mode-line ((t (:box (:line-width -1 :color "grey75" :style nil)
                       :foreground "gray20"
                       :background "gray80"))))
 '(mode-line-highlight ((t (:box (:line-width 1 :color "grey40" :style nil)))))
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

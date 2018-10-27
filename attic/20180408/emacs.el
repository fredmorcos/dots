;;; .emacs --- Emacs configuration
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t; -*-

;; disable the garbage collector
;; (setq gc-cons-threshold 402653184
;;       gc-cons-percentage 0.6)

;; (setq gc-cons-threshold (eval-when-compile (* 1024 1024 512)))
;; (run-with-idle-timer 2 t #'garbage-collect)

;; disable filename handler
;; (defvar temp--file-name-handler-alist file-name-handler-alist)
;; (setq file-name-handler-alist nil)

;; custom file
(setq custom-file "~/Workspace/dotfiles/emacs/custom.el")

;; font and frame
(set-face-attribute 'default nil :family "Monospace" :height 100)
(toggle-frame-maximized)

;; package
(package-initialize)
(push '("melpa" . "http://melpa.org/packages/") package-archives)

;; disable version control lookups
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(setq-default vc-follow-symlinks t)

;; some useful stuff
(defalias 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(cua-selection-mode 1)

;; requires
(require 'ivy)
;; (require 'dired)
;; (require 'org-indent)

(require 'lsp-mode)
(require 'lsp-ui)
(require 'company-lsp)

(push 'company-lsp company-backends)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(lsp-define-stdio-client
 lsp-rust-mode "language-id"
 (lambda () default-directory)
 '("/home/fred/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rls"))

(add-hook 'rust-mode-hook #'lsp-rust-mode-enable)
(add-hook 'rust-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'flycheck-mode)

;; hooks
(defmacro add-hook-fn (mode-hook fn)
  "Simple 'add-hook' maco using MODE-HOOK and FN."
  (list 'add-hook mode-hook `(lambda () ,fn)))

(add-hook    'focus-out-hook          #'garbage-collect)
(add-hook    'after-init-hook         #'which-key-mode)
(add-hook    'after-init-hook         #'ivy-mode)
(add-hook    'before-save-hook        #'delete-trailing-whitespace)
(add-hook    'prog-mode-hook          #'show-paren-mode)
(add-hook    'emacs-lisp-mode-hook    #'checkdoc-minor-mode)
(add-hook    'emacs-lisp-mode-hook    #'eldoc-mode)
(add-hook    'emacs-lisp-mode-hook    #'flycheck-mode)
;; (add-hook    'rust-mode-hook          #'flycheck-mode)
;; (add-hook    'rust-mode-hook          #'cargo-minor-mode)
;; (add-hook    'rust-mode-hook          #'racer-mode)
;; (add-hook    'racer-mode-hook         #'eldoc-mode)
;; (add-hook-fn 'racer-mode-hook         (local-set-key [f1] 'racer-describe))
;; (add-hook-fn 'cargo-process-mode-hook (toggle-truncate-lines -1))

;; ivy keybindings
(global-set-key (kbd "C-c M-x") #'execute-extended-command)
(global-set-key (kbd "C-s")     #'swiper)
(global-set-key (kbd "C-r")     #'swiper)
(global-set-key (kbd "M-x")     #'counsel-M-x)
(global-set-key (kbd "C-x C-f") #'counsel-find-file)
(global-set-key (kbd "C-c g")   #'counsel-git-grep)

(add-hook
 'ivy-mode-hook
 #'(lambda ()
     (eval-when-compile (defvar ivy-minibuffer-map))
     (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
     (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)))

;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
(setq ivy-initial-inputs-alist nil)

;; keybindings for helpful
;; (require 'shut-up)
;; (require 'helpful)

;; (global-set-key (kbd "C-h f")   #'helpful-callable)
;; (global-set-key (kbd "C-h v")   #'helpful-variable)
;; (global-set-key (kbd "C-h k")   #'helpful-key)
;; (global-set-key (kbd "C-c C-.") #'helpful-at-point)

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

(custom-set-variables
 ;; proxy
 '(url-proxy-services '(("no_proxy" . "127.0.0.1")))
 '(url-privacy-level 'high)
 ;; debugging
 ;; '(debug-on-error t)
 ;; package
 '(package-check-signature nil)
 '(package-selected-packages
   '(which-key flx ivy swiper counsel smex json-mode flycheck rust-mode
               cargo racer toml-mode))
 ;; helpful pdf-tools hledger-mode ob-rust
 ;; general
 '(load-prefer-newer t)
 '(inhibit-compacting-font-caches t)
 '(auto-window-vscroll nil)
 ;; interface
 '(font-use-system-font nil)
 '(blink-cursor-mode nil)
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(inhibit-startup-buffer-menu t)
 '(initial-scratch-message nil)
 '(initial-major-mode 'fundamental-mode)
 '(frame-title-format "%b - emacs")
 '(help-window-select t)
 '(uniquify-buffer-name-style 'forward)
 ;; backups
 '(savehist-mode t)
 '(save-place t)
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
 ;; file encoding
 '(coding-system-for-read 'utf-8-unix)
 '(coding-system-for-write 'utf-8-unix)
 ;; indentation
 '(indent-tabs-mode nil)
 '(standard-indent 2)
 ;; filling
 '(fill-column 70)
 '(colon-double-space t)
 '(default-justification 'left)
 ;; whitespace
 '(mode-require-final-newline 'visit-save)
 '(require-final-newline 'visit-save)
 '(show-trailing-whitespace nil)
 '(whitespace-action '(cleanup))
 '(whitespace-style
   '(face tabs lines-tail trailing space-before-tab
          indentation empty space-after-tab))
 ;; comments
 '(comment-column 40)
 '(comment-inline-offset 1)
 '(comment-empty-lines nil)
 '(comment-multi-line t)
 '(comment-padding 1)
 '(comment-auto-fill-only-comments t)
 '(comment-style 'extra-line)
 '(comment-fill-column 70)
 ;; scrolling
 '(scroll-conservatively 101)
 ;; auto revert
 '(auto-revert-interval 1)
 ;; dired
 '(dired-listing-switches "-alh --group-directories-first")
 '(dired-use-ls-dired t)
 '(dired-omit-files "^\\...+$")
 '(dired-dwim-target t)
 '(wdired-use-dired-vertical-movement 'sometimes)
 ;; xml
 '(nxml-slash-auto-complete-flag t)
 '(nxml-auto-insert-xml-declaration-flag t)
 '(nxml-sexp-element-flag t)
 ;; sh-mode
 '(sh-indentation 2)
 '(sh-basic-offset 2)
 ;; auctex
 ;; '(TeX-auto-save t)
 ;; '(TeX-parse-self t)
 ;; '(TeX-master nil)
 ;; rust
 '(rust-indent-offset 2)
 '(rust-indent-method-chain t)
 '(rust-always-locate-project-on-open t)
 ;; json
 '(json-reformat:indent-width 2)
 '(json-reformat:pretty-string? t)
 '(js-indent-level 2)
 ;; ivy
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-wrap t)
 '(ivy-regex-ignore-order t)
 '(ivy--regex-ignore-order t)
 '(ivy-extra-directories '())
 ;; ediff
 '(ediff-split-window-function #'split-window-horizontally)
 '(ediff-window-setup-function #'ediff-setup-windows-plain)
 ;; compilation mode
 '(compilation-always-kill t)
 '(compilation-window-height 20)
 ;; font-lock
 ;; '(font-lock-maximum-decoration nil)
 ;; '(font-lock-support-mode 'jit-lock-mode)
 ;; '(jit-lock-stealth-time 16)
 ;; '(jit-lock-defer-contextually t)
 ;; '(jit-lock-stealth-nice 0.5)
 ;; from emacs bootstrap
 '(save-interprogram-paste-before-kill t)
 '(mouse-yank-at-point t)
 '(visible-bell nil)
 '(ring-bell-function 'ignore)
 '(minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
 '(cursor-in-non-selected-windows nil)
 '(highlight-nonselected-windows nil)
 '(indent-tabs-mode nil)
 '(fringes-outside-margins t)
 '(x-select-enable-clipboard t))

;; latex
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
;; (add-hook 'LaTeX-mode-hook #'flyspell-mode)

;; Emacs lisp
(push '("\\emacs\\'" . emacs-lisp-mode) auto-mode-alist)

;; PET mode
(defconst pet-mode-file "~/Workspace/dotfiles/emacs/pet-mode.el")
(when (file-exists-p pet-mode-file)
  (autoload 'pet-mode pet-mode-file)
  (push '("\\.pet\\'" . pet-mode) auto-mode-alist))

;; hledger mode
;; (push '("\\.journal\\'" . hledger-mode) auto-mode-alist)

;; dired
(add-hook 'dired-mode-hook #'auto-revert-mode)
(add-hook 'dired-mode-hook  'dired-hide-details-mode)
(add-hook
 'dired-mode-hook
 #'(lambda ()
     (local-set-key [f5] #'(lambda ()
                             (interactive)
                             (term "/bin/bash")))
     (local-set-key (kbd "C-s") 'dired-isearch-filenames)))

;; term
(add-hook
 'term-mode-hook
 #'(lambda ()
     (advice-add
      'term-handle-exit :after
      #'(lambda ()
          (let ((buf (current-buffer)))
            (when (null (get-buffer-process buf))
              (kill-buffer buf)))))))

;; org
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook
 'org-mode-hook
 #'(lambda ()
     ;; (org-babel-do-load-languages
     ;;  'org-babel-load-languages
     ;;  '((rust . t)))
     (require 'ox-latex)
     (eval-when-compile (defvar org-latex-packages-alist))
     (eval-when-compile (defvar org-latex-listings))
     (eval-when-compile (defvar org-latex-pdf-process))
     (add-to-list 'org-latex-packages-alist '("" "minted"))
     (setq-local org-latex-listings 'minted)
     (setq-local
      org-latex-pdf-process
      '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
     (setq-local default-justification 'full)))

;; re-enable filename handler
;; (setq file-name-handler-alist temp--file-name-handler-alist)

;; re-enable the garbage collector
;; (add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 16777216
;;                                                gc-cons-percentage 0.1)))

(provide '.emacs)
;;; .emacs ends here

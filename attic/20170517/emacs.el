;;; package -- Summary
;;; Commentary:
;;; Code:

;; (custom-set-variables
;;  '(debug-on-error t))

(defun url-setup ()
  "Setup URL settings."
  (custom-set-variables
   '(url-proxy-services '(("no_proxy" . "127.0.0.1")))
   '(url-privacy-level 'high)))
(eval-after-load 'url #'url-setup)

(package-initialize)
(custom-set-variables
 '(package-check-signature nil)
 '(package-selected-packages
   '(org clang-format elpy flycheck org-bullets which-key magit htmlize iedit
         flycheck-pos-tip ghc haskell-mode company-ghc flycheck-haskell popup
         flycheck-hdevtools flycheck-ghcmod paredit json-mode company-irony
         company-irony-c-headers flycheck-irony irony-eldoc c-eldoc)))
(push '("melpa" . "http://melpa.org/packages/") package-archives)

;; don't lookup version control information
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(setq-default vc-follow-symlinks t)

;; font and frame size
(custom-set-faces '(default ((t (:height 105)))))
(toggle-frame-maximized)

(add-hook 'focus-out-hook #'garbage-collect)

;; color modifications
(defalias 's-f-a 'set-face-attribute)
(s-f-a 'mode-line-inactive  nil :box '(:line-width 1 :color "grey75"))
(s-f-a 'mode-line-highlight nil :box '(:line-width 1 :color "grey60"))
(s-f-a 'mode-line           nil :box '(:line-width 1 :color "grey70"))
(s-f-a 'cursor              nil :background "steel blue")
(s-f-a 'show-paren-match    nil :background "light steel blue")
(s-f-a 'fringe              nil :foreground "grey75")
(s-f-a 'highlight           nil :background "grey97")

;; custom file
(setq custom-file "~/Workspace/dotfiles/emacs/custom.el")
;; (load custom-file)

;; general settings
(defalias 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
(cua-selection-mode 1)

;; helper to get system info
(defconst userinfo-username
  (let ((sysname (system-name)))
    (cond ((equal sysname "floron") "fnm")
          ((equal sysname "axon") "fred"))))

;; user full name and email
(cond ((equal userinfo-username "fnm")
       (defconst emacs-user-full-name "Frederic-Gerald Morcos")
       (defconst emacs-user-email "frederic-gerald.n.morcos@intel.com"))
      ((equal userinfo-username "fred")
       (defconst emacs-user-full-name "Fred Morcos")
       (defconst emacs-user-email "fred.morcos@gmail.com")))

(defun disable-some-modes ()
  "Disable some slow and unused modes."
  (abbrev-mode -1)
  (pyvenv-mode -1))

;; general hooks
(add-hook 'after-init-hook      #'global-hl-line-mode)
(add-hook 'after-init-hook      #'which-key-mode)
(add-hook 'before-save-hook     #'delete-trailing-whitespace)
(add-hook 'prog-mode-hook       #'show-paren-mode)
(add-hook 'prog-mode-hook       #'company-mode)
(add-hook 'prog-mode-hook       #'flycheck-mode)
(add-hook 'flycheck-mode-hook   #'flycheck-pos-tip-mode)
(add-hook 'text-mode-hook       #'disable-some-modes)
(add-hook 'prog-mode-hook       #'disable-some-modes)
(add-hook 'LaTeX-mode-hook      #'flycheck-mode)
(add-hook 'LaTeX-mode-hook      #'company-mode)
(add-hook 'LaTeX-mode-hook      #'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)

;; file position, backups, etc...
(require 'saveplace)
(defconst emacs-temp-dir          (concat temporary-file-directory "emacs/"))
(defconst emacs-autosaves-dir     (concat emacs-temp-dir "autosaves"))
(defconst emacs-backups-dir       (concat emacs-temp-dir "backups"))
(defconst emacs-places-file       (concat user-emacs-directory "places"))
(defconst emacs-recentf-file      (concat user-emacs-directory "recentf"))
(defconst emacs-autosaves-pattern (concat emacs-autosaves-dir "/\\1"))
(defconst emacs-backups-pattern   (concat emacs-backups-dir "/"))

(make-directory emacs-autosaves-dir t)
(make-directory emacs-backups-dir   t)

(custom-set-variables
 ;; general
 '(load-prefer-newer t)
 '(inhibit-compacting-font-caches t)
 ;; interface
 '(font-use-system-font t)
 '(blink-cursor-mode nil)
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(menu-bar-mode nil)
 '(inhibit-startup-screen t)
 '(inhibit-startup-message t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-echo-area-message userinfo-username)
 '(initial-scratch-message nil)
 '(initial-major-mode 'fundamental-mode)
 '(column-number-mode t)
 '(size-indication-mode nil)
 '(hl-line-sticky-flag nil)
 '(switch-to-visible-buffer nil)
 '(window-combination-resize t)
 '(frame-title-format "%b - emacs")
 ;; backups
 '(savehist-mode t)
 '(save-place t)
 '(save-place-file emacs-places-file)
 '(recentf-mode t)
 '(recentf-save-file emacs-recentf-file)
 '(auto-save-mode t)
 '(auto-save-file-name-transforms
   `((".*" ,emacs-autosaves-pattern t)))
 '(backup-directory-alist
   `((".*" . ,emacs-backups-pattern)))
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
 '(electric-indent-mode t)
 '(default-justification 'left)
 '(fill-column 80)
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
 '(comment-fill-column 80)
 ;; spell checking
 '(ispell-program-name "hunspell")
 ;; scrolling
 '(scroll-step 1)
 '(scroll-margin 3)
 '(scroll-conservatively 101)
 ;; auto revert
 '(auto-revert-interval 1)
 ;; dired
 '(dired-listing-switches "-alh --group-directories-first")
 '(dired-use-ls-dired t)
 '(dired-omit-files "^\\...+$")
 '(wdired-use-dired-vertical-movement 'sometimes)
 ;; gud-mode and gdb
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(gud-tooltip-mode t)
 ;; compilation
 '(compilation-always-kill t)
 '(compilation-read-command nil)
 '(compilation-scroll-output 'first-error)
 '(compile-command "make")
 ;; c-mode
 '(c-cleanup-list
   '(brace-else-brace
     brace-elseif-brace
     brace-catch-brace
     scope-operator
     compact-empty-funcall
     comment-close-slash))
 '(c-macro-shrink-window-flag t)
 '(Man-width 80)
 ;; python
 '(python-indent-guess-indent-offset nil)
 '(python-indent-offset 4)
 '(python-skeleton-autoinsert t)
 '(python-shell-interpreter-args "-i -W once")
 ;; tool-tips
 '(tooltip-hide-delay 20)
 '(x-gtk-use-system-tooltips nil)
 ;; org
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-confirm-babel-evaluate nil)
 '(org-startup-folded t)
 '(org-catch-invisible-edits t)
 '(org-html-html5-fancy t)
 '(org-html-postamble nil)
 ;; xml
 '(nxml-slash-auto-complete-flag t)
 '(nxml-auto-insert-xml-declaration-flag t)
 '(nxml-sexp-element-flag t)
 ;; company
 ;; '(company-tooltip-align-annotations t)
 ;; '(company-idle-delay 0.2)
 ;; '(company-tooltip-idle-delay 0.2)
 ;; '(company-minimum-prefix-length 3)
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(company-echo-truncate-lines nil)
 '(company-lighter-base "COMP")
 ;; flycheck
 '(flycheck-display-errors-delay 0.1)
 '(flycheck-pos-tip-timeout 20)
 '(flycheck-idle-change-delay 0.1)
 ;; rtags
 ;; '(rtags-track-container t)
 ;; '(rtags-container-timer-interval 0.2)
 ;; '(rtags-rc-log-enabled t)
 ;; '(rtags-verbose-results t)
 ;; '(rtags-rdm-process-use-pipe t)
 ;; '(rtags-reparse-timeout 200)
 '(rtags-completions-enabled nil)
 '(rtags-periodic-reparse-timeout 0.2)
 '(rtags-enable-unsaved-reparsing t)
 '(rtags-reindex-on-save t)
 '(rtags-show-containing-function t)
 '(rtags-autostart-diagnostics t)
 '(rtags-display-summary-as-tooltip t)
 ;; '(rtags-display-current-error-as-tooltip t)
 '(rtags-find-file-case-insensitive t)
 '(rtags-symbolnames-case-insensitive t)
 '(rtags-tooltips-enabled t)
 ;; irony
 '(company-irony-ignore-case t)
 '(irony-lighter " IR")
 ;; term
 '(term-completion-autolist t)
 '(term-input-ignoredups t)
 ;; dot
 '(graphviz-dot-indent-width 2)
 '(graphviz-dot-toggle-completions t)
 '(graphviz-dot-delete-completions t)
 '(graphviz-dot-view-command "dotty %s")
 '(graphviz-dot-auto-indent-on-semi nil)
 ;; project-explorer
 '(pe/follow-current t)
 '(pe/width 30)
 ;; change-log
 '(add-log-full-name emacs-user-full-name)
 '(add-log-mailing-address emacs-user-email)
 '(change-log-version-info-enabled t)
 ;; sh-mode
 '(sh-indentation 2)
 '(sh-basic-offset 2)
 ;; company-ghc
 '(company-ghc-component-prefix-match t)
 '(company-ghc-show-info t)
 ;; haskell
 '(haskell-decl-scan-bindings-as-variables t)
 '(haskell-doc-show-global-types t)
 '(haskell-align-imports-pad-after-name t)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-doc-chop-off-context nil)
 '(haskell-stylish-on-save t)
 ;; '(haskell-tags-on-save t)
 '(hindent-reformat-buffer-on-save t)
 '(haskell-process-show-debug-tips nil)
 ;; auctex
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(TeX-master nil))

;; latex
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Emacs lisp
(push '("\\emacs\\'" . emacs-lisp-mode) auto-mode-alist)

;; PET mode
(defconst pet-mode-file "~/Workspace/dotfiles/emacs/pet-mode.el")
(when (file-exists-p pet-mode-file)
  (autoload 'pet-mode pet-mode-file)
  (push '("\\.pet\\'" . pet-mode) auto-mode-alist))

;; make script files executable after saving
(defun make-buffer-executable ()
  "Make buffer executable."
  (executable-make-buffer-file-executable-if-script-p))
(add-hook 'after-save-hook #'make-buffer-executable)

;; run term from dired
(defun run-term ()
  "Run terminal."
  (interactive)
  (term "/bin/bash"))

(defun dired-mode-setup ()
  "Dired mode setup."
  (declare-function dired-isearch-filenames "dired")
  (local-set-key [f5] #'run-term)
  (local-set-key (kbd "C-s") #'dired-isearch-filenames))

(add-hook 'dired-mode-hook #'auto-revert-mode)
(add-hook 'dired-mode-hook  'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'dired-mode-setup)

;; kill term buffer when it exits with ctrl-d
(defun kill-term-buffer (&rest args)
  "Kill terminal buffer, ignore ARGS."
  (let ((buf (current-buffer)))
    (when (null (get-buffer-process buf))
      (kill-buffer buf))))

(defun kill-buffer-on-term-exit ()
  "Kill term buffer on exit."
  (advice-add 'term-handle-exit :after #'kill-term-buffer))

(add-hook 'term-mode-hook #'kill-buffer-on-term-exit)

;; company
(defun company-mode-setup ()
  "Setup company mode."
  (defvar company-backends)
  (setq-local company-backends (delete 'company-bbdb         company-backends))
  (setq-local company-backends (delete 'company-css          company-backends))
  (setq-local company-backends (delete 'company-semantic     company-backends))
  (setq-local company-backends (delete 'company-clang        company-backends))
  (setq-local company-backends (delete 'company-xcode        company-backends))
  (setq-local company-backends (delete 'company-cmake        company-backends))
  (setq-local company-backends (delete 'company-gtags        company-backends))
  (setq-local company-backends (delete 'company-etags        company-backends))
  (setq-local company-backends (delete 'company-oddmuse      company-backends))
  (setq-local company-backends (delete 'company-eclim        company-backends))
  (setq-local company-backends (delete 'company-dabbrev      company-backends))
  (setq-local company-backends (delete 'company-dabbrev-code company-backends))
  ;; (setq-local company-backends `(company-dabbrev-code . ,company-backends)))
  (setq-local company-backends (delete '(company-dabbrev-code
                                         company-gtags
                                         company-etags
                                         company-keywords)
                                       company-backends)))

(add-hook 'company-mode #'company-mode-setup)

;; python - run (elpy-config) at some point
;; (add-hook 'after-init-hook #'elpy-enable)
(add-hook 'python-mode #'elpy-mode)

;; c-mode and c++-mode
(defun c-mode-common-setup ()
  "Setup C/C++ programming mode."
  (local-set-key [f1] #'manual-entry)
  (local-set-key [f2] #'ff-find-other-file)
  (require 'rtags)
  ;; (require 'company-rtags)
  (require 'flycheck-rtags)
  (require 'irony)
  (require 'irony-eldoc)
  (require 'company-irony)
  (require 'company-irony-c-headers)
  (defvar flycheck-disabled-checkers)
  (defvar flycheck-highlighting-mode)
  (defvar flycheck-check-syntax-automatically)
  (defvar company-backends)
  (declare-function rtags-enable-standard-keybindings "rtags")
  (declare-function rtags-diagnostics "rtags")
  (declare-function flycheck-select-checker "flycheck")
  (rtags-enable-standard-keybindings)
  (rtags-diagnostics)
  (company-mode-setup)
  ;; (push 'company-rtags company-backends)
  (irony-mode)
  ;; (irony-cdb-autosetup-compile-options)
  (push '(company-irony-c-headers company-irony) company-backends)
  (setq-local flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil)
  (flycheck-select-checker 'rtags)
  (when (file-exists-p
         (concat (file-name-directory (buffer-file-name)) ".clang-format"))
    (add-hook 'before-save-hook #'clang-format-buffer nil t)))

;; irony mode keybindings
(defun irony-mode-setup ()
  "Setup irony mode."
  (defvar irony-mode-map)
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'c-mode-hook        #'c-turn-on-eldoc-mode)
(add-hook 'c-mode-common-hook #'c-mode-common-setup)
(add-hook 'irony-mode-hook    #'irony-mode-setup)
(add-hook 'irony-mode-hook    #'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook    #'irony-eldoc)

;; haskell
(defun haskell-mode-setup ()
  "Setup haskell mode."
  (defconst
    ghc-mod-el-file
    "~/.cabal/share/x86_64-linux-ghc-8.0.1/ghc-mod-5.7.0.0/elisp/ghc.el")
  (when (file-exists-p ghc-mod-el-file)
    (load-file ghc-mod-el-file))
  (ghc-init)
  (require 'company-ghc)
  (declare-function company-ghc-turn-on-autoscan "company-ghc")
  (defvar company-backends)
  (setq-local company-backends `((company-ghc
                                  company-capf
                                  company-dabbrev-code)
                                 . ,company-backends))
  (company-ghc-turn-on-autoscan))

(defun haskell-mode-flycheck-setup ()
  "Setup flycheck in Haskell mode."
  (if (eq major-mode 'haskell-mode)
      (progn
        (defvar flycheck-disabled-checkers)
        (setq-local flycheck-disabled-checkers
                    `(haskell-stack-ghc . ,flycheck-disabled-checkers)))))

;; (add-hook 'haskell-mode-hook  #'hindent-mode)
(add-hook 'haskell-mode-hook  #'interactive-haskell-mode)
(add-hook 'haskell-mode-hook  #'haskell-decl-scan-mode)
(add-hook 'haskell-mode-hook  #'haskell-doc-mode)
(add-hook 'haskell-mode-hook  #'haskell-mode-setup)
(add-hook 'flycheck-mode-hook #'haskell-mode-flycheck-setup)

;; org
(defun org-mode-setup ()
  "Setup org mode."
  (require 'ob)
  (require 'ob-python)
  (require 'ob-C)
  (require 'ob-makefile)
  (require 'ob-shell)
  (require 'ob-haskell)
  (require 'org-indent)
  (declare-function org-babel-execute-src-block-maybe "org-babel")
  (local-set-key (kbd "C-c C-x") #'org-babel-execute-src-block-maybe)
  (declare-function org-indent-mode "org-indent")
  (org-indent-mode)
  (org-babel-do-load-languages
   'org-babel-load-language
   '((python . t) (C . t) (makefile . t) (shell . t) (haskell . t)))
  (setq-local fill-column 80))

(add-hook 'org-mode-hook #'org-mode-setup)
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook #'org-bullets-mode)

(provide '.emacs)
;;; .emacs ends here

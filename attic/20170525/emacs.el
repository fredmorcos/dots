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
   '(org org-bullets htmlize which-key iedit magit paredit json-mode elpy
         company flycheck flycheck-pos-tip c-eldoc clang-format cache
         flycheck-nim nim-mode)))
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
(add-hook 'text-mode-hook       #'disable-some-modes)
(add-hook 'prog-mode-hook       #'disable-some-modes)
(add-hook 'LaTeX-mode-hook      #'flycheck-mode)
(add-hook 'LaTeX-mode-hook      #'company-mode)
(add-hook 'LaTeX-mode-hook      #'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
(add-hook 'flycheck-mode-hook   #'flycheck-pos-tip-mode)

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
 '(org-pretty-entities t)
 ;; xml
 '(nxml-slash-auto-complete-flag t)
 '(nxml-auto-insert-xml-declaration-flag t)
 '(nxml-sexp-element-flag t)
 ;; company
 '(company-selection-wrap-around t)
 '(company-echo-truncate-lines nil)
 '(company-lighter-base "Comp")
 ;; eldoc
 '(eldoc-minor-mode-string " Eld")
 '(eldoc-echo-area-use-multiline-p t)
 ;; term
 '(term-completion-autolist t)
 '(term-input-ignoredups t)
 ;; change-log
 '(add-log-full-name emacs-user-full-name)
 '(add-log-mailing-address emacs-user-email)
 '(change-log-version-info-enabled t)
 ;; sh-mode
 '(sh-indentation 2)
 '(sh-basic-offset 2)
 ;; auctex
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(TeX-master nil)
 ;; nim
 '(nim-nimsuggest-path "~/nim/bin/nimsuggest"))

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

;; python - run (elpy-config) at some point
;; (add-hook 'after-init-hook #'elpy-enable)
(add-hook 'python-mode #'elpy-mode)

;; nim
(defun nim-mode-setup ()
  "Setup for nim-mode."
  (require 'flycheck-nim)
  (declare-function flycheck-select-checker "flycheck")
  (flycheck-select-checker 'nim))

(add-hook 'nim-mode-hook #'nimsuggest-mode)
(add-hook 'nim-mode-hook #'nim-mode-setup)

;; gud
(defun gud-mode-setup ()
  "Setup GDB and GUD."
  (custom-set-variables '(gud-tooltip-mode t)))

(add-hook 'gud-mode-hook #'gud-mode-setup)

;; c-mode and c++-mode
(defun c-mode-common-setup ()
  "Setup C/C++ programming mode."
  (local-set-key [f1] #'manual-entry)
  (local-set-key [f2] #'ff-find-other-file)
  (maybe-enable-clang-format)
  (defvar flycheck-cppcheck-checks)
  (defvar flycheck-cppcheck-suppressions)
  (defvar flycheck-cppcheck-standards)
  (defvar flycheck-gcc-args)
  (defvar flycheck-clang-args)
  ;; (defvar flycheck-enabled-checkers)
  (declare-function flycheck-add-next-checker "flycheck")
  (defvar company-clang-arguments)
  (setq-local flycheck-cppcheck-checks '("all"))
  (setq-local flycheck-cppcheck-suppressions
              '("missingIncludeSystem" "readdirCalled"
                "unmatchedSuppression" "unusedFunction"))
  (setq-local flycheck-cppcheck-standards
              `("posix"
                ,(cond ((eq major-mode 'c-mode) "c99")
                       ((eq major-mode 'c++-mode) "c++11"))))
  (when (not 'org-src-mode)
    (setq-local flycheck-cppcheck-include-path
                `(,(buffer-dir (file-name-directory buffer-file-name))
                  ,flycheck-cppcheck-include-path)))
  ;; (setq-local flycheck-enabled-checkers
  ;;             '(c/c++-clang c/c++-gcc c/c++-cppcheck))
  (flycheck-add-next-checker 'c/c++-clang '(info . c/c++-gcc))
  (flycheck-add-next-checker 'c/c++-gcc '(info . c/c++-cppcheck))
  (let* ((makefile (find-makefile))
         (flags-clang (make-flags makefile 'clang)))
    (progn
      (setq-local flycheck-gcc-args (make-flags makefile 'gcc))
      (setq-local flycheck-clang-args flags-clang)
      (setq-local company-clang-arguments flags-clang))))

(defun maybe-enable-clang-format ()
  "Enable clang-format for buffer if .clang-format file is found."
  (let* ((buffer-file (buffer-file-name))
         (buffer-dir (file-name-directory buffer-file))
         (clang-format-file (concat buffer-dir ".clang-format")))
    (when (file-exists-p clang-format-file)
      (add-hook 'before-save-hook #'clang-format-buffer nil t))))

(defun find-makefile-helper (filename)
  "Helper for the find-makefile function using FILENAME."
  (when buffer-file-name
    (let ((dir (locate-dominating-file buffer-file-name filename)))
      (when dir (concat (file-name-as-directory dir) filename)))))

(defun find-makefile ()
  "Find a Makefile for the current buffer."
  (let ((f (find-makefile-helper "Makefile")))
    (if f f (find-makefile-helper "makefile"))))

(defun make-args (makefile-name cc)
  "Get make args from MAKEFILE-NAME and CC."
  (append
   `("-f" ,makefile-name)
   (cond
    ((eq cc 'gcc)
     (cond ((eq major-mode 'c-mode) '("show-cflags" "CC=gcc"))
           ((eq major-mode 'c++-mode) '("show-cxxflags" "CXX=g++"))))
    ((eq cc 'clang)
     (cond ((eq major-mode 'c-mode) '("show-cflags" "CC=clang"))
           ((eq major-mode 'c++-mode) '("show-cxxflags" "CXX=clang++")))))))

(defun make-flags-helper (makefile-name cc)
  "Get flags based on MAKEFILE-NAME and CC."
  (when makefile-name
    (let* ((args (make-args makefile-name cc))
           (out (with-temp-buffer
                  (list (apply #'call-process
                               "make" nil (current-buffer) nil args)
                        (buffer-string))))
           (ret (car out))
           (txt (car (cdr out))))
      (when (eq ret 0)
        (split-string (substring txt 0 -1))))))

(defun make-flags (makefile-name cc)
  "Get flags based on MAKEFILE-NAME and CC or default ones."
  (let ((flags (make-flags-helper makefile-name cc))
        (compiler-std (cond ((eq major-mode 'c-mode) "-std=c99")
                            ((eq major-mode 'c++-mode) "-std=c++11"))))
    (if flags flags
      `(,compiler-std "-W" "-Wall" "-Wextra" "-Wpedantic" "-pedantic"
                      "-D_DEFAULT_SOURCE" "-D_FILE_OFFSET_BITS=64"))))

;; (add-hook 'c-mode-common-hook #'c-turn-on-eldoc-mode)
(add-hook 'c-mode-common-hook #'c-mode-common-setup)

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
   '((python . t) (C . t) (makefile . t) (shell . t)))
  (setq-local fill-column 80)
  (text-scale-set 2))

(add-hook 'org-mode-hook #'org-mode-setup)
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'flyspell-mode)
(add-hook 'org-mode-hook #'org-bullets-mode)

(provide '.emacs)
;;; .emacs ends here

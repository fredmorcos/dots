;;; package -- Summary
;;; Commentary:
;;; Code:

(custom-set-variables
 '(debug-on-error t))

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
   '(org clang-format elpy flycheck org org-bullets which-key company-irony
         company-irony-c-headers flycheck-irony company-quickhelp magit htmlize
         flycheck-pos-tip json-mode ghc haskell-mode company-ghc irony-eldoc
         flycheck-haskell flycheck-hdevtools flycheck-ghcmod paredit iedit)))
(push '("melpa" . "http://melpa.org/packages/") package-archives)
;; (unless package-archive-contents (package-refresh-contents))
;; (package-install-selected-packages)
;; (async-start 'package-refresh-contents 'ignore)

;; don't lookup version control information
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(setq-default vc-follow-symlinks t)

;; font and frame size
(custom-set-faces '(default ((t (:height 105)))))
(toggle-frame-maximized)

;; garbage collector
;; (defconst gc-thresh 10000)
;; (custom-set-variables
;;  '(gc-cons-percentage 50.0)
;;  '(gc-cons-threshold gc-thresh))

;; (defun gc-disable ()
;;   "Disable garbage collection."
;;   (setq gc-cons-threshold most-positive-fixnum))

;; (defun gc-enable ()
;;   "Enable garbage collection."
;;   (setq gc-cons-threshold gc-thresh))

(add-hook 'focus-out-hook #'garbage-collect)
;; (add-hook 'minibuffer-setup-hook #'gc-disable)
;; (add-hook 'minibuffer-exit-hook  #'gc-enable)

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

;; idle cleanup stuff
(defun delete-if-relevant (filename)
  "Delete a file with name FILENAME if it is relevant."
  (defconst full-filename (concat "/tmp/" filename))
  (when (and (or (string-prefix-p "irony." filename)
                 (string-prefix-p "preamble-" filename))
             (file-exists-p full-filename))
    (delete-file full-filename)))

(defun cleanup-irony-files ()
  "Cleanup irony and clang files in /tmp."
  (mapc #'delete-if-relevant (directory-files "/tmp")))

;; (when (equal (system-name) "floron")
;;   (run-with-idle-timer 10 't #'cleanup-irony-files))
(add-hook 'kill-emacs-hook #'cleanup-irony-files)

(defun disable-some-modes ()
  "Disable some slow and unused modes."
  (abbrev-mode -1)
  (pyvenv-mode -1))

;; general hooks
(add-hook 'after-init-hook    #'global-hl-line-mode)
(add-hook 'after-init-hook    #'which-key-mode)
(add-hook 'before-save-hook   #'delete-trailing-whitespace)
(add-hook 'prog-mode-hook     #'show-paren-mode)
(add-hook 'prog-mode-hook     #'company-mode)
(add-hook 'prog-mode-hook     #'flycheck-mode)
(add-hook 'company-mode-hook  #'company-quickhelp-mode)
(add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode)
(add-hook 'text-mode-hook     #'disable-some-modes)
(add-hook 'prog-mode-hook     #'disable-some-modes)
;; (add-hook 'prog-mode-hook     #'whitespace-mode)

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
 ;; company
 '(company-irony-ignore-case t)
 '(company-idle-delay 0.2)
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(company-tooltip-idle-delay 0.2)
 '(company-tooltip-align-annotations t)
 '(company-minimum-prefix-length 3)
 ;; flycheck
 '(flycheck-display-errors-delay 0.1)
 '(flycheck-pos-tip-timeout 20)
 '(flycheck-idle-change-delay 0.1)
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
 '(hindent-reformat-buffer-on-save t))

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
  (local-set-key [f5] #'run-term))

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
  (setq-local company-backends (delete 'company-bbdb     company-backends))
  (setq-local company-backends (delete 'company-css      company-backends))
  (setq-local company-backends (delete 'company-semantic company-backends))
  (setq-local company-backends (delete 'company-clang    company-backends))
  (setq-local company-backends (delete 'company-xcode    company-backends))
  (setq-local company-backends (delete 'company-cmake    company-backends))
  (setq-local company-backends (delete 'company-gtags    company-backends))
  (setq-local company-backends (delete 'company-etags    company-backends))
  (setq-local company-backends `(company-capf         . ,company-backends))
  (setq-local company-backends `(company-dabbrev-code . ,company-backends)))

(add-hook 'company-mode #'company-mode-setup)

;; python - run (elpy-config) at some point
;; (add-hook 'after-init-hook #'elpy-enable)
(add-hook 'python-mode #'elpy-mode)

;; c-mode and c++-mode
(defun c-mode-common-setup ()
  "Setup C/C++ programming mode."
  (local-set-key [f1] #'manual-entry)
  (local-set-key [f2] #'ff-find-other-file)
  (local-set-key [f5] #'irony-get-type)
  (local-set-key [f7] #'run-make)
  (local-set-key [f8] #'close-compilation-buffer)
  (setq-local irony-additional-clang-options
              (makefile-get-cflags-or-default 'clang))
  (defvar company-backends)
  (setq-local company-backends `((company-irony
                                  company-irony-c-headers)
                                 . ,company-backends))
  (if (eq major-mode 'c++-mode)
      (add-hook 'before-save-hook #'clang-format-buffer nil t)))

(defun c-mode-flycheck-gcc ()
  "Enable flycheck gcc."
  (defvar flycheck-gcc-args)
  (setq-local flycheck-gcc-args (makefile-get-cflags-or-default 'gcc))
  (flycheck-add-next-checker 'irony 'c/c++-gcc))

(defun c-mode-flycheck-cppcheck ()
  "Enable flycheck cppcheck."
  (defvar flycheck-cppcheck-checks)
  (defvar flycheck-cppcheck-suppressions)
  (defvar flycheck-cppcheck-standards)
  (defvar flycheck-cppcheck-include-path)
  (setq-local flycheck-cppcheck-checks '("all"))
  (setq-local flycheck-cppcheck-suppressions
              '("missingIncludeSystem" "readdirCalled"
                "unmatchedSuppression" "unusedFunction"))
  (setq-local flycheck-cppcheck-standards
              `(,(if (eq major-mode 'c-mode) "c99" "c++11") "posix"))
  (when (not 'org-src-mode)
    (let ((buffer-dir (file-name-directory buffer-file-name)))
      (setq-local flycheck-cppcheck-include-path
                  `(buffer-dir . ,flycheck-cppcheck-include-path)))))

(defun c-mode-flycheck-clangcheckers ()
  "Enable flycheck clangcheck and clangtidy."
  (push "~/Workspace/flycheck-clangcheck" load-path)
  ;; (require 's)
  (require 'flycheck-clangcheck)
  (declare-function flycheck-clangcheck-setup "flycheck-clangcheck.el")
  (flycheck-clangcheck-setup)
  (require 'flycheck-clangtidy)
  (declare-function flycheck-clangtidy-setup "flycheck-clangtidy.el")
  (flycheck-clangtidy-setup)
  (defvar flycheck-clangcheck-extra-arg)
  (defvar flycheck-clangtidy-extra-arg)
  (let ((cflags (makefile-get-cflags-or-default 'clang)))
    (setq-local flycheck-clangcheck-extra-arg cflags)
    (setq-local flycheck-clangcheck-extra-arg cflags)))

(defun toggle-extra-c/c++-checkers ()
  "Toggle extra flycheck checkers for C/C++."
  (interactive)
  (defvar *extra-c/c++-checkers-on* t)
  (defvar flycheck-disabled-checkers)
  (if (eq *extra-c/c++-checkers-on* t)
      (progn
        (setq-local flycheck-disabled-checkers
                    '(c/c++-clang
                      c/c++-gcc
                      c/c++-clangcheck
                      c/c++-clangtidy))
        (setq-local *extra-c/c++-checkers-on* nil)
        (message "Extra C/C++ Checkers turned off"))
    (progn
      (setq-local flycheck-disabled-checkers '(c/c++-clang))
      (setq-local *extra-c/c++-checkers-on* t)
      (message "Extra C/C++ Checkers turned on")))
  (declare-function flycheck-buffer "flycheck.el")
  (flycheck-buffer))

(defun c-mode-flycheck-setup ()
  "Setup flycheck in C and C++ modes."
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (progn
        (defvar flycheck-disabled-checkers)
        (setq-local flycheck-disabled-checkers '(c/c++-clang))
        (flycheck-irony-setup)
        (c-mode-flycheck-cppcheck)
        (c-mode-flycheck-gcc)
        (c-mode-flycheck-clangcheckers)
        (declare-function flycheck-add-next-checker "flycheck.el")
        (flycheck-add-next-checker 'c/c++-clangtidy 'c/c++-clangcheck t)
        (flycheck-add-next-checker 'c/c++-clangcheck 'irony t)
        (flycheck-add-next-checker 'irony 'c/c++-cppcheck t)
        (flycheck-add-next-checker 'irony 'c/c++-gcc t)
        (toggle-extra-c/c++-checkers)
        (local-set-key [f6] #'toggle-extra-c/c++-checkers))))

(defun compose-comp-cmds (cmds)
  "Compose a compilation mode command from CMDS."
  (if (not cmds) "make clean"
    (concat "make clean && " (car cmds) " && "
            (compose-comp-cmds (cdr cmds)))))

(defun run-make ()
  "Run make for the current buffer."
  (interactive)
  (cond ((eq major-mode 'c-mode)
         (compile (compose-comp-cmds
                   '("make cppcheck"
                     "make CC=gcc"
                     "make CC=clang"
                     "make CC=gcc RELEASE=yes"
                     "make CC=clang RELEASE=yes"
                     "scan-build make CC=gcc"
                     "scan-build make CC=clang"
                     "scan-build make CC=gcc RELEASE=yes"
                     "scan-build make CC=clang RELEASE=yes"))))
        ((eq major-mode 'c++-mode)
         (compile (compose-comp-cmds
                   '("make cppcheck"
                     "make CXX=g++"
                     "make CXX=clang++"
                     "make CXX=g++ RELEASE=yes"
                     "make CXX=clang++ RELEASE=yes"
                     "scan-build make CXX=g++"
                     "scan-build make CXX=clang++"
                     "scan-build make CXX=g++ RELEASE=yes"
                     "scan-build make CXX=clang++ RELEASE=yes"))))))

(defun close-compilation-buffer ()
  "Close the make compilation buffer."
  (interactive)
  (let ((win (get-buffer-window "*compilation*")))
    (kill-buffer "*compilation*")
    (let ((buf (window-buffer win)))
      (if (string-prefix-p "*" (buffer-name buf))
          (delete-window win)))))

(defun find-makefile ()
  "Find Makefile from current buffer's C/C++ file's directory."
  (when buffer-file-name
    (let ((dir (locate-dominating-file buffer-file-name "Makefile")))
      (when dir (concat (file-name-as-directory dir) "Makefile")))))

(defun make-get-cflags-cmd (makefile-name cc)
  "Get cflags or cxxflags cmd based on MAKEFILE-NAME and CC."
  (concat (concat "make -f " makefile-name " ")
          (cond ((eq major-mode 'c-mode)
                 (concat "show-cflags CC="
                         (cond ((eq cc 'clang) "clang")
                               ((eq cc 'gcc)   "gcc"))))
                ((eq major-mode 'c++-mode)
                 (concat "show-cxxflags CXX="
                         (cond ((eq cc 'clang) "clang++")
                               ((eq cc 'gcc)   "g++")))))))

(defun makefile-get-cflags (compiler)
  "Gets CFLAGS from a Makefile with COMPILER into a list of strings."
  (let ((makefile (find-makefile)))
    (when makefile
      (let* ((cmd (make-get-cflags-cmd makefile compiler))
             (cmd-output (shell-command-to-string cmd)))
        (when (not (string-prefix-p "make: ***" cmd-output t))
          (split-string (substring cmd-output 0 -1)))))))

(defun makefile-get-cflags-or-default (compiler)
  "Gets CFLAGS from a Makefile with COMPILER or defaults ones on error."
  (let ((cflags (makefile-get-cflags compiler)))
    (if cflags cflags
      `("-W" "-Wall" "-Wextra" "-D_DEFAULT_SOURCE"
        ,(concat "-std=" (cond ((eq major-mode 'c++-mode) "c++11")
                               ((eq major-mode 'c-mode)   "c99")))))))

(add-hook 'c-mode-common-hook #'irony-mode)
(add-hook 'c-mode-common-hook #'c-mode-common-setup)
(add-hook 'flycheck-mode-hook #'c-mode-flycheck-setup)
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
;; (add-hook 'org-mode-hook #'org-preview-html-mode)
(add-hook 'org-mode-hook #'org-bullets-mode)

;; rtm
;; (defun rtm-mode-setup ()
;;   "Setup RTM mode."
;;   (set-face-attribute 'simple-rtm-task nil :foreground "grey30")
;;   (set-face-attribute 'simple-rtm-task-duedate nil
;;                       :foreground "lime green" :weight 'bold)
;;   (set-face-attribute 'simple-rtm-task-duedate-due nil
;;                       :foreground "tomato" :background nil :weight 'bold)
;;   (set-face-attribute 'simple-rtm-task-priority-1 nil
;;                       :foreground "orange" :weight 'bold))

;; (defun check-all-mail ()
;;   "Check all online services."
;;   (interactive)
;;   (simple-rtm-mode)
;;   (rtm-mode-setup))

;; (global-set-key [f9] #'check-all-mail)

(provide '.emacs)
;;; .emacs ends here

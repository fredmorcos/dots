(load-theme 'tango-dark)

(custom-set-faces
 '(cursor ((t (:background "dim gray")))))
(set-face-attribute 'default nil :family "Monaco" :height 102)

(require 'package)
(add-to-list
 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list
 'package-archives '("marmalade" "http://marmalade-repo.org/packages/"))

;; (require 'ido)
;; (ido-mode t)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode")
(load "haskell-mode-autoloads")

;; (load "~/.emacs.d/elpa/haskell-mode-20130724.1040/haskell-mode")

;; (add-to-list 'load-path "~/.emacs.d/elpa/haskell-mode-20130724.1040/")
;; (require 'haskell-mode-autoloads)

;; (add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)
;; (add-hook 'haskell-mode-hook 'flymake-hlint-load)

;; (add-to-list 'load-path "~/.emacs.d/elpa/ghc-20130520.1540/")
;; (autoload 'ghc-init "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

(ispell-change-dictionary "american")

(cwarn-mode)

(custom-set-variables
 '(auto-save-default nil)
 '(backup-inhibited t t)
 '(blink-cursor-mode nil)
 '(c-backslash-column 79)
 '(c-backslash-max-column 80)
 '(c-block-comment-prefix "* ")
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(comment-style (quote extra-line))
 '(dabbrev-check-all-buffers t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(echo-keystrokes 0.1)
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(fill-column 80)
 '(fill-indent-according-to-mode t t)
 '(fringe-mode 8 nil (fringe))
 '(global-linum-mode t)
 ;; '(haskell-check-command "ghc -fno-code")
 '(imenu-use-popup-menu t)
 '(indent-tabs-mode nil)
 '(inhibit-default-init t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message "")
 '(js-indent-level 2)
 '(linum-format (quote dynamic))
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(paredit-mode t)
 '(prelude-guru nil)
 '(python-indent-offset 4)
 '(require-final-newline (quote visit-save))
 '(save-place t nil (saveplace))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tab-always-indent (quote complete))
 '(tab-width 2)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(which-function-mode t)
 '(whitespace-line-column 80)
 '(whitespace-style (quote (face lines-tail trailing tabs empty))))

;; Some extra key-bindings
(global-set-key [f1] 'manual-entry)
(global-set-key [f2] 'info)
(global-set-key [f5] 'other-window)
(global-set-key [f6] (lambda () (interactive) (compile "make -j")))
(global-set-key [f7] (lambda () (interactive) (compile "make clean")))
(global-set-key [f8] 'next-error)
(global-set-key [f9] 'kill-compilation)
(global-set-key [f10] 'rgrep)
(global-set-key [f11] 'kill-some-buffers)
(global-set-key [f12] 'indent-whole-buffer)
(global-set-key [C-f12] 'imenu)

(defun indent-whole-buffer ()
  "Indent an entire buffer."
  (save-excursion
    (mark-whole-buffer)
    (indent-for-tab-command)))

(defun compilation-autoclose (buf str)
  "Automatically close compilation buffer BUF if contain the string STR."
  (cond ((string-match "finished" str)
         (run-with-timer 1 nil 'delete-window (get-buffer-window buf t)))))

(defun indent-certain-buffers ()
  "Indent buffers that have 'c-mode' enabled."
  (cond ((eq major-mode 'c-mode) (indent-whole-buffer))) nil)

;; On the fly spell checking
(defun enable-flyspell ()
  "Enables 'flyspell' in normal mode."
  (ispell-change-dictionary "american")
  (flyspell-mode))

;; On the fly spell checking for coding
(defun enable-flyspell-prog ()
  "Enables 'flyspell' in programming mode."
  (ispell-change-dictionary "american")
  (flyspell-prog-mode))

(setq compilation-finish-functions 'compilation-autoclose)
(setq write-file-functions 'indent-certain-buffers)

;; Enable coloring of trailing lines
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Enable on the fly spell checking for several modes
(add-hook 'text-mode-hook 'enable-flyspell)
(add-hook 'prog-mode-hook 'enable-flyspell-prog)

;; Clean trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; .PHONY: check-syntax
;; check-syntax:
;; $(CC) -fsyntax-only  $(CHK_SOURCES)
(add-hook 'c-mode-hook 'flymake-mode-on)

;; yasnippet
;; (add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-0.8.0/")
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; pcomplete shell
(add-hook 'shell-mode-hook 'pcomplete-shell-setup)

;; multi-major-mode
;; (add-to-list 'load-path "~/opt/share/emacs/site-lisp")
;; (require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)

;; mako mode
;; (require 'mmm-mako)
;; (load "~/opt/share/emacs/site-lisp/mmm-mako")
;; (add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
;; (mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako)

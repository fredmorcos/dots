(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-save t)
 '(TeX-master nil)
 '(TeX-parse-self t)
 '(auto-hscroll-mode (quote current-line))
 '(auto-save-file-name-transforms (\` ((".*" (\, emacs-autosaves-pattern) t))))
 '(auto-save-mode t)
 '(backup-directory-alist (\` ((".*" \, emacs-backups-pattern))))
 '(backup-inhibited nil t)
 '(blink-cursor-mode nil)
 '(coding-system-for-read (quote utf-8-unix) t)
 '(coding-system-for-write (quote utf-8-unix) t)
 '(colon-double-space t)
 '(column-number-mode t)
 '(comment-fill-column 70)
 '(company-minimum-prefix-length 1)
 '(company-tooltip-align-annotations t)
 '(counsel-mode t)
 '(custom-file "~/Workspace/dots/emacs/custom.el")
 '(default-justification (quote left))
 '(delete-old-versions t)
 '(ediff-split-window-function (function split-window-horizontally))
 '(ediff-window-setup-function (function ediff-setup-windows-plain))
 '(eldoc-echo-area-use-multiline-p t)
 '(fill-column 70)
 '(font-use-system-font t)
 '(frame-resize-pixelwise t)
 '(frame-title-format "%b - emacs" t)
 '(help-window-select t)
 '(history-delete-duplicates t)
 '(history-length 30)
 '(hscroll-margin 1)
 '(hscroll-step 1)
 '(indent-tabs-mode nil)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message nil)
 '(ivy--regex-ignore-order t)
 '(ivy-action-wrap t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-display-style (quote fancy))
 '(ivy-mode t)
 '(ivy-regex-ignore-order t)
 '(ivy-rich-mode t)
 '(ivy-rich-path-style (quote abbrev))
 '(ivy-rich-switch-buffer-align-virtual-buffer t)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(ivy-wrap t)
 '(load-prefer-newer t)
 '(lsp-ui-doc-border "orange red")
 '(lsp-ui-doc-include-signature t)
 '(make-backup-files t)
 '(menu-bar-mode nil)
 '(mode-require-final-newline (quote visit-save))
 '(org-cycle-separator-lines 0)
 '(org-indent-indentation-per-level 2)
 '(org-startup-folded t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-check-signature nil)
 '(package-selected-packages
   (quote
    (rmsbolt hledger-mode which-key flx avy ivy ivy-rich swiper counsel smex org-bullets f ht lsp-mode lsp-ui flycheck company company-lsp toml-mode markdown-mode json-mode yaml-mode gnuplot-mode rust-mode)))
 '(recentf-mode t)
 '(recentf-save-file emacs-recentf-file)
 '(require-final-newline (quote visit-save))
 '(rust-indent-method-chain t)
 '(rust-indent-offset 2)
 '(rust-indent-where-clause t)
 '(save-place-file emacs-places-file)
 '(save-place-mode t)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 4)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-paren-mode t)
 '(show-trailing-whitespace nil)
 '(size-indication-mode t)
 '(standard-indent 2)
 '(tab-width 2)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(url-privacy-level (quote high))
 '(url-proxy-services (quote (("no_proxy" . "127.0.0.1"))))
 '(vc-make-backup-files t)
 '(which-key-mode t)
 '(whitespace-action (quote (cleanup)))
 '(whitespace-style
   (quote
    (face tabs lines-tail trailing space-before-tab indentation empty space-after-tab))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "cornsilk"))))
 '(lsp-ui-doc-background ((t (:background "white smoke"))))
 '(lsp-ui-sideline-code-action ((t (:foreground "orange"))))
 '(lsp-ui-sideline-current-symbol ((t (:height 0.99 :weight ultra-bold :box (:line-width -1 :color "dim gray" :style nil) :foreground "dim gray"))))
 '(mode-line ((t (:box (:line-width -1 :color "grey75" :style nil) :foreground "gray20" :background "gray80"))))
 '(mode-line-highlight ((t (:box (:line-width 1 :color "grey40" :style nil)))))
 '(rust-question-mark-face ((t (:inherit (font-lock-builtin-face))))))

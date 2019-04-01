;;; custom -- custom emacs configs
;;; Code:
;;; Commentary:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-mode t)
 '(custom-file "~/Workspace/dots/emacs/custom.el")
 '(ivy--regex-ignore-order t)
 '(ivy-action-wrap t)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-display-style (quote fancy))
 '(ivy-initial-inputs-alist nil)
 '(ivy-mode t)
 '(ivy-regex-ignore-order t)
 '(ivy-rich-mode t)
 '(ivy-rich-path-style (quote abbrev))
 '(ivy-rich-switch-buffer-align-virtual-buffer t)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(ivy-wrap t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-check-signature nil)
 '(package-selected-packages
   (quote
    (use-package-ensure-system-package f ht diminish use-package bind-key which-key fzf flx avy ivy ivy-rich swiper counsel smex org-bullets git-gutter-fringe+ symbol-overlay multiple-cursors iedit magit vdiff vdiff-magit yasnippet yasnippet-snippets hledger-mode rmsbolt flycheck company company-tabnine company-box lsp-mode lsp-ui company-lsp toml-mode markdown-mode json-mode gnuplot-mode dockerfile-mode meson-mode yaml-mode flycheck-yamllint rust-mode lsp-java irony irony-eldoc flycheck-irony company-irony company-irony-c-headers cquery ccls z3-mode boogie-friends)))
 '(url-privacy-level (quote high))
 '(url-proxy-services (quote (("no_proxy" . "127.0.0.1"))))
 '(use-package-always-ensure t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'custom)
;;; custom.el ends here

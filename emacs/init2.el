(im/config "Recent Files"
 (im/after recentf
  (defun init/do-recentf-exclude (dir)
   (add-to-list 'recentf-exclude dir))
  (declare-function init/do-recentf-exclude 'init)
  (init/do-recentf-exclude (expand-file-name package-user-dir))
  (init/do-recentf-exclude (no-littering-expand-etc-file-name ""))
  (init/do-recentf-exclude (no-littering-expand-var-file-name ""))
  (init/do-recentf-exclude "/usr/share/emacs")
  (mapc 'init/do-recentf-exclude native-comp-eln-load-path)
  (setopt
   recentf-max-menu-items 50
   recentf-max-saved-items 100))
 (recentf-mode))

(im/config "Whitespace"
 (im/after whitespace
  (im/dim whitespace-mode "Ws")
  (setopt
   whitespace-line-column fill-column
   show-trailing-whitespace nil
   whitespace-action '(cleanup auto-cleanup)
   whitespace-style
   '(face tabs lines-tail empty tab-mark missing-newline-at-eof
     space-after-tab  space-after-tab::tab    space-after-tab::space
     space-before-tab space-before-tab::space space-before-tab::tab
     indentation      indentation::tab        indentation::space)))
 (im/hook make-mode-hook whitespace-mode)
 (im/hook emacs-lisp-mode-hook whitespace-mode)
 (im/hook hledger-mode-hook whitespace-mode))

(im/config "Emacs LISP"
 (im/after elisp-mode
  (setopt
   lisp-indent-offset 1
   lisp-indent-function 'common-lisp-indent-function))
 (im/pkg highlight-defined
  (im/hook emacs-lisp-mode-hook highlight-defined-mode))
 (im/pkg highlight-quoted
  (im/hook emacs-lisp-mode-hook highlight-quoted-mode))
 (im/pkg eros
  (im/hook emacs-lisp-mode-hook eros-mode))
 (im/pkg ipretty
  (ipretty-mode))
 (im/pkg suggest)

(im/config "Parenthesis Highlighting"
 (im/after paren
  (setopt
   show-paren-style 'mixed
   show-paren-highlight-openparen t
   show-paren-context-when-offscreen 'overlay
   show-paren-when-point-inside-paren t
   show-paren-when-point-in-periphery t))
 (im/hook prog-mode-hook show-paren-mode)
 (im/hook conf-desktop-mode-hook show-paren-mode))

(im/config "Spell Checking"
 (im/pkg jinx
  (im/after jinx
   (im/dim jinx-mode "Jx")
   (im/key-local "M-$" jinx-correct jinx-mode-map)
   (im/key-local "C-M-$" jinx-languages jinx-mode-map))
  (im/hook text-mode-hook jinx-mode)
  (im/hook prog-mode-hook jinx-mode)
  (im/hook conf-mode-hook jinx-mode)))

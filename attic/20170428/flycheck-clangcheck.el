;;; flycheck-clangcheck.el --- A Flycheck checker difinition using ClangCheck.
;;; Commentary:
;;; Code:

(require 's)                            ; for s-prepend
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'flycheck)

(flycheck-def-option-var flycheck-clangcheck-extra-arg nil c/c++-clangcheck
  "Additional argument to append to the compiler command line for ClangCheck.

The value of this variable is a list of strings, where each
string is an additional argument to pass to ClangCheck, via the
`-extra-arg-before' option."
  :type '(repeat (string :tag "Argument"))
  :safe #'flycheck-string-list-p)

(flycheck-define-checker c/c++-clangcheck
  "A C/C++ syntax checker using ClangCheck."
  :command ("clang-check"
            "-analyze"
	    (option-list "-extra-arg-before=" flycheck-clangcheck-extra-arg s-prepend)
            "--extra-arg=-Wno-unknown-warning-option" ; silence GCC options
            "--extra-arg=-Wno-null-character"         ; silence null
	    source-inplace
	    "--"
	    (eval (concat "-x" (cl-case major-mode (c++-mode "c++") (c-mode "c"))))
	    "-fno-color-diagnostics"
	    "-fno-caret-diagnostics"
	    "-fno-diagnostics-show-option")
  :error-patterns
  ((info line-start (file-name) ":" line ":" column
	 ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
	    ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
	  ": " (or "fatal error" "error") ": " (message) line-end))
  :modes (c-mode c++-mode))

;;;###autoload
(defun flycheck-clangcheck-setup ()
  "Setup Flycheck ClangCheck."
  (interactive)
  (add-to-list 'flycheck-checkers 'c/c++-clangcheck))

(provide 'flycheck-clangcheck)

;;; flycheck-clangcheck.el ends here

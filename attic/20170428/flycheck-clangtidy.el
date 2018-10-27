;;; flycheck-clangtidy.el --- A Flycheck checker difinition using ClangTidy.
;;; Commentary:
;;; Code:

(require 's)                            ; for s-prepend
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'flycheck)

(flycheck-def-option-var flycheck-clangtidy-extra-arg nil c/c++-clangtidy
  "Additional argument to append to the compiler command line for ClangTidy.

The value of this variable is a list of strings, where each
string is an additional argument to pass to ClangTidy, via the
`-extra-arg-before' option."
  :type '(repeat (string :tag "Argument"))
  :safe #'flycheck-string-list-p)

(flycheck-define-checker c/c++-clangtidy
  "A C/C++ syntax checker using ClangTidy."
  :command ("clang-tidy"
            (eval (concat "-checks"
                          "=boost-*"
                          ",cert-*"
                          ",cppcoreguidelines-*"
                          ",clang-analyzer-*"
                          ",google-*"
                          ",-google-readability-*"
                          ",llvm-*"
                          ",misc-*"
                          ",modernize-*"
                          ",performance-*"
                          ",readability-*"
                          ",-readability-avoid-const-*"
                          ",-readability-braces-*"))
	    (option-list "-extra-arg-before=" flycheck-clangtidy-extra-arg s-prepend)
            "--extra-arg=-Wno-unknown-warning-option" ; silence GCC options
            "--extra-arg=-Wno-null-character"         ; silence null
	    source-inplace
	    "--"
	    (eval (concat "-x" (cl-case major-mode (c++-mode "c++") (c-mode "c"))))
            "-fno-color-diagnostics"
            "-fno-caret-diagnostics"
	    "-fno-diagnostics-show-option")
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": error: "
          (message (one-or-more not-newline) "\n"
                   (one-or-more not-newline) "\n"
                   (one-or-more not-newline) "\n"
                   (one-or-more not-newline))
          line-end)
   (warning line-start (file-name) ":" line ":" column ": warning: "
            (message (one-or-more not-newline) "\n"
                     (one-or-more not-newline) "\n"
                     (one-or-more not-newline) "\n"
                     (one-or-more not-newline))
            line-end)
   (info line-start (file-name) ":" line ":" column ": note: "
         (message (one-or-more not-newline) "\n"
                  (one-or-more not-newline) "\n"
                  (one-or-more not-newline) "\n"
                  (one-or-more not-newline))
         line-end))
  :modes (c-mode c++-mode))

;;;###autoload
(defun flycheck-clangtidy-setup ()
  "Setup Flycheck ClangTidy."
  (interactive)
  (add-to-list 'flycheck-checkers 'c/c++-clangtidy))

(provide 'flycheck-clangtidy)

;;; flycheck-clangtidy.el ends here

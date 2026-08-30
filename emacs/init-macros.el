;;; package --- Init macros -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Configuration Top-Level

(defvar *init-benchmark* :disabled)

(defmacro config (name &rest body)
 "Create a config section with NAME and BODY."
 `(progn
   ,(unless (eq *init-benchmark* :disabled)
     `(setq start-time (current-time)))
   ,@body
   ,(unless (eq *init-benchmark* :disabled)
     `(let* ((end-time (current-time))
             (total-time (time-subtract end-time start-time))
             (total-time-sec (time-to-seconds total-time)))
       (message "[Init-Benchmark] %s took %.2f seconds" ,name total-time-sec)))))

;; Declarations

(defmacro declvar (var)
 "Declare the variable VAR."
 `(eval-when-compile (defvar ,var)))

(defmacro declfun (func pkg)
 "Declare FUNC from PKG."
 `(eval-when-compile (declare-function ,func ,pkg)))

;;; Lazy Loading

(defalias 'after #'with-eval-after-load)

;; Modes.

(defun mode (ext mode)
 "Autoload and enable MODE for file extension EXT."
 (push `(,ext . ,mode) auto-mode-alist))

;;; Packages

(defun package (pkg)
 "Install PKG and add it to list of selected packages."
 (unless (package-installed-p pkg)
  (declvar package-archive-contents)
  (unless package-archive-contents
   (message "+++ Refreshing package repositories")
   (package-refresh-contents t))
  (message "+++ Installing %s..." pkg)
  (package-install pkg))
 (after 'package
  (declvar package-selected-packages)
  (add-to-list 'package-selected-packages pkg)))

(provide 'init-macros)
;;; init-macros.el ends here

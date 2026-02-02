;;; package --- Init macros -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Configuration Top-Level

(defmacro config (_name &rest body)
 "Create a config section with NAME and BODY."
 `(progn ,@body))

;;; Faces.

(defmacro face (face &rest props)
 "Set FACE properties to PROPS."
 `(custom-set-faces '(,face ((t ,@props)))))

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

(defmacro packages (&rest pkgs)
 "Install PKGS."
 `(progn
   ,@(mapcar #'(lambda (pkg)
                `(package-setup ,pkg))
      pkgs)))

(defun package-setup (pkg)
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

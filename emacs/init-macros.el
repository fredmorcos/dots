;;; package --- Init macros -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Configuration Top-Level

(defmacro config (_name &rest body)
 "Create a config section with NAME and BODY."
 `(progn ,@body))

;;; Customization

(defmacro custom (pkg &rest opts)
 "Set options OPTS after PKG is loaded."
 `(after ,pkg (setopt ,@opts)))

;;; Hooks

(defmacro hook (hook-var hook-pkg func func-pkg &optional depth local)
 "Add a hook FUNC of FUNC-PKG to HOOK-VAR in HOOK-PKG."
 `(progn
   (eval-and-compile
    (autoload ,func (symbol-name ,func-pkg)))
   (after ,hook-pkg
    (add-hook ,hook-var ,func ,depth ,local))))

;;; Faces.

(defmacro face (face &rest props)
 "Set FACE properties to PROPS."
 `(custom-set-faces '(,face ((t ,@props)))))

;; Declarations

(defmacro declvars (&rest vars)
 "Declare the variables VARS."
 `(eval-when-compile
   ,@(mapcar #'(lambda (var)
                `(defvar ,var))
      vars)))

(defmacro declfunc (func pkg)
 "Declare FUNC from PKG."
 `(eval-when-compile
   (declare-function ,func ,pkg)))

;;; Lazy Loading

(defalias 'after #'with-eval-after-load)

(defmacro autoloads (pkg int &rest funcs)
 "Create autoloads for INT (interactive) FUNCS from PKG."
 `(progn
   ,@(mapcar #'(lambda (func)
                `(progn
                  (autoload ,func (symbol-name ,pkg) nil ,int)
                  (declfunc ,func ,pkg)))
      funcs)))

;;; Packages

(defmacro packages (&rest pkgs)
 "Install PKGS."
 `(progn
   ,@(mapcar #'(lambda (pkg)
                `(package-setup ,pkg))
      pkgs)))

(defun package-setup (pkg)
 "Install PKG and add it to list of selected packages."
 (defvar *init/packages-refreshed* nil)
 (when (not (package-installed-p pkg))
  (when (not *init/packages-refreshed*)
   (message "+++ Refreshing package repositories")
   (package-refresh-contents)
   (setq *init/packages-refreshed* t))
  (message "+++ Installing %s..." pkg)
  (package-install pkg))
 (after 'package
  (declvars package-selected-packages)
  (add-to-list 'package-selected-packages pkg)))

;; Hooks.
(defmacro hook-globals (hook &rest funcs)
 "Hook FUNCS into HOOK."
 `(progn
   ,@(mapcar #'(lambda (func)
                `(add-hook ,hook ,func))
      funcs)))

(defmacro hook-local (hook func)
 "Hook FUNC into HOOK locally."
 `(add-hook ,hook ,func nil t))

(defmacro hook-progn (hook &rest body)
 "Hook (lambda () BODY) to HOOK."
 `(add-hook ,hook (lambda () (progn ,@body))))

;; Bind keys.

(defmacro im/key (key func &optional pkg)
 "Define KEY in PKG-KEYMAP to call FUNC from PKG."
 `(progn
   (im/autoload ,func ,pkg)
   (global-set-key (kbd ,key) #',func)))

(defmacro im/key-local (key func keymap &optional pkg)
 "Define KEY in KEYMAP to call FUNC from PKG."
 `(progn
   (im/autoload ,func ,pkg)
   (eval-when-compile (defvar ,keymap))
   (define-key ,keymap (kbd ,key) #',func)))

(defmacro im/key-interactive (key &rest body)
 "Define KEY to call an interactive lambda with BODY."
 `(global-set-key (kbd ,key) (lambda () (interactive) ,@body)))

(defmacro im/key-disable (key keymap)
 "Disable a KEY binding from KEYMAP."
 `(progn
   (eval-when-compile (defvar ,keymap))
   (define-key ,keymap (kbd ,key) nil)))

(defmacro im/key-remap (old-func new-func)
 "Remap a global key from OLD-FUNC to NEW-FUNC."
 `(progn
   (global-set-key [remap ,old-func] #',new-func)))

;; Diminish.

(defun im/dim-helper (mode text)
 "Diminish MODE to TEXT helper."
 (let ((element (assoc mode minor-mode-alist))
       (new-text (if text (concat " " text) nil)))
  (if element
   (setf (nth 1 element) new-text)
   (push `(,mode ,new-text) minor-mode-alist))))

(defmacro im/dim (mode &optional text)
 "Diminish MODE to TEXT or nothing."
 `(im/dim-helper ',mode ,text))

;; Modes.

(defmacro im/mode (ext mode &optional pkg)
 "Autoload and enable MODE from PKG for file extension EXT."
 `(progn
   (im/autoload ,mode ,pkg)
   (push '(,(concat "\\" ext "\\'") . ,mode) auto-mode-alist)))

(provide 'init-macros)
;;; init-macros.el ends here

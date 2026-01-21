;;; package --- Init macros -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Configuration Top-Level

(defmacro config (_name &rest body)
  "Create a config section with NAME and BODY."
  `(progn ,@body))

(defmacro builtin (_name &rest body)
  "Create a section for a builtin package NAME and run BODY."
  `(progn ,@body))

;;; Lazy Loading

(defalias 'after #'with-eval-after-load)

(defmacro autoloads (pkg &rest funcs)
  "Create autoloads for FUNCS from PKG."
  `(eval-and-compile
     ,@(mapcar #'(lambda (func)
                   `(progn
                      (autoload ,func ,pkg)
                      (declare-function ,func ,pkg)))
         funcs)))

;; Variables

(defmacro declvars (&rest vars)
  "Declare the variables VARS."
  `(eval-when-compile
     ,@(mapcar #'(lambda (var)
                   `(defvar ,var))
         vars)))

;;; Packages

(defmacro package (pkg &rest body)
  "Install PKG and run BODY."
  `(progn
     (package-setup ,pkg)
     ,@body))

(defun package-setup (pkg)
  "Install PKG and add it to list of selected packages."
  (after 'package
    (declvars package-selected-packages)
    (push pkg package-selected-packages))
  (defvar *init/packages-refreshed* nil)
  (when (not (package-installed-p pkg))
    (when (not *init/packages-refreshed*)
      (message "+++ Refreshing package repositories")
      (package-refresh-contents)
      (setq *init/packages-refreshed* t))
    (message "+++ Installing %s..." pkg)
    (package-install pkg)))

;;; Faces.

(defmacro face (face &rest props)
  "Set FACE properties to PROPS."
  `(custom-set-faces '(,face ((t ,@props)))))

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

;; Popup buffers.

(defmacro im/disable-popup (regexp)
  "Stop buffers that match REGEXP from popping up."
  `(push (cons ,regexp (cons #'display-buffer-no-window nil)) display-buffer-alist))

;; Modes.

(defmacro im/mode (ext mode &optional pkg)
  "Autoload and enable MODE from PKG for file extension EXT."
  `(progn
     (im/autoload ,mode ,pkg)
     (push '(,(concat "\\" ext "\\'") . ,mode) auto-mode-alist)))

(provide 'init-macros)
;;; init-macros.el ends here

;;; package --- Init macros -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Top-Level Configuration.
(defmacro im/config (_name &rest body)
 "Define a toplevel configuration called NAME and execute BODY."
 `(progn ,@body))

;; Helpers.
(defmacro im/autoload (func pkg)
 "Create an autoload for FUNC from PKG."
 (when (stringp pkg)
  `(eval-when-compile
    (autoload ',func ,pkg)
    (declare-function ,func ,pkg))))

;; Hooks.
(defmacro im/hook (hook func &optional pkg local)
 "Autoload FUNC from PKG and add it to LOCAL HOOK."
 `(progn
   (im/autoload ,func ,pkg)
   ,(if local
     `(add-hook ',hook #',func 10 t)
     `(add-hook ',hook #',func))))

(defmacro im/hookn (hook &rest body)
 "Hook (lambda () BODY) to HOOK."
 `(add-hook ',hook (lambda () (progn ,@body))))

;; Bind keys.
(defmacro im/key (key func &optional pkg)
 "Define KEY in PKG-KEYMAP to call FUNC from PKG."
 `(progn
   (im/autoload ,func ,pkg)
   (global-set-key (kbd ,key) #',func)))

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

(defmacro im/key-local (key func keymap &optional pkg)
 "Define KEY in KEYMAP to call FUNC from PKG."
 `(progn
   (im/autoload ,func ,pkg)
   (eval-when-compile (defvar ,keymap))
   (define-key ,keymap (kbd ,key) #',func)))

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

;; Faces.
(defmacro im/face (face &rest props)
 "Set FACE properties to PROPS."
 `(custom-set-faces '(,face ((t ,@props)))))

;; Lazy loading.
(defmacro im/after (pkg &rest body)
 "Execute BODY when PKG is loaded."
 `(with-eval-after-load ',pkg ,@body))

;; Modes.
(defmacro im/mode (ext mode &optional pkg)
 "Autoload and enable MODE from PKG for file extension EXT."
 `(progn
   (im/autoload ,mode ,pkg)
   (push '(,(concat "\\" ext "\\'") . ,mode) auto-mode-alist)))

;; Packages.
(defmacro im/pkg (pkg &rest body)
 "Install PKG if not already installed and execute BODY."
 `(progn
   (defvar im/packages-refreshed nil)
   (autoload 'package-installed-p "package")
   (when (not (package-installed-p ',pkg))
    (when (not im/packages-refreshed)
     (message "+++ Refreshing package repositories")
     (package-refresh-contents)
     (setq im/packages-refreshed t))
    (message "+++ Installing %s..." ',pkg)
    (package-install ',pkg))
   (defvar package-selected-packages)
   (push ',pkg package-selected-packages)
   ,@body))

(provide 'init-macros)
;;; init-macros.el ends here

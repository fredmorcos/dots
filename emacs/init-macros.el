;;; package --- Init macros -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Helpers.
(defmacro fm/autoload (func pkg)
 "Create an autoload for FUNC from PKG."
 (when (stringp pkg)
  `(eval-when-compile
    (autoload ',func ,pkg)
    (declare-function ,func ,pkg))))

;; Hooks.
(defmacro fm/hook (hook func &optional pkg local)
 "Autoload FUNC from PKG and add it to LOCAL HOOK."
 `(progn
   (fm/autoload ,func ,pkg)
   ,(if local
     `(add-hook ',hook #',func 10 t)
     `(add-hook ',hook #',func))))

(defmacro fm/hookn (hook &rest body)
 "Hook (lambda () BODY) to HOOK."
 `(add-hook ',hook (lambda () (progn ,@body))))

;; Bind keys.
(defmacro fm/key (key func &optional pkg)
 "Define KEY in PKG-KEYMAP to call FUNC from PKG."
 `(progn
   (fm/autoload ,func ,pkg)
   (global-set-key (kbd ,key) #',func)))

(defmacro fm/key-interactive (key &rest body)
 "Define KEY to call an interactive lambda with BODY."
 `(global-set-key (kbd ,key) (lambda () (interactive) ,@body)))

(defmacro fm/key-disable (key keymap)
 "Disable a KEY binding from KEYMAP."
 `(progn
   (eval-when-compile (defvar ,keymap))
   (define-key ,keymap (kbd ,key) nil)))

(defmacro fm/key-remap (old-func new-func)
 "Remap a global key from OLD-FUNC to NEW-FUNC."
 `(progn
   (global-set-key [remap ,old-func] #',new-func)))

(defmacro fm/key-local (key func keymap &optional pkg)
 "Define KEY in KEYMAP to call FUNC from PKG."
 `(progn
   (fm/autoload ,func ,pkg)
   (eval-when-compile (defvar ,keymap))
   (define-key ,keymap (kbd ,key) #',func)))

;; Diminish.
(defun fm/dim-helper (mode text)
 "Diminish MODE to TEXT helper."
 (let ((element (assoc mode minor-mode-alist))
       (new-text (if text (concat " " text) nil)))
  (if element
   (setf (nth 1 element) new-text)
   (push `(,mode ,new-text) minor-mode-alist))))

(defmacro fm/dim (mode &optional text)
 "Diminish MODE to TEXT or nothing."
 `(fm/dim-helper ',mode ,text))

;; Popup buffers.
(defmacro fm/disable-popup (regexp)
 "Stop buffers that match REGEXP from popping up."
 `(push (cons ,regexp (cons #'display-buffer-no-window nil)) display-buffer-alist))

;; Faces.
(defmacro fm/face (face &rest props)
 "Set FACE properties to PROPS."
 `(custom-set-faces '(,face ((t ,@props)))))

;; Lazy loading.
(defmacro fm/after (pkg &rest body)
 "Execute BODY when PKG is loaded."
 `(with-eval-after-load ',pkg ,@body))

;; Modes.
(defmacro fm/mode (ext mode &optional pkg)
 "Autoload and enable MODE from PKG for file extension EXT."
 `(progn
   (fm/autoload ,mode ,pkg)
   (push '(,(concat "\\" ext "\\'") . ,mode) auto-mode-alist)))

;; Packages.
(defmacro fm/pkg (pkg &rest body)
 "Install PKG if not already installed and execute BODY."
 `(progn
   (defvar packages-refreshed nil)
   (autoload 'package-installed-p "package")
   (when (not (package-installed-p ',pkg))
    (when (not packages-refreshed)
     (message "+++ Refreshing package repositories to install %s" ',pkg)
     (package-refresh-contents)
     (setq packages-refreshed t))
    (message "+++ Installing %s..." ',pkg)
    (package-install ',pkg)
    (push ',pkg package-selected-packages))
   ,@body))

(provide 'init-macros)
;;; init-macros.el ends here

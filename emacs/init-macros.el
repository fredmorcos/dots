;;; package --- Init macros -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; hooks
(defmacro fm/hook (hook func &optional pkg local)
 "Autoload FUNC from PKG and add it to LOCAL HOOK."
 `(progn
   ,(when pkg `(autoload ',func ,pkg))
   ,(when pkg `(declare-function ,func ,pkg))
   ,(if local
     `(add-hook ',hook #',func 10 t)
     `(add-hook ',hook #',func))))

(defmacro fm/hooks (hooks func &optional pkg local)
 "Autoload FUNC from PKG and add it to LOCAL HOOKS."
 `(progn
   ,(when pkg `(autoload ',func ,pkg))
   ,(when pkg `(declare-function ,func ,pkg))
   ,@(let ((exps nil))
      (progn (if local
              (while hooks
               (push `(add-hook ',(pop hooks) #',func 10 t) exps)
               exps)
              (while hooks
               (push `(add-hook ',(pop hooks) #',func) exps)
               exps))
       exps))))

(defmacro fm/hookn (hook &rest body)
 "Hook (lambda () BODY) to HOOK."
 `(add-hook ',hook (lambda () (progn ,@body)) 10))

(defmacro fm/hookn-interactive (hook &rest body)
 "Hook (lambda () BODY) to HOOK."
 `(add-hook ',hook (lambda () (interactive) (progn ,@body)) 10))

;; bind keys
(defmacro fm/key (key func &optional pkg-keymap pkg)
 "Define KEY in PKG-KEYMAP to call FUNC from PKG."
 (cond
  ((not pkg-keymap)
   `(progn
     ,(when (and func pkg)
       `(autoload ',func ,pkg)
       `(declare-function ,func ,pkg))
     (global-set-key (kbd ,key) ,(if func `#',func nil))))
  (t
   `(progn
     (eval-when-compile (defvar ,pkg-keymap))
     ,(when (and func pkg)
       `(autoload ',func ,pkg)
       `(declare-function ,func ,pkg))
     (define-key ,pkg-keymap (kbd ,key) ,(if func `#',func nil))))))

;; diminish
(defun fm/dim-helper (mode text)
 "Diminish MODE to TEXT helper."
 (let ((element (seq-find (lambda (x) (eq (car x) mode)) minor-mode-alist))
       (new-text (if text (concat " " text) "")))
  (if element
   (setf (nth 1 element) new-text)
   (push `(,mode ,new-text) minor-mode-alist))))

(defmacro fm/dim (mode &optional text enforce)
 "Diminish MODE to TEXT, this happens on mode hook unless ENFORCE is set."
 (let ((hook (intern (concat (symbol-name mode) "-hook"))))
  (if enforce
   `(fm/dim-helper ',mode ,text)
   `(fm/hookn ,hook (fm/dim-helper ',mode ,text)))))

;; faces
(defmacro fm/face (face &rest props)
 "Set FACE properties to PROPS."
 `(if (facep ',face)
   (custom-set-faces '(,face ((t ,@props))))
   (if (stringp (car ',props))
    (fm/var ,face (car ',props))
    (fm/var ,face '((t ,@props))))))

;; vars
(defmacro fm/vars (&rest customs)
 "Custom-Set the CUSTOMS list of var-val pairs."
 `(custom-set-variables
   ,@(let ((exps nil))
     (while customs
      (push `(quote (,(pop customs) ,(pop customs))) exps))
     exps)))

(defmacro fm/var (var val)
 "Custom-Set VAR to VAL."
 `(custom-set-variables '(,var ,val)))

;; lazy loading
(defmacro fm/after (pkg &rest body)
 "Execute BODY when PKG is loaded."
 `(with-eval-after-load ',pkg ,@body))

;; modes
(defmacro fm/mode (ext mode &optional pkg)
 "Autoload and enable MODE from PKG for file extension EXT."
 `(progn
   ,(when pkg `(autoload ',mode ,pkg))
   (push '(,(concat "\\" ext "\\'") . ,mode) auto-mode-alist)))

(defmacro fm/pkg (pkg &rest body)
 "Install PKG if not already installed and execute BODY."
 `(progn
   (defvar packages-refreshed nil)
   (if (not (package-installed-p ',pkg))
    (progn
     (when (not packages-refreshed)
      (progn
       (message "Refreshing package repositories...")
       (package-refresh-contents)
       (setq packages-refreshed t)))
     (package-install ',pkg))
    (push ',pkg package-selected-packages))
   (progn ,@body)))

(provide 'init-macros)
;;; init-macros.el ends here

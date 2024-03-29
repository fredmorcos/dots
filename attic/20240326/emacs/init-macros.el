;;; package --- Init macros -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defmacro im/config (name &rest configuration)
 "Grouping macro for package NAME with CONFIGURATION."
 (let ((customizations)
       (eval-after-loads)
       (inits)
       (functions)
       (do-require)
       (do-debug)
       (package-name)
       (autoloads)
       (commands))
  (while configuration
   (pcase (car configuration)
    (:custom
     (pop configuration)
     (while (not (symbolp (car configuration)))
      ;; (message "%s is not a keyword" (car configuration))
      (let* ((customization (pop configuration))
             (transformed-customization
              `(,(nth 0 customization)    ; Name
                ,(nth 1 customization)    ; Value
                nil
                nil
                ,(nth 2 customization)))) ; Note
       ;; Customizations need to be quoted.
       (push `',transformed-customization customizations))))
    (:after
     (pop configuration)
     (while (not (symbolp (car configuration)))
      (setf eval-after-loads (append eval-after-loads `(,(pop configuration))))))
    (:init
     (pop configuration)
     (while (not (symbolp (car configuration)))
      (push (pop configuration) inits)))
    (:require
     (pop configuration)
     (setq do-require t))
    (:debug
     (pop configuration)
     (setq do-debug t))
    (:functions
     (pop configuration)
     (while (not (symbolp (car configuration)))
      (push (pop configuration) functions)))
    (:package
     (pop configuration)
     (setq package-name
      (if (stringp (car configuration))
       (intern (pop configuration))
       name)))
    (:autoloads
     (pop configuration)
     (while (not (keywordp (car configuration)))
      (let* ((func-autoload (pop configuration))
             (autoload-call
              `(eval-when-compile
                (autoload ',func-autoload ,(symbol-name name))
                (declare-function ',func-autoload ',name))))
       (push `',autoload-call autoloads))))
    (:commands
     (pop configuration)
     (while (not (keywordp (car configuration)))
      (let* ((cmd-autoload (pop configuration))
             (command-call
              `(eval-when-compile
                (autoload ',cmd-autoload ,(symbol-name name) nil t)
                (declare-function ',cmd-autoload ',name))))
       (push `',command-call commands))))
    (_
     (pop configuration)
     (while (not (symbolp (car configuration)))
      (pop configuration)))))
  `(progn
    ,(when package-name
      `(progn
        ,(when do-debug
          `(message "+++ Running the :package section for %s" ,(symbol-name name)))
        (progn
         (defvar im/packages-refreshed nil
          "Whether package-refresh-contents has been called")
         (autoload 'package-installed-p "package")
         (when (not (package-installed-p ',package-name))
          (when (not im/packages-refreshed)
           (message "+++ Refreshing package repositories")
           (package-refresh-contents)
           (setq im/packages-refreshed t))
          (message "+++ Installing package %s" ,(symbol-name package-name))
          (package-install ',package-name))
         (push ',package-name package-selected-packages))))
    ,(when autoloads
      `(progn
        ,(when do-debug
          `(message "+++ Running the :autoloads section for %s" ,(symbol-name name)))
        ,@(mapcar (lambda (current-autoload) (eval current-autoload)) autoloads)))
    ,(when commands
      `(progn
        ,(when do-debug
          `(message "+++ Running the :commands section for %s" ,(symbol-name name)))
        ,@(mapcar (lambda (current-command) (eval current-command)) commands)))
    ,(when functions
      `(progn
        ,(when do-debug
          `(message "+++ Running the :functions section for %s" ,(symbol-name name)))
        ,@functions))
    ,(when inits
      `(progn
        ,(when do-debug
          `(message "+++ Running the :init section for %s" ,(symbol-name name)))
        ,@inits))
    ,(when do-require
      `(progn
        ,(when do-debug
          `(message "+++ Running the :require section for %s" ,(symbol-name name)))
        (require ',name)))
    ,(when customizations
      `(progn
        ,(when do-debug
          `(message "+++ Running the :custom section for %s" ,(symbol-name name)))
        (custom-theme-set-variables 'user ,@customizations)))
    ,(when eval-after-loads
      `(progn
        ,(when do-debug
          `(message "+++ Running the :after section for %s" ,(symbol-name name)))
        (with-eval-after-load ',name ,@eval-after-loads))))))

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

;; Modes.
(defmacro fm/mode (ext mode &optional pkg)
 "Autoload and enable MODE from PKG for file extension EXT."
 `(progn
   (fm/autoload ,mode ,pkg)
   (push '(,(concat "\\" ext "\\'") . ,mode) auto-mode-alist)))

(provide 'init-macros)
;;; init-macros.el ends here

;;; package --- QoL improvements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun im/replace-escapes ()
 "Replace strange newline escapes with proper UNIX newlines."
 (interactive)
 (goto-char (point-min))
 (while (search-forward "\\n" nil t) (replace-match (char-to-string ?\n) nil t))
 (goto-char (point-min))
 (while (search-forward "\\t" nil t) (replace-match (char-to-string ?\t) nil t))
 (goto-char (point-min))
 (while (search-forward "" nil t) (replace-match "" nil t)))

(defun im/move-line-up ()
 "Move a line up."
 (interactive)
 (transpose-lines 1)
 (forward-line -2))

(defun im/move-line-down ()
 "Move a line down."
 (interactive)
 (forward-line 1)
 (transpose-lines 1)
 (forward-line -1))

(defun im/insert-pair (left right &optional region-only)
 "Insert LEFT & RIGHT in text, or around it if REGION-ONLY is t."
 (if (use-region-p)
  (let ((begin (region-beginning))
        (end (region-end)))
   (progn
    (goto-char begin)
    (insert-char left)
    (goto-char (+ 1 end))
    (insert-char right)))
  (progn
   (insert-char left)
   (when (not region-only)
    (progn
     (insert-char right)
     (backward-char))))))

(defun im/insert-pair-curly ()
 "Insert curly braces in or around text."
 (interactive)
 (im/insert-pair ?\{ ?\} nil))

(defun im/insert-pair-parens ()
 "Insert parenthesis in or around text."
 (interactive)
 (im/insert-pair ?\( ?\) t))

(defun im/insert-pair-quote ()
 "Insert single quotes in or around text."
 (interactive)
 (im/insert-pair ?\' ?\' t))

(defun im/insert-pair-double-quotes ()
 "Insert double quotes in or around text."
 (interactive)
 (im/insert-pair ?\" ?\" t))

(defun im/insert-pair-backtick ()
 "Insert back-ticks in or around text."
 (interactive)
 (im/insert-pair ?\` ?\` t))

(defun im/generate-password ()
 "Generate a password and insert it."
 (interactive)
 (shell-command "pwgen -c -n -y -s -B -1 34 1" (current-buffer)))

(defun im/insert-buffer-name ()
 "Insert the buffer's filename."
 (interactive)
 (insert (buffer-name)))

(defmacro im/append (list element)
 "Append ELEMENT to LIST."
 `(progn
   (eval-when-compile
    (autoload '-insert-at "dash")
    (autoload '-contains-p "dash"))
   (when (not (-contains-p ,list ,element))
    (setf ,list (-insert-at (length ,list) ,element ,list)))))

(provide 'qol)
;;; qol.el ends here

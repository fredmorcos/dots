;;; package --- QoL improvements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun qol/replace-escapes ()
 "Replace strange newline escapes with proper UNIX newlines."
 (interactive)
 (goto-char (point-min))
 (while (search-forward "\\n" nil t) (replace-match (char-to-string ?\n) nil t))
 (goto-char (point-min))
 (while (search-forward "\\t" nil t) (replace-match (char-to-string ?\t) nil t))
 (goto-char (point-min))
 (while (search-forward "" nil t) (replace-match "" nil t)))

(defun qol/insert-pair (left right &optional region-only)
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

(defun qol/insert-pair-curly ()
 "Insert curly braces in or around text."
 (interactive)
 (qol/insert-pair ?\{ ?\} nil))

(defun qol/insert-pair-parens ()
 "Insert parenthesis in or around text."
 (interactive)
 (qol/insert-pair ?\( ?\) t))

(defun qol/insert-pair-quote ()
 "Insert single quotes in or around text."
 (interactive)
 (qol/insert-pair ?\' ?\' t))

(defun qol/insert-pair-double-quotes ()
 "Insert double quotes in or around text."
 (interactive)
 (qol/insert-pair ?\" ?\" t))

(defun qol/insert-pair-backtick ()
 "Insert back-ticks in or around text."
 (interactive)
 (qol/insert-pair ?\` ?\` t))

(defun qol/generate-password ()
 "Generate a password and insert it."
 (interactive)
 (shell-command "pwgen -c -n -y -s -B -1 34 1" (current-buffer)))

(defun qol/insert-buffer-name ()
 "Insert the buffer's filename."
 (interactive)
 (insert (buffer-name)))

(provide 'qol)
;;; qol.el ends here

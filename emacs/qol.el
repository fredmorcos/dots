;;; package --- QoL improvements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun fm/replace-escapes ()
 "Replace strange newline escapes with proper UNIX newlines."
 (interactive)
 (goto-char (point-min))
 (while (search-forward "\\n" nil t) (replace-match (char-to-string ?\n) nil t))
 (goto-char (point-min))
 (while (search-forward "\\t" nil t) (replace-match (char-to-string ?\t) nil t))
 (goto-char (point-min))
 (while (search-forward "

(defun fm/move-line-up ()
 "Move a line up."
 (interactive)
 (transpose-lines 1)
 (forward-line -2))

(defun fm/move-line-down ()
 "Move a line down."
 (interactive)
 (forward-line 1)
 (transpose-lines 1)
 (forward-line -1))

(defun fm/generate-password ()
 "Generate a password and insert it."
 (interactive)
 (shell-command "pwgen -c -n -y -s -B -1 34 1" (current-buffer)))

(defun fm/insert-pair (left right &optional region-only)
 "Insert LEFT & RIGHT in or around text if REGION-ONLY is t."
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

(provide 'qol)
;;; qol.el ends here
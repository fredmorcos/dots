;;; package -- Summary
;;; Commentary:
;;; Code:

;; -*- lexical-binding: t; -*-

(defvar pet-mode-map (make-sparse-keymap) "Keymap for PET mode.")

(defun pet-tabulate ()
  "Tabulate an expense in PET mode."
  (interactive)
  (move-beginning-of-line nil)
  (search-forward "201")
  (left-word)
  (indent-for-tab-command)
  (right-word)
  (right-word)
  (right-word)
  (right-word)
  (delete-char 1)
  (indent-for-tab-command)
  (right-word)
  (delete-char 1)
  (indent-for-tab-command)
  (next-line)
  (move-beginning-of-line nil))

(defun pet-replicate ()
  "Replicate an expense in PET mode."
  (interactive)
  (previous-line)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (kill-ring-save nil nil t)
  (move-end-of-line nil)
  (newline)
  (yank)
  (move-beginning-of-line nil)
  (search-forward "201")
  (left-word)
  (kill-word 3)
  (pet-insert-date)
  (move-end-of-line nil)
  (next-line))

(defun pet-insert-date ()
  "Insert current date into buffer."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(define-key pet-mode-map (kbd "C-c C-c") #'pet-tabulate)
(define-key pet-mode-map [f2] #'pet-insert-date)
(define-key pet-mode-map [f5] #'pet-replicate)

(toggle-truncate-lines)

;;;###autoload
(define-minor-mode pet-mode
  "Minor mode for PET files."
  :init-value nil
  :lighter " PET"
  :keymap pet-mode-map
  :global nil)

(provide 'pet-mode)

;;; pet-mode ends here

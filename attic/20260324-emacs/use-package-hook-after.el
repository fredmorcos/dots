;;; use-package-hook-after.el --- :hook-after keyword for use-package  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Name <name@place.com>,
;; Maintainer: Name <name@place.com>,
;; URL: https://nameplace.com
;; Version: 0.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, use-package

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Implement a :hook-after keyword for `use-package'.

;;; Code:

(require 'use-package-core)

;;;###autoload
(defun use-package-normalize/:hook-after (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      ;; NOTE 2025-08-18: This should do more work to ensure the data
      ;; has the corrent form.
      (unless (listp arg)
        (use-package-error (concat label " must be a cons cell with a symbol as car")))
      arg)))

;;;###autoload
(defun use-package-handler/:hook-after (name _keyword args rest state)
  (append (use-package-process-keywords name rest state)
          (mapcar
           (lambda (configuration-data)
             (pcase-let ((`(,feature . ,hooks) configuration-data))
               `(with-eval-after-load ',feature
                  ,@(if (consp (car-safe hooks))
                        (mapcar
                         (lambda (hook-data)
                           (pcase-let ((`(,hook . ,function) hook-data))
                             `(add-hook ',hook #',function)))
                         hooks)
                      (pcase-let ((`(,hook . ,function) hooks))
                        (list `(add-hook ',hook #',function)))))))
           args)))

(defvar use-package-keywords)

;; NOTE 2025-08-18: The `use-package-keywords' documentation warns
;; that the order of keywords is important.  I thus put the
;; :hook-after before :hook, but did not test it thorougly.
;;
;; (add-to-list 'use-package-keywords :hook-after :append)

(unless (memq :hook-after use-package-keywords)
  (pcase-let* ((position-of-hook (seq-position use-package-keywords :hook))
               (`(,pre-hook-keywords ,hook-keywords) (seq-partition use-package-keywords position-of-hook)))
    (push :hook-after hook-keywords)
    (setq use-package-keywords (append pre-hook-keywords hook-keywords))))

(provide 'use-package-hook-after)
;;; use-package-hook-after.el ends here

(require 'use-package-hook-after)

(progn
 (use-package-ensure-elpa 'pulsar '(t) 'nil)
 (unless (fboundp 'recenter) (autoload #'recenter "pulsar" nil t))
 (add-hook 'after-init-hook #'recenter)
 (with-eval-after-load 'magit (add-hook 'magit-post-refresh-hook #'recenter))
 (with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'recenter) (add-hook 'dired-mode-hook #'pulsar-pulse-line)))

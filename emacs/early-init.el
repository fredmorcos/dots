;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Enable init profiling
;; (require 'profiler)
;; (profiler-start 'cpu)
;; (add-hook 'after-init-hook #'profiler-report)

(eval-when-compile
 (defconst emacs-dots-dir "/home/fred/Workspace/dots/emacs/")
 (push emacs-dots-dir load-path))

(require 'init-macros)

;; Custom file (cus-edit).
(fm/var custom-file "/dev/null")

;; A big contributor to startup times is garbage collection.
(fm/var gc-cons-threshold most-positive-fixnum)
(fm/var gc-cons-percentage 0.6)

;; Set version control stuff.
(fm/after vc-hooks
 (fm/var vc-handled-backends '(Git))

 ;; Disable version control when opening files.
 (remove-hook 'find-file-hook #'vc-refresh-state))

;; Built-ins.
(fm/var file-name-handler-alist nil)
(fm/var auto-window-vscroll nil)

;; startup
(fm/var after-init-hook nil)
(fm/var auto-save-list-file-prefix nil)

;; Prevent an early unstyled Emacs by handling UI elements.
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-scroll-bar-mode nil)
(set-fringe-style '(8 . 8))

;; Frame-related improvements.
(fm/var frame-resize-pixelwise t)
(fm/var frame-title-format "%b - emacs")

;; Resizing the Emacs frame can be a terribly expensive part of
;; changing the font. By inhibiting this, we easily halve startup
;; times with fonts that are larger than the system default.
(fm/var frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other
;; settings in this file and can conflict with later config
;; (particularly where the cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Disable tramp when loading .el and .elc files.
(fm/var file-name-handler-alist nil)

;; Improves text rendering performance.
(fm/var bidi-paragraph-direction 'left-to-right)
(fm/var bidi-inhibit-bpa t)

;; nativecomp
(fm/var comp-deferred-compilation t)

;; Run the GC after 5 seconds of idleness.
(run-with-idle-timer 5 t #'garbage-collect)

;; Specify the variable-pitch and other common faces.
(fm/after faces
 (fm/face variable-pitch
  :family "Iosevka Aile")
 (fm/face default
  :family "Monospace"
  :height 90
  :background "Gray98"
  :foreground "Gray40")
 (fm/face fringe
  :background "Gray98")
 (fm/face cursor
  :background "SlateGray3")
 (fm/face region
  :background "LightSteelBlue1")
 (fm/face link
  :foreground "RoyalBlue3"
  :underline (:color "LightSteelBlue3"))
 (fm/face highlight
  :background "Wheat")
 (fm/face error
  :foreground "Red3")
 (fm/face mode-line
  :inherit 'variable-pitch
  :height 0.8
  :background "Gray95"
  :foreground "Gray50"
  :box (:color "Lavender"))
 (fm/face mode-line-inactive
  :inherit mode-line
  :foreground "Gray80")
 (fm/face mode-line-buffer-id
  :foreground "RoyalBlue")
 (fm/face mode-line-highlight
  :inherit mode-line-emphasis
  :background "PowderBlue"))

;; Tab-bar colors & settings.
(fm/after tab-line
 (defun fm/tab-line/tab-name-function (buffer &optional buffers)
  "Put spaces around BUFFER's and BUFFERS' tab name(s)."
  (progn
   (eval-when-compile
    (declare-function tab-line-tab-name-buffer "tab-line" t nil))
   (let ((tab-name (tab-line-tab-name-buffer buffer buffers)))
    (concat "  " tab-name "  "))))

 (fm/var tab-line-close-button-show nil)
 (fm/var tab-line-new-button-show nil)
 (fm/var tab-line-tab-name-function #'fm/tab-line/tab-name-function)

 (fm/key "<C-prior>" tab-line-switch-to-prev-tab nil "tab-line")
 (fm/key "<C-next>" tab-line-switch-to-next-tab nil "tab-line")

 (fm/face tab-line
  :inherit 'variable-pitch
  :height 0.9
  :background "Gray85"
  :box (:color "Lavender"))
 (fm/face tab-line-tab
  :inherit 'tab-line)
 (fm/face tab-line-tab-current
  :inherit 'tab-line-tab
  :background "Gray90")
 (fm/face tab-line-tab-inactive
  :inherit 'tab-line-tab))

(global-tab-line-mode)

(fm/after package
 (fm/var package-quickstart t))

(fm/after url-vars
 (fm/var url-privacy-level 'high)
 (fm/var url-proxy-services '(("no_proxy" . "127.0.0.1"))))

;;; early-init.el ends here

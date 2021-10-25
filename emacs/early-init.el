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

(setq default-frame-alist
 '((width . 160)
   (height . 60)
   (background-color . "Gray98")))

(fm/after emacs
 ;; A big contributor to startup time is garbage collection.
 (fm/var gc-cons-threshold (* 100 1024 1024))
 (fm/var gc-cons-percentage 0.8)

 ;; Prevent an early unstyled Emacs by handling UI elements.
 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (set-scroll-bar-mode nil)
 (set-fringe-style '(8 . 8))

 ;; Disable tramp when loading .el and .elc files.
 (fm/var file-name-handler-alist nil)

 ;; This slows down normal operation.
 (fm/var auto-window-vscroll nil)

 ;; Frame-related improvements.
 (fm/var frame-resize-pixelwise t)
 (fm/var frame-title-format "%b - emacs")

 ;; Resizing the Emacs frame can be a terribly expensive part of changing the font. By
 ;; inhibiting this, we easily halve startup times with fonts that are larger than the
 ;; system default.
 (fm/var frame-inhibit-implied-resize t)

 ;; Improves text rendering performance.
 (fm/var bidi-paragraph-direction 'left-to-right)
 (fm/var bidi-inhibit-bpa t))

;; Set version control stuff.
(fm/after vc-hooks
 (fm/var vc-handled-backends '(Git))

 ;; Disable version control when opening files.
 (remove-hook 'find-file-hook #'vc-refresh-state))

;; Startup.
(fm/after startup
 (fm/var after-init-hook nil)
 (fm/var auto-save-list-file-prefix nil))

;; Ignore X resources; its settings would be redundant with the other
;; settings in this file and can conflict with later config
;; (particularly where the cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Native Compilation.
(fm/var comp-deferred-compilation t)

;; Run the GC after 5 seconds of idleness.
(run-with-idle-timer 5 t #'garbage-collect)

;; ---------- THEME ----------

;; Specify some common faces.
(fm/after faces
 (fm/faces
  default             (:background "Gray98" :foreground "Gray40")
  fringe              (:background "Gray98")
  cursor              (:background "SlateGray3")
  region              (:background "LightSteelBlue1")
  link                (:foreground "RoyalBlue3" :underline (:color "LightSteelBlue3"))
  highlight           (:background "Wheat")
  error               (:foreground "Red3")
  mode-line           (:background "Gray95" :foreground "Gray60" :box (:color "Lavender"))
  mode-line-inactive  (:inherit mode-line :foreground "Gray80")
  mode-line-buffer-id (:foreground "RoyalBlue")
  mode-line-highlight (:inherit mode-line-emphasis :background "PowderBlue")))

(fm/after font-lock
 (fm/faces
  font-lock-function-name-face (:inherit font-lock-builtin-face)
  font-lock-keyword-face       (:foreground "MediumSlateBlue")
  font-lock-type-face          (:foreground "DarkGreen")
  font-lock-variable-name-face (:foreground "DarkCyan")
  font-lock-string-face        (:foreground "OliveDrab")
  font-lock-comment-face       (:foreground "DarkMagenta")
  font-lock-warning-face       (:foreground "Orange3")
  font-lock-constant-face      (:foreground "CornflowerBlue")))

(fm/after display-line-numbers
 (fm/faces
  line-number              (:foreground "Gray85")
  line-number-current-line (:foreground "Gray70")))

(fm/after hl-line
 (fm/faces
  hl-line (:background "Gray95" :extend nil)))

(fm/after paren
 (fm/faces
  show-paren-match            (:background "Gray90")
  show-paren-mismatch         (:background "LightSalmon")
  show-paren-match-expression (:background "Lavender")))

(fm/after flyspell
 (fm/faces
  flyspell-duplicate (:underline "YellowGreen")
  flyspell-incorrect (:underline "Orchid")))

(fm/after org
 (fm/faces
  org-document-title   (:weight semi-bold :foreground "Gray20")
  org-document-info    (:foreground "Gray20")
  org-target           (:slant italic :foreground "Tan")
  org-table            (:foreground "RoyalBlue")
  org-ellipsis         (:weight semi-bold :foreground "Gray30")
  org-level-1          (:weight semi-bold :foreground "Gray30")
  org-level-2          (:weight semi-bold :foreground "Gray30")
  org-level-3          (:weight semi-bold :foreground "Gray30")
  org-level-4          (:weight semi-bold :foreground "Gray30")
  org-link             (:inherit link :weight semi-bold :height 0.8)
  org-todo             (:foreground "Maroon" :weight semi-bold)
  org-done             (:foreground "ForestGreen" :weight semi-bold)
  org-drawer           (:foreground "Snow3")
  org-special-keyword  (:inherit font-lock-keyword-face :weight semi-bold)
  org-block            (:family "Monospace")
  org-block-begin-line (:foreground "thistle")
  org-block-end-line   (:foreground "thistle")))

(fm/after indent-guide
 (fm/faces
  indent-guide-face (:foreground "gray80")))

(fm/after diff-hl
 (fm/faces
  diff-hl-delete (:background "RosyBrown1")
  diff-hl-insert (:background "DarkSeaGreen2")
  diff-hl-change (:background "PowderBlue")))

(fm/after symbol-overlay
 (fm/faces
  symbol-overlay-default-face (:background "HoneyDew2")))

(fm/after multiple-cursors
 (fm/faces
  mc/cursor-bar-face (:background "Gray40" :foreground "White")
  mc/cursor-face     (:background "Gray50" :foreground "White")))

(fm/after hledger-mode
 (fm/faces
  hledger-description-face (:foreground "RoyalBlue")
  hledger-account-face     (:inherit default)
  hledger-amount-face      (:weight semi-bold :background "Gray80" :foreground "Gray40")
  hledger-date-face        (:weight semi-bold :foreground "Gray40")))

(fm/after flycheck
 (fm/faces
  flycheck-error   (:underline "Red1")
  flycheck-info    (:underline "ForestGreen")
  flycheck-warning (:underline "DarkOrange")))

(fm/after flycheck-posframe
 (fm/faces
  flycheck-posframe-background-face (:background "CornSilk")
  flycheck-posframe-warning-face    (:foreground "DarkOrange")
  flycheck-posframe-border-face     (:background "Wheat" :foreground "Wheat")
  flycheck-posframe-error-face      (:foreground "DarkRed")))

(fm/after company
 (fm/faces
  company-tooltip (:background "gray95")))

(fm/after tree-sitter-hl
 (fm/faces tree-sitter-hl-face:property (:slant normal :weight regular)))

(fm/after lsp-mode
 (fm/faces
  lsp-face-highlight-read  (:inherit highlight)
  lsp-face-semhl-namespace (:foreground "CadetBlue")
  lsp-face-semhl-enum      (:foreground "MediumPurple")
  lsp-face-semhl-struct    (:foreground "BlueViolet")))

(fm/after lsp-lens-face
 (fm/faces
  lsp-lens-face       (:inherit shadow)
  lsp-lens-mouse-face (:inherit link)))

(fm/after lsp-rust
 (fm/faces
  lsp-rust-analyzer-inlay-type-face (:inherit font-lock-type-face :height 0.8
                                     :background "HoneyDew2")
  lsp-rust-analyzer-inlay-param-face (:height 0.8 :weight semi-bold
                                      :foreground "DimGray" :background "Azure2")
  lsp-rust-analyzer-inlay-chain-face (:height 0.8 :weight semi-bold
                                      :foreground "DimGray" :background "Khaki")))

(fm/after lsp-ui-doc
 (fm/faces
  lsp-ui-doc-background (:background "Gray92")
  lsp-ui-doc-header (:foreground "Gray98" :background "RoyalBlue")))

(fm/after lsp-ui-peek
 (fm/faces
  lsp-ui-peek-list      (:background "Gray96")
  lsp-ui-peek-peek      (:background "Gray92")
  lsp-ui-peek-selection (:background "LightSkyBlue1")
  lsp-ui-peek-header    (:foreground "Gray98" :background "RoyalBlue")
  lsp-ui-peek-filename  (:foreground "RoyalBlue")))

(fm/after lsp-ui
 (fm/hookn lsp-ui-mode-hook
  (fm/faces lsp-ui-doc-border ("Gray50"))))

(fm/after web-mode
 (fm/faces
  web-mode-current-column-highlight-face (:background "LightSteelBlue1")
  web-mode-current-element-highlight-face (:background "LightSkyBlue1")))

;; -------- END THEME --------

(fm/after package
 (fm/var package-quickstart t))

(fm/after url-vars
 (fm/var url-privacy-level 'high)
 (fm/var url-proxy-services '(("no_proxy" . "127.0.0.1"))))

;;; early-init.el ends here

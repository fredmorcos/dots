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
(setq-default custom-file "/dev/null")

(setq default-frame-alist
 '((width . 160)
   (height . 60)
   (background-color . "Gray98")))

;; Prevent an early unstyled Emacs by handling UI elements.
(fm/after tool-bar (tool-bar-mode -1))
(fm/after menu-bar (menu-bar-mode -1))
(fm/after fringe (set-fringe-style '(8 . 8)))
(fm/after scroll-bar
 (set-scroll-bar-mode nil)
 (horizontal-scroll-bar-mode -1))
(fm/after simple
 (line-number-mode -1)
 (column-number-mode))

(fm/after emacs
 ;; A big contributor to startup time is garbage collection.
 (setq-default gc-cons-threshold (* 100 1024 1024))
 (setq-default gc-cons-percentage 0.8)

 ;; Disable tramp when loading .el and .elc files.
 (setq-default file-name-handler-alist nil)

 ;; This slows down normal operation.
 (setq-default auto-window-vscroll nil)

 ;; Frame-related improvements.
 (setq-default frame-resize-pixelwise t)
 (setq-default frame-title-format "%b - emacs")

 ;; Resizing the Emacs frame can be a terribly expensive part of changing the font. By
 ;; inhibiting this, we easily halve startup times with fonts that are larger than the
 ;; system default.
 (setq-default frame-inhibit-implied-resize t)

 ;; Improves text rendering performance.
 (setq-default bidi-paragraph-direction 'left-to-right)
 (setq-default bidi-inhibit-bpa t))

;; Set version control stuff.
(fm/after vc-hooks
 (setq-default vc-handled-backends '(Git))

 ;; Disable version control when opening files.
 (remove-hook 'find-file-hook #'vc-refresh-state))

;; Startup.
(fm/after startup
 (setq-default after-init-hook nil)
 (setq-default auto-save-list-file-prefix nil))

;; Ignore X resources; its settings would be redundant with the other
;; settings in this file and can conflict with later config
;; (particularly where the cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Native Compilation.
(setq-default comp-deferred-compilation t)

;; Run the GC after 5 seconds of idleness.
(run-with-idle-timer 5 t #'garbage-collect)

(fm/after package
 (setq-default package-archives
  '(("gnu"   . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("org"   . "https://orgmode.org/elpa/")))
 (setq-default package-quickstart-file
  (concat (expand-file-name user-emacs-directory) "var/package-qa"))
 (setq-default package-quickstart t))

(fm/after url-vars
 (setq-default url-privacy-level 'high)
 (setq-default url-proxy-services '(("no_proxy" . "127.0.0.1"))))

;; ---------- THEME ----------

;; Specify some common faces.
(fm/after faces
 (fm/face default             :background "Gray98" :foreground "Gray40")
 (fm/face fringe              :background "Gray98")
 (fm/face cursor              :background "SlateGray3")
 (fm/face region              :background "LightSteelBlue1")
 (fm/face link                :foreground "RoyalBlue3" :underline "LightSteelBlue3")
 (fm/face highlight           :background "Wheat")
 (fm/face error               :foreground "Red3")

 (fm/face mode-line
  :background "Gray95"
  :foreground "Gray60"
  :box (:color "Gray95" :line-width 4))

 (fm/face mode-line-inactive  :inherit mode-line :foreground "Gray80")
 (fm/face mode-line-buffer-id :foreground "RoyalBlue")
 (fm/face mode-line-highlight :inherit mode-line-emphasis :background "PowderBlue"))

(fm/after font-lock
 (fm/face font-lock-function-name-face :inherit font-lock-builtin-face)
 (fm/face font-lock-keyword-face       :foreground "MediumSlateBlue")
 (fm/face font-lock-type-face          :foreground "DarkGreen")
 (fm/face font-lock-variable-name-face :foreground "DarkCyan")
 (fm/face font-lock-string-face        :foreground "OliveDrab")
 ;; (fm/face font-lock-comment-face       :foreground "Thistle3")
 ;; (fm/face font-lock-doc-face           :foreground "YellowGreen")
 (fm/face font-lock-warning-face       :foreground "Orange3")
 (fm/face font-lock-constant-face      :foreground "CornflowerBlue"))

(fm/after whitespace
 (fm/face whitespace-line :background "Gray90" :weight bold))

(fm/after display-line-numbers
 (fm/face line-number              :foreground "Gray85")
 (fm/face line-number-current-line :foreground "Gray70"))

(fm/after hl-line
 (fm/face hl-line :background "Gray95" :extend nil))

(fm/after paren
 (fm/face show-paren-match            :background "Gray90")
 (fm/face show-paren-mismatch         :background "LightSalmon")
 (fm/face show-paren-match-expression :background "Lavender"))

(fm/after flyspell
 (fm/face flyspell-duplicate :underline "YellowGreen")
 (fm/face flyspell-incorrect :underline "Orchid"))

(fm/after org
 (fm/face org-document-title   :foreground "Gray20" :weight bold)
 (fm/face org-document-info    :foreground "Gray20")
 (fm/face org-target           :slant italic :foreground "Tan")
 (fm/face org-table            :foreground "RoyalBlue")
 (fm/face org-ellipsis         :foreground "Gray30" :weight bold)
 (fm/face org-level-1          :foreground "Gray30" :weight bold)
 (fm/face org-level-2          :foreground "Gray30" :weight bold)
 (fm/face org-level-3          :foreground "Gray30" :weight bold)
 (fm/face org-level-4          :foreground "Gray30" :weight bold)
 (fm/face org-link             :inherit link :weight bold :height 0.8)
 (fm/face org-todo             :foreground "Maroon" :weight bold)
 (fm/face org-done             :foreground "ForestGreen" :weight bold)
 (fm/face org-drawer           :foreground "Snow3")
 (fm/face org-special-keyword  :inherit font-lock-keyword-face :weight bold)
 (fm/face org-block            :family "Monospace")
 (fm/face org-block-begin-line :foreground "thistle")
 (fm/face org-block-end-line   :foreground "thistle"))

(fm/after indent-guide
 (fm/face indent-guide-face :foreground "gray80"))

(fm/after diff-hl
 (fm/face diff-hl-delete :background "RosyBrown1")
 (fm/face diff-hl-insert :background "DarkSeaGreen2")
 (fm/face diff-hl-change :background "PowderBlue"))

(fm/after symbol-overlay
 (fm/face symbol-overlay-default-face :background "HoneyDew2"))

(fm/after multiple-cursors
 (fm/face mc/cursor-bar-face :background "Gray40" :foreground "White")
 (fm/face mc/cursor-face     :background "Gray50" :foreground "White"))

(fm/after hledger-mode
 (setq-default hledger-description-face '(:foreground "RoyalBlue"))
 (setq-default hledger-account-face '(:inherit default))
 (setq-default hledger-amount-face
  '(:background "Gray80"
    :foreground "Gray40"
    :weight bold))
 (setq-default hledger-date-face '(:foreground "Gray40" :weight bold)))

(fm/after flycheck
 (fm/face flycheck-error   :underline "Red1")
 (fm/face flycheck-info    :underline "ForestGreen")
 (fm/face flycheck-warning :underline "DarkOrange"))

(fm/after flycheck-posframe
 (fm/face flycheck-posframe-background-face :background "SeaShell")
 (fm/face flycheck-posframe-border-face
  :background "DarkOrange"
  :foreground "DarkOrange")
 (fm/face flycheck-posframe-warning-face    :foreground "DarkOrange")
 (fm/face flycheck-posframe-error-face      :foreground "DarkRed"))

(fm/after company
 (fm/face company-tooltip :background "gray95"))

(fm/after company-posframe
 (setq-default company-posframe-show-params
  '(:internal-border-width 1
    :internal-border-color "gray60")))

(fm/after tree-sitter-hl
 (fm/face tree-sitter-hl-face:property :slant normal :weight regular))

(fm/after lsp-mode
 (fm/face lsp-face-highlight-read :inherit highlight))

(fm/after lsp-semantic-tokens
 (fm/face lsp-face-semhl-namespace :foreground "CadetBlue")
 (fm/face lsp-face-semhl-enum      :foreground "MediumPurple")
 (fm/face lsp-face-semhl-struct    :foreground "BlueViolet"))

(fm/after lsp-lens-face
 (fm/face lsp-lens-face       :inherit shadow)
 (fm/face lsp-lens-mouse-face :inherit link))

(fm/after lsp-diagnostics
 (setq-default lsp-diagnostics-attributes
  '((unnecessary :underline "DarkOrange")
    (deprecated  :strike-through t))))

(fm/after lsp-rust
 (fm/face lsp-rust-analyzer-inlay-type-face
  :inherit font-lock-type-face
  :background "HoneyDew2"
  :height 0.8)
 (fm/face lsp-rust-analyzer-inlay-param-face
  :foreground "DimGray"
  :background "Azure2"
  :weight bold
  :height 0.8)
 (fm/face lsp-rust-analyzer-inlay-chain-face
  :background "Khaki"
  :foreground "DimGray"
  :weight bold
  :height 0.8))

(fm/after lsp-ui-doc
 (setq-default lsp-ui-doc-border "Gray50")
 (fm/face lsp-ui-doc-background :background "Gray92")
 (fm/face lsp-ui-doc-header     :background "RoyalBlue" :foreground "Gray98"))

(fm/after lsp-ui-peek
 (fm/face lsp-ui-peek-list      :background "Gray96")
 (fm/face lsp-ui-peek-peek      :background "Gray92")
 (fm/face lsp-ui-peek-selection :background "LightSkyBlue1")
 (fm/face lsp-ui-peek-header    :background "RoyalBlue" :foreground "Gray98")
 (fm/face lsp-ui-peek-filename  :foreground "RoyalBlue"))

(fm/after web-mode
 (fm/face web-mode-current-column-highlight-face  :background "Azure2")
 (fm/face web-mode-current-element-highlight-face :background "Azure2"))

;;; early-init.el ends here

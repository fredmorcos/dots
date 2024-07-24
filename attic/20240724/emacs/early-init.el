;;; early-init.el --- Emacs early configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Enable init profiling
;; (require 'profiler)
;; (profiler-start 'cpu)
;; (add-hook 'after-init-hook #'profiler-report)

(eval-when-compile
 (defconst emacs-dots-dir "~/Workspace/dots/emacs/")
 (push emacs-dots-dir load-path))
(require 'init-macros)

;; Custom file (cus-edit).
(setq-default custom-file "/dev/null")

;; (setq default-frame-alist
;;  '((width . 160)
;;    (height . 60)
;;    (background-color . "Gray98")
;;    ;; (alpha-background . 95)
;;    (use-frame-synchronization . t)))

;; Disable showing the warnings buffer, but still log warnings.
(im/after warnings
 (setq-default warning-minimum-level :emergency))

;; UI.
(im/after frame
 (modify-all-frames-parameters
  '((background-color . "Gray98")
    (fullscreen . maximized)
    (use-frame-synchronization . t))))

;; Prevent an early unstyled Emacs by handling UI elements.
(im/after tool-bar (tool-bar-mode -1))
(im/after menu-bar (menu-bar-mode -1))
(im/after tool-bar
 (setopt tool-bar-position 'left)
 (setq-default tool-bar-style 'image))
(im/after fringe (set-fringe-style '(8 . 8)))
(im/after scroll-bar
 (set-scroll-bar-mode nil)
 (horizontal-scroll-bar-mode -1))
(im/after simple
 (line-number-mode -1)
 (column-number-mode))

(im/after emacs
 ;; Skip redisplays
 (setq-default redisplay-skip-fontification-on-input t)
 (setq-default redisplay-skip-initial-frame t)

 ;; A big contributor to startup time is garbage collection.
 (setq-default gc-cons-threshold (* 100 1024 1024))
 (setq-default gc-cons-percentage 0.8)

 ;; Disable tramp when loading .el and .elc files.
 ;; (setq-default file-name-handler-alist nil)

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
(im/after vc-hooks
 (setq-default vc-handled-backends '(Git))

 ;; Disable version control when opening files.
 (remove-hook 'find-file-hook #'vc-refresh-state))

;; Startup.
(im/after startup
 (setq-default after-init-hook nil)
 (setq-default auto-save-list-file-prefix nil))

;; Ignore X resources; its settings would be redundant with the other
;; settings in this file and can conflict with later config
;; (particularly where the cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Native Compilation.
(setq-default comp-deferred-compilation t)
(im/after comp
 (setq-default native-comp-async-report-warnings-errors 'silent))

;; Run the GC after 5 seconds of idleness.
(run-with-idle-timer 5 t #'garbage-collect)

(im/after package
 (setq-default package-archives
  '(("gnu"   . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))
 (setq-default package-quickstart-file
  (concat (expand-file-name user-emacs-directory) "var/package-qa"))
 (setq-default package-quickstart t)
 (setq-default package-native-compile t)
 (setq-default package-install-upgrade-built-in t))

(im/after url-vars
 (setq-default url-privacy-level 'high)
 (setq-default url-proxy-services '(("no_proxy" . "127.0.0.1"))))

;; ---------- THEME ----------

(load-theme 'modus-operandi)

;; ;; Specify some common faces.
;; (im/after faces
;;  (im/face default             :background "Gray98" :foreground "Gray40")
;;  (im/face fringe              :background "Gray98")
;;  (im/face cursor              :background "SlateGray3")
;;  (im/face region              :background "LightSteelBlue1")
;;  (im/face link                :foreground "RoyalBlue3" :underline "LightSteelBlue3")
;;  (im/face highlight           :background "Wheat")
;;  (im/face error               :foreground "Red3")

;;  (im/face fill-column-indicator :foreground "SeaShell2")

;;  (im/face mode-line
;;   :background "Gray95"
;;   :foreground "Gray60"
;;   :box (:color "Gray95" :line-width 0))

;;  (im/face mode-line-inactive  :inherit mode-line :foreground "Gray80")
;;  (im/face mode-line-buffer-id :foreground "RoyalBlue")
;;  (im/face mode-line-highlight :inherit mode-line-emphasis :background "PowderBlue")

;;  (im/face vertical-border :inherit mode-line-inactive))

;; (im/after font-lock
;;  (im/face font-lock-function-name-face :inherit font-lock-builtin-face)
;;  (im/face font-lock-keyword-face       :foreground "MediumSlateBlue")
;;  (im/face font-lock-type-face          :foreground "DarkGreen")
;;  (im/face font-lock-variable-name-face :foreground "DarkCyan")
;;  (im/face font-lock-string-face        :foreground "OliveDrab")
;;  (im/face font-lock-warning-face       :foreground "Orange3")
;;  (im/face font-lock-constant-face      :foreground "CornflowerBlue")
;;  (im/face font-lock-doc-face           :foreground "HoneyDew3"))

;; (im/after whitespace
;;  (im/face whitespace-line :background "Gray90" :weight bold))

;; (im/after display-line-numbers
;;  ;; (im/face line-number                 :foreground "Gray60")
;;  ;; (im/face line-number-current-line    :foreground "Gray50")
;;  (im/face line-number              :foreground "Gray85")
;;  (im/face line-number-current-line :foreground "Gray70"))

;; (im/after hl-line
;;  (im/face hl-line :background "Gray95" :extend nil))

;; (im/after paren
;;  (im/face show-paren-match            :background "Gray90")
;;  (im/face show-paren-mismatch         :background "LightSalmon")
;;  (im/face show-paren-match-expression :background "Lavender"))

;; (im/after flyspell
;;  (im/face flyspell-duplicate :underline "YellowGreen")
;;  (im/face flyspell-incorrect :underline "Orchid"))

;; (im/after org
;;  (im/face org-document-title   :foreground "Gray20" :weight bold)
;;  (im/face org-document-info    :foreground "Gray20")
;;  (im/face org-target           :slant italic :foreground "Tan")
;;  (im/face org-table            :foreground "RoyalBlue")
;;  (im/face org-ellipsis         :foreground "Gray30" :weight bold)
;;  (im/face org-level-1          :foreground "Gray30" :weight bold)
;;  (im/face org-level-2          :foreground "Gray30" :weight bold)
;;  (im/face org-level-3          :foreground "Gray30" :weight bold)
;;  (im/face org-level-4          :foreground "Gray30" :weight bold)
;;  (im/face org-link             :inherit link :weight bold :height 0.8)
;;  (im/face org-todo             :foreground "Maroon" :weight bold)
;;  (im/face org-done             :foreground "ForestGreen" :weight bold)
;;  (im/face org-drawer           :foreground "Snow3")
;;  (im/face org-special-keyword  :inherit font-lock-keyword-face :weight bold)
;;  (im/face org-block            :family "Monospace")
;;  (im/face org-block-begin-line :foreground "thistle")
;;  (im/face org-block-end-line   :foreground "thistle")
;;  (im/face org-code             :foreground "maroon"))

;; (im/after indent-guide
;;  (im/face indent-guide-face :foreground "gray80"))

;; (im/after diff-hl
;;  (im/face diff-hl-delete :background "RosyBrown1")
;;  (im/face diff-hl-insert :background "DarkSeaGreen2")
;;  (im/face diff-hl-change :background "PowderBlue"))

;; (im/after symbol-overlay
;;  (im/face symbol-overlay-default-face :background "HoneyDew2"))

;; (im/after multiple-cursors
;;  (im/face mc/cursor-bar-face :background "Gray40" :foreground "White")
;;  (im/face mc/cursor-face     :background "Gray50" :foreground "White"))

;; (im/after volatile-highlights
;;  (im/face vhl/default-face :background "Papaya Whip"))

;; (im/after eros
;;  (im/face eros-result-overlay-face :background "RoyalBlue" :foreground "White"))

;; (im/after hledger-mode
;;  (setq-default hledger-description-face '(:foreground "RoyalBlue"))
;;  (setq-default hledger-account-face '(:inherit default))
;;  (setq-default hledger-amount-face
;;   '(:background "Wheat"
;;     :foreground "Gray40"
;;     :weight bold))
;;  (setq-default hledger-date-face '(:foreground "Gray40" :weight bold)))

;; (im/after flycheck
;;  (im/face flycheck-info :underline "ForestGreen")
;;  (if (string-equal (system-name) "neuron")
;;   (progn
;;    (im/face flycheck-error   :underline "DarkRed" :background "RosyBrown1")
;;    (im/face flycheck-warning :underline "Peru"    :background "PeachPuff1"))
;;   (progn
;;    (im/face flycheck-error   :underline "Red1")
;;    (im/face flycheck-warning :underline "DarkOrange"))))

;; (im/after flycheck-posframe
;;  (im/face flycheck-posframe-background-face :background "SeaShell")
;;  (im/face flycheck-posframe-border-face
;;   :background "DarkOrange"
;;   :foreground "DarkOrange")
;;  (im/face flycheck-posframe-error-face :foreground "DarkRed")
;;  (if (string-equal (system-name) "neuron")
;;   (im/face flycheck-posframe-warning-face :foreground "Peru")
;;   (im/face flycheck-posframe-warning-face :foreground "DarkOrange")))

;; (im/after company
;;  (im/face company-tooltip :background "Gray95"))

;; (im/after company-posframe
;;  (setq-default company-posframe-show-params
;;   '(:internal-border-width 1
;;     :internal-border-color "Gray60"))
;;  (setq-default company-posframe-quickhelp-show-params
;;   '(:poshandler company-posframe-quickhelp-right-poshandler
;;     :timeout 60
;;     :no-properties nil
;;     :internal-border-width 1
;;     :internal-border-color "Gray60")))

;; (im/after tree-sitter-hl
;;  (im/face tree-sitter-hl-face:property :slant normal :weight regular))

;; (im/after scopeline
;;  (im/face scopeline-face :foreground "HoneyDew3"))

;; (im/after lsp-mode
;;  (im/face lsp-face-highlight-read :inherit highlight))

;; (im/after lsp-semantic-tokens
;;  (im/face lsp-face-semhl-namespace :foreground "CadetBlue")
;;  (im/face lsp-face-semhl-enum      :foreground "MediumPurple")
;;  (im/face lsp-face-semhl-struct    :foreground "BlueViolet"))

;; (im/after lsp-lens-face
;;  (im/face lsp-lens-face       :inherit shadow)
;;  (im/face lsp-lens-mouse-face :inherit link))

;; (im/after lsp-diagnostics
;;  (setq-default lsp-diagnostics-attributes
;;   '((unnecessary :underline "DarkOrange")
;;     (deprecated  :strike-through t))))

;; (im/after lsp-rust
;;  (im/face lsp-rust-analyzer-inlay-face
;;   :foreground "Gray65"
;;   :weight bold
;;   :height 0.8)
;;  (im/face lsp-rust-analyzer-inlay-param-face
;;   :foreground "Gray65"
;;   :weight bold
;;   :height 0.8)
;;  (im/face lsp-rust-analyzer-inlay-type-face
;;   :inherit font-lock-type-face
;;   :foreground "Gray65"
;;   :weight bold
;;   :height 0.8)
;;  (im/face lsp-rust-analyzer-declaration-modifier-face
;;   :underline nil)
;;  (im/face lsp-rust-analyzer-mutable-modifier-face
;;   :underline nil)
;;  (im/face lsp-rust-analyzer-consuming-modifier-face
;;   :underline nil)
;;  (im/face lsp-rust-analyzer-documentation-modifier-face
;;   :foreground "HoneyDew3"))

;; (im/after lsp-ui-doc
;;  (setq-default lsp-ui-doc-border "Gray50")
;;  (im/face lsp-ui-doc-background :background "Gray92")
;;  (im/face lsp-ui-doc-header     :background "RoyalBlue" :foreground "Gray98"))

;; (im/after lsp-ui-peek
;;  (im/face lsp-ui-peek-list      :background "Gray96")
;;  (im/face lsp-ui-peek-peek      :background "Gray92")
;;  (im/face lsp-ui-peek-selection :background "LightSkyBlue1")
;;  (im/face lsp-ui-peek-header    :background "RoyalBlue" :foreground "Gray98")
;;  (im/face lsp-ui-peek-filename  :foreground "RoyalBlue"))

;; (im/after web-mode
;;  (im/face web-mode-current-column-highlight-face  :background "Azure2")
;;  (im/face web-mode-current-element-highlight-face :background "Azure2"))

;; (im/after magit
;;  (im/face magit-branch-local  :foreground "SkyBlue4" :background "Azure")
;;  (im/face magit-branch-remote :foreground "DarkOliveGreen4" :background "Honeydew"))

;; (im/after magit-diff
;;  (im/face magit-diff-added-highlight
;;   :background "#cceecc"
;;   :foreground "#22aa22"
;;   :extend nil)
;;  (im/face magit-diff-added
;;   :background "#ddffdd"
;;   :foreground "#22aa22"
;;   :extend nil)
;;  (im/face magit-diff-removed-highlight
;;   :background "#eecccc"
;;   :foreground "#aa2222"
;;   :extend nil)
;;  (im/face magit-diff-removed
;;   :background "#ffdddd"
;;   :foreground "#aa2222"
;;   :extend nil)
;;  (im/face magit-diff-context-highlight
;;   :background "Grey95"
;;   :foreground "Grey50"
;;   :extend nil))

;;; early-init.el ends here

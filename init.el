(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
This version of Emacs does not have SSL support."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  )

(package-initialize)

;; use-package for automatic MELPA downloading/uploading
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Clipboard and input methods
(setq select-enable-clipboard t)
(setq default-input-method "vietnamese-telex")

;; Navigation
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Directory explorer + all-the-icons
(use-package all-the-icons
  :ensure t)
(use-package neotree
  :ensure t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(global-set-key [f8] 'neotree-toggle)

;; DEVELOPMENT ASSISTS ----------------------------------------------------
;; Auto-pairs [] {} () ""
(electric-pair-mode 1)

;; Javadoc/Doxygen comment creation
(semantic-mode 1)
(require 'doc-mode)
(add-hook 'c-mode-common-hook 'doc-mode)
(global-set-key (kbd "C-c c d") 'doc-mode-fix-tag-doc)
(global-set-key (kbd "C-c c r") 'doc-mode-remove-tag-doc)
(global-set-key (kbd "C-c c t") 'doc-mode-toggle-tag-doc)
(global-set-key (kbd "C-c c f") 'doc-mode-fold)

;; Autocompletion framework
(use-package company
  :ensure t)

;; Real-time error checking
(use-package flycheck
  :ensure t)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Code autoformatting
(use-package format-all
  :ensure t)

;; Indentation highlighting
(use-package highlight-indent-guides
  :ensure t)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-delay 1)
(setq highlight-indent-guides-responsive "top")

;; Folding
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; LANGUAGE MODES ---------------------------------------------------------
;; C++
(use-package company-irony
  :ensure t)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(use-package yasnippet
  :ensure t)
(add-hook 'after-init-hook 'global-company-mode)

;; Rust
(use-package rust-mode
  :ensure t)
(use-package cargo
  :ensure t)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(use-package flycheck-rust
  :ensure t)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Qt/QML


(use-package qml-mode
  :ensure t)
 (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
 (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

;; LOOKS ------------------------------------------------------------------
;; Terminal mode only: transparent background
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)
;; GTK only: transparent background
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))
;; Window title format
(setq-default frame-title-format '("%b"))
;; Modeline
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(use-package doom-modeline
  :ensure t)
(doom-modeline-mode 1)

;; Doom themes
(use-package doom-themes
  :ensure t)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc).
;; doom-dark+ works best with Materia GTK.
(load-theme 'doom-dark+ t)

;; Disable scrollbar
(scroll-bar-mode -1)

;; Highlight current line
(global-hl-line-mode 1)

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)))
;; Set the title
(setq dashboard-banner-logo-title "G N U • E M A C S")
;; Set the banner
(setq dashboard-startup-banner 'logo)
;; Content is not centered by default.
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar-width 4)
 '(doom-modeline-buffer-encoding t)
 '(doom-modeline-github nil)
 '(font-use-system-font t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   (quote
    (dashboard doom-themes doom-modeline flycheck company highlight-indent-guides format-all yasnippet)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote top)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "MesloLGS NF" :foundry "PfEd" :slant normal :weight normal :height 113 :width normal)))))

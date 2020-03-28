;;; htkhiem's emacs setup
;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
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

;; System-wide clipboard
(setq select-enable-clipboard t)

;; Navigation
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Dev stuff
(electric-pair-mode 1)
(require 'doxymacs) ; Not available on MELPA
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(use-package company
  :ensure t)
(use-package yasnippet
  :ensure t)
(add-hook 'after-init-hook 'global-company-mode)
(use-package flycheck
  :ensure t)
(add-hook 'after-init-hook #'global-flycheck-mode)
(use-package format-all
  :ensure t)
(use-package highlight-indent-guides
  :ensure t)
;; Indentation highlighting
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-delay 0)
(setq highlight-indent-guides-responsive "top")

;; Folding
(add-hook 'c-mode-common-hook 'hs-minor-mode)
;; LOOKS ------------------------------------------------------------------
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar-width 4)
 '(doom-modeline-buffer-encoding t)
 '(doom-modeline-github nil)
 '(doom-modeline-height 12)
 '(font-use-system-font t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   (quote
    (doom-themes doom-modeline flycheck company highlight-indent-guides format-all yasnippet)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tool-bar-position (quote top)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "MesloLGS NF" :foundry "PfEd" :slant normal :weight normal :height 113 :width normal)))))

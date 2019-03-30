(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (diminish ag helm-ag ace-window highlight-symbol company-lsp helm-projectile flycheck-rust cargo rust-mode toml-mode company lsp-ui flycheck helm use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'use-package)
(setq use-package-always-ensure t)

(use-package highlight-symbol
  :bind
  ("M-n" . highlight-symbol-next)
  ("M-p" . highlight-symbol-prev))

(use-package ag)

(use-package helm-ag
  :after ag)

(use-package helm-projectile
  :after helm
  :config
  (helm-projectile-on))

(use-package diminish)

(use-package helm
  :diminish helm-mode
  :init
  (require 'helm-config)
  :bind
  ("C-c f" . helm-projectile-find-file-dwim)
  ("M-x" . helm-M-x)
  ("C-x r b" . helm-filtered-bookmarks)
  ("C-x C-f" . helm-find-files)
  :init
  (helm-mode 1))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind
  ("M-<down>" . flycheck-next-error)
  ("M-<up>" . flycheck-previous-error))

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1))

;; `company' backend for `lsp-mode'
(use-package company-lsp
  :ensure t
  :after company lsp-mode
  :init
  (push 'company-lsp company-backends))

(use-package lsp-mode
  :commands lsp
  :diminish lsp-mode
  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui)

(use-package toml-mode)


(use-package eldoc
  :diminish eldoc-mode)

(use-package rust-mode
  :hook (rust-mode . lsp)
  :bind ("C-c r" . rust-run)
  :config (setq rust-format-on-save t))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :diminish cargo-minor-mode)

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;; (which-function-mode 1)
(global-set-key (kbd "C--") #'undo)
(global-set-key (kbd "M-g") #'goto-line)

(setq inhibit-startup-screen t)

(provide 'init)
;;; init.el ends here

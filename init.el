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
    (company-lsp helm-projectile flycheck-rust cargo rust-mode toml-mode company lsp-ui flycheck helm use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package helm
  :init
  :bind (("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)
         ("C-x f" . helm-recentf)
         ("C-x b" . helm-mini)
         ("M-/" . helm-dabbrev)
	 ("C-c f" . helm-projectile-find-file-dwim))
  :config (progn
            (helm-mode 1)))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind
  ("M-<down>" . flycheck-next-error)
  ("M-<up>" . flycheck-previous-error))

(use-package company
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
  :config
  (require 'lsp-clients))

(use-package lsp-ui)

(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp)
  :bind ("C-c r" . rust-run)
  :config (setq rust-format-on-save t))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


;; (which-function-mode 1)
(global-set-key (kbd "C--") #'undo)
(global-set-key (kbd "M-g") #'goto-line)

(defvar mode-line-cleaner-alist
  '((auto-complete-mode . "")
    (yas/minor-mode . "")
    (cargo-minor-mode . "")
    (helm-mode . "")
    (lsp-mode . "")
    (flymake-mode . "")
    (eldoc-mode . "")
    (company-mode . "")
    ;; Major modes
    (python-mode . "Py")
    (emacs-lisp-mode . "EL"))
  "Alist for `clean-mode-line`.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(require 'cl)
(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(setq inhibit-startup-screen t)

(provide 'init)
;;; init.el ends here

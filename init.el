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
    (magit go-guru auto-complete-config go-autocomplete go-rename racer lsp-ui diminish ag helm-ag ace-window highlight-symbol company-lsp helm-projectile flycheck-rust cargo rust-mode toml-mode company flycheck helm use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(toggle-debug-on-error)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package highlight-symbol
  :bind
  ("M-n" . highlight-symbol-next)
  ("M-p" . highlight-symbol-prev)
  :config
  (setq highlight-symbol-nav-mode t)
  (setq highlight-symbol-mode t)
  (setq highlight-symbol-idle-delay 0.3))

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

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil))

(use-package toml-mode)


(use-package eldoc
  :diminish eldoc-mode)

(use-package rust-mode
  :hook (rust-mode . lsp)
  :bind
  ("C-c g" . rust-run)
  ("C-c b" . rust-test)
  :config
  (setq rust-format-on-save t))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :diminish cargo-minor-mode)

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package racer
  :after rust-mode
  :diminish racer-mode
  :hook (rust-mode . racer-mode)
  :bind
  ("M-j" . racer-find-definition)
  (:map racer-mode-map ("M-." . #'xref-find-definitions)))


;; setup for Go
;; install binaries:
;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/rogpeppe/godef/...
;; go get -u github.com/nsf/gocode
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u github.com/dougm/goflymake
;; go get -u github.com/alecthomas/gometalinter
;; gometalinter --install --update
;; go get -u golang.org/x/tools/cmd/gorename

(use-package flycheck-gometalinter
  :demand t
  :config
  (flycheck-gometalinter-setup)
  (setq flycheck-gometalinter-vendor t)
  (setq flycheck-gometalinter-fast t)
  (setq flycheck-gometalinter-disable-linters '("gotype"))
  (setq flycheck-gometalinter-deadline "10s"))

(use-package auto-complete
  :hook (go-mode . auto-complete-mode))

(use-package go-autocomplete
  :demand t)

(use-package go-mode
  :after go-eldoc
  :bind
  ("M-," . pop-tag-mark)
  ("M-P" . recompile)          ; Redo most recent compile cmd
  ("M-<down>" . next-error)
  ("M-<up>" . previous-error)
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-command "goimports")
  (setq compile-command
       "go build -i -v && go test -v && go vet")
  (go-eldoc-setup))

;; (use-package auto-complete-config
;;   :hook go-mode
;;   :config (ac-config-default))

(use-package go-eldoc)

(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
	'("~/.emacs.d/snippets")))

(use-package go-guru
  :demand t
  :bind
  ("M-d" . go-guru-describe)
  ("M-." . go-guru-definition) ; godef-jump)
  ("M-?" . go-guru-referrers)
  :config
  (go-guru-hl-identifier-mode) ; highlight identifiers
  )

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; other setups
;; (which-function-mode 1)
(global-set-key (kbd "C--") #'undo)
(global-set-key (kbd "M-g") #'goto-line)

(setq inhibit-startup-screen t)

(provide 'init)
;;; init.el ends here

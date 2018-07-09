(tool-bar-mode -1)
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
	("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package slime
  :ensure slime
  :init
  (setq slime-contribs '(slime-fancy slime-quicklisp slime-company))
  :config
  (setq slime-lisp-implementations
        `((roswell ("ros" "run"))
          (ecl ("ros" "-L" "ecl" "run" "-l" ,(expand-file-name "~/.eclrc")))
          (sbcl ("ros" "-L" "sbcl" "run" "-l" ,(expand-file-name "~/.sbclrc"))))
        slime-auto-start 'always
        slime-default-lisp 'sbcl)
  (use-package slime-company
    :ensure t
    :config
    (slime-setup '(slime-company))
    (define-key slime-mode-map (kbd "TAB") 'company-indent-or-complete-common)
    (add-hook 'slime-lisp-mode-hook 'company-mode)))

(use-package clojure-mode
  :ensure t)

(use-package cider
  :after (clojure-mode)
  :ensure t
  :bind (("C-c M-j" . cider-jack-in)
         ("C-c M-J" . cider-jack-in-clojurescript)
	 ("C-c M-c" . cider-connect))
  :config
  (setq cider-default-repl-command "lein"))

(use-package clj-refactor
  :after (clojure-mode cider)
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'cider-repl-mode-hook 'clj-refactor-mode)
  (cljr-add-keybindings-with-prefix "C-c m"))

(use-package ido
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (use-package ido-completing-read+
    :ensure t
    :config
    (ido-ubiquitous-mode 1))
  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode 1)
    (setq ido-enable-flex-matching t
	  id-use-faces nil)))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command))
  :config
  (smex-initialize))

(use-package magit
  :ensure t
  :after (ido)
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package company
  :ensure t
  :config
  (setq company-tooltip-align-annotations t)
  (use-package company-quickhelp
    :ensure t
    :config
    (add-hook 'company-mode-hook 'company-quickhelp-mode)))

(use-package rust-mode
  :ensure t
  :config
  (use-package flycheck-rust
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
    (add-hook 'rust-mode-hook 'flycheck-mode))
  (use-package cargo
    :ensure t
    :config
    (add-hook 'rust-mode-hook 'cargo-minor-mode))
  (use-package racer
    :ensure t
    :config
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)
    (add-hook 'racer-mode-hook 'company-mode))
  (define-key rust-mode-map (kbd "TAB") 'company-indent-or-complete-common))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook
	    (lambda ()
	      (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil)))
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package recentf
  :config
  (recentf-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key projectile company-quickhelp slime-company flycheck-rust racer company cargo rust-mode ido-vertical-mode magit smex ido-completing-read+ flx-ido cider paredit use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

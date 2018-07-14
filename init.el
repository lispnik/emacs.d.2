(tool-bar-mode -1)
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name)
      ring-bell-function 'ignore
      blink-matching-paren nil)
(when (eq window-system 'ns)
  (setq ns-command-modifier 'meta
	ns-alternate-modifier 'super))
(require 'cl)
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

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package flycheck
  :ensure t
  :config
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

(use-package dired
  :config
  (add-hook 'dired-mode-hook 'hl-line-mode)
  (use-package dired-x
    :bind (:map dired-mode-map ("M-o" . dired-omit-mode))
    :config
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    
    (when (eq system-type 'windows-nt)
      (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^ntuser.*\\|NTUSER.*"))))

(use-package dired-atool
  :ensure t
  :bind (:map dired-mode-map
              ("z" . dired-atool-do-unpack)
	      ("Z" . dired-atool-do-pack)))

(use-package slime
  :ensure slime
  :init
  (setq slime-contribs '(slime-fancy slime-quicklisp slime-company))
  :config
  (setq slime-lisp-implementations
	(case system-type
	  (windows-nt
	   ;; Alllows SDL2 applications to start from SLIME
	   `((ccl ("cmd" "/c" ,(expand-file-name "~/Clozure CL/wx86cl64.exe")))))
	  (t `((roswell ("ros" "run"))
               (ecl ("ros" "-L" "ecl" "run"))
               (sbcl ("ros" "-L" "sbcl" "run")))))
        slime-auto-start 'always
        slime-default-lisp (case system-type
			     (windows-nt 'ccl)
			     (t 'sbcl)))
  (use-package slime-company
    :ensure t
    :config
    (slime-setup '(slime-company))
    (define-key slime-mode-map (kbd "TAB") 'company-indent-or-complete-common)
    (add-hook 'slime-repl-mode-hook 'company-mode)
    (define-key slime-repl-mode-map (kbd "TAB") 'company-indent-or-complete-common)))

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
  (projectile-mode)
  (use-package ripgrep
    :ensure t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package recentf
  :bind (("C-x f" . recentf-ido-find-file))
  :config
  (use-package recentf-ext
    :ensure t)
  (recentf-mode 1)
  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
      (when file
	(find-file file)))))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook 'flycheck-mode)
  (use-package company-go
    :ensure t
    :config
    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode)))))

(use-package restclient
  :ensure t
  :config
  (use-package company-restclient
    :ensure t)
  (use-package restclient-test
    :ensure t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e8825f26af32403c5ad8bc983f8610a4a4786eb55e3a363fa9acb48e0677fe7e" "5acb6002127f5d212e2d31ba2ab5503df9cd1baa1200fbb5f57cc49f6da3056d" default)))
 '(package-selected-packages
   (quote
    (recentf-ext restclient-test company-restclient restclient projectile-ripgrep dired-atool farmhouse-theme espresso-theme company-go go-mode anzu which-key projectile company-quickhelp slime-company flycheck-rust racer company cargo rust-mode ido-vertical-mode magit smex ido-completing-read+ flx-ido cider paredit use-package)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fira Code Retina" :foundry "outline" :slant normal :weight normal :height 100 :width normal)))))

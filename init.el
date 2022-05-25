;;; -*- lexical-binding: t -*-

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defun turn-off-indent-tabs-mode ()
  (setq indent-tabs-mode nil))

(use-package emacs
  :custom ((desktop-load-locked-desktop t)
           (desktop-save-mode t)
           (savehist-mode t)
           (show-paren-mode t)
           (mac-option-modifier 'meta)
           (mac-command-modifier 'super)
           (vc-follow-symlinks t)
           (default-directory "~/")
           (blink-matching-paren nil)
           (ring-bell-function 'ignore)
           (inhibit-startup-screen t)
           (inhibit-startup-echo-area-message (user-login-name))
           (tool-bar-style 'image)))

(use-package modus-themes
  :straight t
  :init
  (modus-themes-load-themes)
  :config (modus-themes-load-operandi)
  :custom ((modus-themes-italic-constructs nil)
           (modus-themes-slanted-constructs nil)
           (modus-themes-bold-constructs nil))
  :bind (("<f5>" . modus-themes-toggle)))

(use-package bind-key :straight t)
(use-package delight :straight t)

(use-package yaml-mode :straight t)
(use-package markdown-mode :straight t)
(use-package nasm-mode :straight t)
(use-package lua-mode :straight t)
(use-package dockerfile-mode :straight t)
(use-package ag :straight t)
(use-package ripgrep :straight t)

(use-package vterm
  :custom (vterm-always-compile-module t)
  :straight t)

(use-package lisp-mode
  :hook ((lisp-mode . turn-off-indent-tabs-mode)))

(use-package emacs-mode
  :hook ((emacs-lisp-mode . turn-off-indent-tabs-mode))
  :bind ("C-c C-c" . eval-defun))

(use-package dired
  :hook (dired-mode . hl-line-mode)
  :config
  (use-package dired-x
    :bind (:map dired-mode-map ("M-o" . dired-omit-mode))
    :hook (dired-mode . (lambda () (dired-omit-mode 1)))
    :init (when (eq system-type 'windows-nt)
            (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^ntuser.*\\|NTUSER.*")))

  (use-package dired-atool
    :if (memq system-type '(gnu/linux darwin)) 
    :straight t
    :bind (:map dired-mode-map
                ("z" . dired-atool-do-unpack)
                ("Z" . dired-atool-do-pack))))

(use-package diff-hl
  :straight t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status))
  :config
  ;; from https://github.com/dgutov/diff-hl 
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package company
  :straight t
  :bind (:map emacs-lisp-mode-map ("TAB" . company-indent-or-complete-common))
  :hook (emacs-lisp-mode . company-mode)
  :init (setq company-tooltip-align-annotations t)
  :config 
  (use-package company-quickhelp
    :straight t
    :hook (company-mode . company-quickhelp-mode)))

(use-package slime-company
  :straight t
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))

(use-package slime
  :straight t
  :after (paredit)
  :hook ((slime-repl-mode . enable-paredit-mode)
         (slime-mode . enable-paredit-mode))
  :bind (:map slime-repl-mode-map
              ("TAB" . company-indent-or-complete-common)
              ("DEL" . paredit-backward-delete))
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company)))

(use-package plisp-mode
  :straight t
  :mode ("\\.l\\'" . plisp-mode)
  :custom
  (plisp-disable-slime-p t)
  (plisp-documentation-directory "~/Projects/pil21/doc"))

;;; https://www.reddit.com/r/emacs/comments/km3klo/comment/gmionkm/?utm_source=share&utm_medium=web2x&context=3
(use-package geiser
  :ensure t
  :defer t
  :defines geiser-guile-binary
  :functions geiser-impl--set-buffer-implementation
  :commands (geiser run-geiser)
  :config
  ;; Send the argument of `run-geiser' to
  ;; `geiser-impl--set-buffer-implementation' BEFORE `run-geiser' is
  ;; ran. As I had to set the Scheme implementation by hand otherwise
  ;; with `geiser-set-scheme'
  ;; (setq geiser-guile-binary "/usr/bin/guile3") ; Use the latest guile
  (advice-add 'run-geiser :before #'geiser-impl--set-buffer-implementation)) 

(use-package geiser-racket :straight t)

(use-package ggtags
  :straight t
  :hook ((c-mode . ggtags-mode)
         (c++-mode . ggtags-mode)))

(use-package ediff
  :init (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package projectile
  :straight t
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config (projectile-mode))

(use-package which-key
  :straight t
  :config (which-key-mode)
  :delight)

(use-package paredit
  :straight t
  :hook
  ((emacs-lisp-mode . enable-paredit-mode)
   (lisp-mode . enable-paredit-mode)
   (sly-mrepl-mode . enable-paredit-mode)
   (sly-mrepl-mode . (lambda ()
                            (define-key sly-mrepl-mode-map
                              (read-kbd-macro paredit-backward-delete-key) nil))))
  :bind ((:map lisp-mode-map
               ("M-<left>" . paredit-backward-slurp-sexp)
               ("M-<right>" . paredit-forward-slurp-sexp))))

(use-package anzu
  :straight t
  :config (global-anzu-mode 1)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :delight)

(use-package editorconfig
  :straight t
  :config (editorconfig-mode)
  :delight)

(use-package org-roam
  :straight t
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (expand-file-name "~/Documents/Roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))

(use-package deft
  :straight t
  :after (org org-roam)
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(use-package org-download
  :straight t
  :after org
  :bind (:map org-mode-map
              (("s-Y" . org-download-screenshot)
               ("s-y" . org-download-yank))))

(use-package flycheck
  :straight t
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :config
  (global-flycheck-mode)
  (flycheck-define-checker sh-shellcheck
    "A shell script syntax and style checker using Shellcheck.

See URL `https://github.com/koalaman/shellcheck/'."
  :command ("shellcheck" "-f" "checkstyle" "-s" (eval (symbol-name sh-shell)) source)
  :modes sh-mode
  :error-parser flycheck-parse-checkstyle))

(use-package elpher :straight t)

(use-package selectrum
  :straight t
  :config
  (selectrum-mode 1))

(use-package selectrum-prescient
  :after selectrum
  :straight t
  :config 
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

;; (use-package yasnippet
;;   :straight t
;;   :hook ((emacs-lisp-mode . yas-minor-mode-on)
;;          (lisp-mode . yas-minor-mode-on)
;;          (lisp-interaction-mode . yas-minor-mode-on)
;;          (c-mode . yas-minor-mode-on)
;;          (c++-mode . yas-minor-mode-on)))

;; (use-package highlight-symbol
;;   :straight t
;;   :hook ((emacs-lisp-mode . highlight-symbol-mode)))

;; (use-package restclient
;;   :straight t
;;   :config
;;   (use-package company-restclient :straight t)
;;   (use-package restclient-test :straight t))

(use-package org
  :straight org
  :hook ((org-mode . turn-off-indent-tabs-mode)
         (org-mode . visual-line-mode)
         (org-mode . turn-off-indent-tabs-mode))
  :after sly
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (emacs-lisp . t)
     (java . t)
     (plantuml . t)))
  (setq org-babel-lisp-eval-fn 'sly-eval)
  :after (sly))

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))

;; (use-package org-present
;;   :straight t
;;   :config
;;   (add-hook 'org-present-mode-hook 'my-turn-off-org-present)
;;   (add-hook 'org-present-mode-quit-hook 'my-turn-off-org-present)
;;   (defun my-turn-on-org-present ()
;;     (org-present-big)
;;     (org-display-inline-images))
;;   (defun my-turn-off-org-present ()
;;     (org-present-small)
;;     (org-remove-inline-images))
;;   (use-package ob-tangle)
;;   (use-package epresent :straight t))

;; (use-package plantuml-mode
;;   :straight t
;;   :config
;;   (setq plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
;;   (use-package flycheck-plantuml :straight t))

;; (use-package hy-mode :straight t)
;; (use-package terraform-mode :straight t)
;; (use-package forth-mode :straight t)
;; (use-package erlang :straight t)
;; (use-package lfe-mode :straight t)
;; (use-package elixir-mode :straight t)

;; (use-package lsp-mode :straight t)
;; (use-package dap-mode :straight t)

;; (use-package go-mode
;;   :straight t
;;   :hook ((go-mode . gofmt-before-save)
;;          (go-mode . lsp-deferred)
;;          (go-mode . (lambda () (setq tab-width 4)))
;;          (go-mode . flycheck-mode)
;;          (go-mode . yas-minor-mode-on)))

;; (use-package sh-mode
;;   :hook (sh-mode . yas-minor-mode-on))

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((nasm-basic-offset . 2)
     (nasm-basic-offset . 4)
     (project-vc-merge-submodules))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "outline" :slant normal :weight normal :height 140 :width normal)))))

;; (use-package sly
;;   :straight t
;;   :init
;;   (setq sly-ignore-protocol-mismatches t
;;         sly-auto-start 'always
;;         sly-mrepl-pop-sylvester nil)
;;   (cond
;;    ((eq system-type 'windows-nt)
;;     ;; Prefixing with "cmd" allows SDL2, IUP and other graphical applications to
;;     ;; start from Sly
;;     (setq sly-lisp-implementations
;;           '((ccl ("cmd" "/c" "wx86cl64"))
;; 	    (sbcl ("cmd" "/c" "sbcl.exe" "--dynamic-space-size" "2048")))))
;;    ((eq system-type 'gnu/linux)
;;     (setq sly-lisp-implementations
;;           '((ccl ("lx86cl64"))
;;             (sbcl ("sbcl" "--dynamic-space-size" "2048"))
;;             (ecl ("ecl")))))
;;    ((eq system-type 'darwin)
;;     (setq sly-lisp-implementations
;;           '((ccl ("/usr/local/bin/ccl64"))
;;             (sbcl ("/usr/local/bin/sbcl" "--dynamic-space-size" "2048"))))))
;;   :hook ((sly-mode . company-mode)
;;          (sly-mode . show-paren-mode)
;;          (sly-mrepl-mode . company-mode)
;;          (sly-mrepl-mode . show-paren-mode))
;;   :bind (:map sly-mode-map ("TAB" . company-indent-or-complete-common))
;;   :config (setq inferior-lisp-program "sbcl"
;;                 sly-default-lisp 'sbcl))

;;; -*- lexical-binding: t -*-

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message (user-login-name)
      ring-bell-function 'ignore
      blink-matching-paren nil
      default-directory "~/")

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package bind-key :straight t)
(use-package delight :straight t)

(use-package ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :straight t)

(use-package fic-mode
  :straight t
  :config (add-hook 'prog-mode-hook 'fic-mode))

(use-package flycheck
  :straight t
  :config
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

(use-package dired
  :config
  (add-hook 'dired-mode-hook 'hl-line-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (use-package dired-x
    :bind (:map dired-mode-map ("M-o" . dired-omit-mode))
    :config
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
    (when (eq system-type 'windows-nt)
      (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^ntuser.*\\|NTUSER.*")))
  (when (member system-type '(gnu/linux darwin))
    (use-package dired-atool
      :straight t
      :bind (:map dired-mode-map
                  ("z" . dired-atool-do-unpack)
                  ("Z" . dired-atool-do-pack)))))

(use-package dockerfile-mode :straight t)
(use-package ag :straight t)
(use-package ripgrep :straight t)

(use-package ggtags
  :straight t
  :config
  (add-hook 'c-mode-hook 'ggtags-mode)
  (add-hook 'c++-mode-hook 'ggtags-mode))

(use-package company
  :straight t
  :config
  (setq company-tooltip-align-annotations t))

(use-package company-quickhelp
  :straight t
  :hook (add-hook 'company-mode-hook 'company-quickhelp-mode))

(use-package sly
  :straight t
  :config
  (setq sly-ignore-protocol-mismatches t
        sly-auto-start 'always)
  (cond
   ((eq system-type 'windows-nt)
    ;; Prefixing with "cmd" allows SDL2, IUP and other
    ;; graphical applications to start from SLIME
    (setq sly-lisp-implementations
          '((ccl ("cmd" "/c" "wx86cl64"))
	    (sbcl ("cmd" "/c" "c:/program files/steel bank common lisp/2.0.0/sbcl.exe" "--dynamic-space-size" "2048")))))
   ((eq system-type 'gnu/linux)
    (setq sly-lisp-implementations
          '((ccl ("lx86cl64"))
            (sbcl ("sbcl" "--dynamic-space-size" "2048"))
            (ecl ("ecl")))))
   ((eq system-type 'darwin)
    (setq sly-lisp-implementations
          '((ccl ("/usr/local/bin/ccl64"))
            (sbcl ("/usr/local/bin/sbcl" "--dynamic-space-size" "2048"))))))
  (add-hook 'sly-mode-hook 'company-mode)
  (add-hook 'sly-mode-hook 'show-paren-mode)
  (add-hook 'sly-mrepl-mode-hook 'company-mode)
  (add-hook 'sly-mrepl-mode-hook 'show-paren-mode)
  (define-key sly-mode-map (kbd "TAB") 'company-indent-or-complete-common))

(use-package magit
  :straight t
  ;; :after (ido)
  :bind (("C-x g" . magit-status))
  :config
  ;; (setq magit-completing-read-function 'magit-ido-completing-read)
  ;; From https://github.com/dgutov/diff-hl 
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package projectile
  :straight t 
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

(use-package which-key
  :straight t
  :config (which-key-mode)
  :delight)

(use-package editorconfig
  :straight t
  :config (editorconfig-mode)
  :delight)

(use-package yaml-mode :straight t)
(use-package hy-mode :straight t)

(use-package paredit
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'sly-mrepl-mode-hook 'enable-paredit-mode)
  (add-hook 'sly-repl-mode-hook
            (lambda ()
              (define-key sly-mrepl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil)))
  ;; (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  ;; (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  )

(use-package highlight-symbol :straight t)

(add-hook 'lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'org-mode-hook (lambda () (setq indent-tabs-mode nil)))

(use-package diff-hl
  :straight t
  :config (global-diff-hl-mode))

(use-package anzu
  :straight t
  :config
  (global-anzu-mode 1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  :delight)

;; (use-package clojure-mode :ensure t)
;; (use-package cider
;;   :after (clojure-mode)
;;   :ensure t
;;   :bind (("C-c M-j" . cider-jack-in)
;;          ("C-c M-J" . cider-jack-in-clojurescript)
;;       ("C-c M-c" . cider-connect))
;;   :config
;;   (setq cider-default-repl-command "lein"))
;; (use-package clj-refactor
;;   :after (clojure-mode cider)
;;   :ensure t
;;   :config
;;   (add-hook 'clojure-mode-hook 'clj-refactor-mode)
;;   (add-hook 'cider-repl-mode-hook 'clj-refactor-mode)
;;   (cljr-add-keybindings-with-prefix "C-c m"))

;; (use-package ido
;;   :straight t 
;;   :config
;;   (ido-mode 1)
;;   (ido-everywhere 1)
;;   (use-package ido-completing-read+
;;     :straight t
;;     :config
;;     (ido-ubiquitous-mode 1))
;;   (use-package flx-ido
;;     :straight t
;;     :config
;;     (flx-ido-mode 1)
;;     (setq ido-enable-flex-matching t
;;           id-use-faces nil)))

(use-package smex
  :straight t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
  :config
  (smex-initialize))

(use-package recentf
;;  :bind (("C-x f" . recentf-ido-find-file))
  :config
  (use-package recentf-ext :straight t)
  (recentf-mode 1))
;; (defun recentf-ido-find-file ()
;;   "Find a recent file using Ido."
;;   (interactive)
;;   (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
;;     (when file
;;       (find-file file))))

(use-package restclient
  :straight t
  :config
  (use-package company-restclient :straight t)
  (use-package restclient-test :straight t))

(use-package lua-mode :straight t)

(defun my-turn-on-org-present ()
  (org-present-big)
  (org-display-inline-images))

(defun my-turn-off-org-present ()
  (org-present-small)
  (org-remove-inline-images))

(use-package org-present
  :straight t
  :config
  (add-hook 'org-present-mode-hook 'my-turn-off-org-present)
  (add-hook 'org-present-mode-quit-hook 'my-turn-off-org-present))
(use-package ob-tangle)
(use-package ob-clojure
  :config
  (setq org-babel-clojure-backend 'cider))

(use-package ob-J)

(use-package epresent :straight t)

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

;; (use-package org
;;   :ensure org-plus-contrib
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((lisp . t)
;;      (emacs-lisp . t)
;;      (clojure . t)
;;      (java . t)
;;      (J . t)
;;      (plantuml . t)))
;;   (add-hook 'org-mode-hook 'visual-line-mode))


(use-package terraform-mode :straight t)

(use-package org
  :straight org-plus-contrib
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (emacs-lisp . t)
     (clojure . t)
     (java . t)
     (J . t)
     (plantuml . t)))
  (add-hook 'org-mode-hook 'visual-line-mode))

(setq org-babel-lisp-eval-fn 'sly-eval)

(use-package plantuml-mode
  :straight t
  :config (setq plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar")))

(use-package flycheck-plantuml :straight t)

;; (use-package edit-server
;;   :ensure t
;;   :config   (when (require 'edit-server nil t)
;;               (setq edit-server-new-frame nil)
;;               (edit-server-start)))

(use-package yaml-mode :straight t)
(use-package hy-mode :straight t)
(use-package elvish-mode :straight t)

(use-package selectrum
  :straight t
  :config
  (selectrum-mode 1)
  (use-package selectrum-prescient
    :straight t
    :config 
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1)))

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; (setq custom-file "~/.emacs-custom.el")
;; (ignore-errors (load custom-file))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

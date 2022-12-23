;;; -*- lexical-binding: t -*-

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package el-patch :straight t)
(use-package bind-key :straight t)
(use-package delight :straight t)
(use-package s :straight t)

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

(use-package flycheck
  :straight t
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :config
  (global-flycheck-mode))


;; (use-package vterm
;;   :custom (vterm-always-compile-module t)
;;   :straight t)

(use-package emacs-mode
  ;; :hook ((emacs-lisp-mode . turn-off-indent-tabs-mode))
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
  :bind ( ;; ("TAB" . company-indent-or-complete-common)
         :map company-active-map
         ("M-/" . company-complete)
         ("TAB" . company-complete-common-or-cycle)
         ("<backtab>" . (lambda () (interactive) (company-complete-common-or-cycle))))
  ;; :hook (emacs-lisp-mode . company-mode)
  :init (setq company-tooltip-align-annotations t)
  :config (global-company-mode)
  (use-package company-quickhelp
    :straight t
    :hook (company-mode . company-quickhelp-mode)))

(use-package sly
  :straight t
  :init
  (setq sly-ignore-protocol-mismatches t
        sly-auto-start 'always
        sly-mrepl-pop-sylvester nil)
  (cond
   ((eq system-type 'windows-nt)
    ;; Prefixing with "cmd" allows SDL2, IUP and other graphical applications to
    ;; start from Sly
    (setq sly-lisp-implementations
          '((ccl ("cmd" "/c" "wx86cl64"))
            (sbcl ("cmd" "/c" "sbcl.exe" "--dynamic-space-size" "2048")))))
   ((eq system-type 'gnu/linux)
    (setq sly-lisp-implementations
          '((ccl ("lx86cl64"))
            (sbcl ("sbcl" "--dynamic-space-size" "2048"))
            (ecl ("ecl")))))
   ((eq system-type 'darwin)
    (setq sly-lisp-implementations
          `((ccl ("/usr/local/bin/ccl64"))
            (sbcl ("sbcl" "--dynamic-space-size" "2048"))
            (abcl ,(let ((java-home (with-temp-buffer
                                      (call-process "/usr/libexec/java_home" nil t nil "-F" "-v" "17")
                                      (s-chomp (buffer-string)))))
                     (list (format "%s/bin/java" java-home) "-jar" (expand-file-name "~/.local/abcl-bin-1.9.0/abcl.jar"))))
            (ecl ("ecl"))))))
  :hook ((sly-mode . show-paren-mode)
         (sly-mrepl-mode . show-paren-mode))
;;;  :bind (:map sly-mode-map ("TAB" . company-indent-or-complete-common))
  :config (setq inferior-lisp-program "sbcl"
                sly-default-lisp 'sbcl))

(use-package platformio-mode :straight t)

;; (use-package ggtags
;;   :straight t
;;   :hook ((c-mode . ggtags-mode)
;;          (c++-mode . ggtags-mode)))

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
   (sly-mrepl-mode . enable-paredit-mode))
  ;;  (sly-mrepl-mode . (lambda ()
  ;;                           (define-key sly-mrepl-mode-map
  ;;                             (read-kbd-macro paredit-backward-delete-key) nil))))
  :bind ((:map lisp-mode-map
               ("M-<left>"  . paredit-backward-slurp-sexp)
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

;; (use-package yasnippet
;;   :straight t
;;   :commands (yas-minor-mode-on)
;;   :hook ((emacs-lisp-mode . yas-minor-mode-on)
;;          (lisp-mode . yas-minor-mode-on)
;;          (lisp-interaction-mode . yas-minor-mode-on)
;;          (c-mode . yas-minor-mode-on)
;;          (c++-mode . yas-minor-mode-on)))

;; (use-package highlight-symbol
;;   :straight t
;;   :hook ((emacs-lisp-mode . highlight-symbol-mode)
;;          (lisp-mode . highlight-symbol-mode)))

;; ;; (use-package restclient
;; ;;   :straight t
;; ;;   :config
;; ;;   (use-package company-restclient :straight t)
;; ;;   (use-package restclient-test :straight t))

(use-package exec-path-from-shell
  :straight t
  :if (or (memq window-system '(mac ns))
          (memq system-type '(gnu/linux)))
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix  "C-c l")
  :config  (setq lsp-enable-which-key-integration t)
  ;; :hook ((go-mode) . lisp)
)

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t))

(use-package ccls
  :straight t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

;; (use-package dap-mode
;;   :straight t
;;   :config
;;   (dap-auto-configure-mode 1)
;;   (use-package dap-lldb
;;     :custom ((dap-lldb-debug-program "/usr/local/Cellar/llvm/15.0.6/bin/lldb-vscode"))))

;; (add-to-list 'load-path "~/.emacs.d/elisp/plisp-mode")

;; ;; (use-package plantuml-mode
;; ;;   :straight t
;; ;;   :config
;; ;;   (setq plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
;; ;;   (use-package flycheck-plantuml :straight t))

;; (use-package go-mode
;;   :straight t
;;   :hook ((go-mode . gofmt-before-save)
;;          (go-mode . lsp-deferred)
;;          (go-mode . (lambda () (setq tab-width 4)))
;;          (go-mode . flycheck-mode)
;;          (go-mode . yas-minor-mode-on)))

;; ;; (use-package sh-mode
;; ;;   :hook (sh-mode . yas-minor-mode-on))

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; (use-package xonsh-mode
;;   :straight t
;;   :mode ("\\.xsh\\'" "\\.xonshrc\\'" "xonshrc"))

;; ;; (use-package em-smart
;; ;;   :after eshell
;; ;;   :config
;; ;;   (setq eshell-where-to-jump 'begin)
;; ;;   (setq eshell-review-quick-commands nil)
;; ;;   (setq eshell-smart-space-goes-to-end t))

;; (use-package eshell)

(load "~/.emacs.d/init-org.el")

(load (setq custom-file
            (format "~/.emacs.d/%s-custom.el"
                    (string-replace "/" "" (symbol-name system-type)))))

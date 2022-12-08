
;; (use-package org-roam
;;   :straight t
;;   :init (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-directory (expand-file-name "~/Documents/Roam"))
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;;          ("C-c n j" . org-roam-dailies-capture-today)
;;          :map org-mode-map
;;          ("C-M-i" . completion-at-point))
;;   :config
;;   (org-roam-setup)
;;   (org-roam-db-autosync-mode))

;; (use-package deft
;;   :straight t
;;   :after (org org-roam)
;;   :bind ("C-c n d" . deft)
;;   :custom
;;   (deft-recursive t)
;;   (deft-use-filter-string-for-filename t)
;;   (deft-default-extension "org")
;;   (deft-directory org-roam-directory))

;; (use-package org-download
;;   :straight t
;;   :after org
;;   :bind (:map org-mode-map
;;               (("s-Y" . org-download-screenshot)
;;                ("s-y" . org-download-yank))))

; (use-package org
;;   :straight org
;;   :hook ((org-mode . turn-off-indent-tabs-mode)
;;          (org-mode . visual-line-mode)
;;          (org-mode . turn-off-indent-tabs-mode))
;;   :after sly
;;   :config
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((lisp . t)
;;      (emacs-lisp . t)
;;      (java . t)
;;      (plantuml . t)))
;;   (setq org-babel-lisp-eval-fn 'sly-eval)
;;   :after (sly))

;; ;; (use-package org-present
;; ;;   :straight t
;; ;;   :config
;; ;;   (add-hook 'org-present-mode-hook 'my-turn-off-org-present)
;; ;;   (add-hook 'org-present-mode-quit-hook 'my-turn-off-org-present)
;; ;;   (defun my-turn-on-org-present ()
;; ;;     (org-present-big)
;; ;;     (org-display-inline-images))
;; ;;   (defun my-turn-off-org-present ()
;; ;;     (org-present-small)
;; ;;     (org-remove-inline-images))
;; ;;   (use-package ob-tangle)
;; ;;   (use-package epresent :straight t))


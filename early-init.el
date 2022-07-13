;; To set the garbage collection threshold to high (100 MB) since LSP client-server communication generates a lot of output/garbage
(setq gc-cons-threshold 100000000)
;; To increase the amount of data Emacs reads from a process
(setq read-process-output-max (* 1024 1024))
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (unless window-system (menu-bar-mode -1))

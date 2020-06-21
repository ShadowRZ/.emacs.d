;;; init-ui.el --- UI inits.
;;; Commentary:

;;; Code:
;; Fonts

(set-fontset-font "fontset-default" '(#xe0a0 . #xe0a2) "PowerlineSymbols")
(set-fontset-font "fontset-default" '(#xe0b0 . #xe0b3) "PowerlineSymbols")

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-frame-parameter nil 'fullscreen 'fullboth)
(setq-default cursor-type 'box)

(use-package moe-theme
  :ensure t
  :config
  (setq moe-theme-highlight-buffer-id t)
  (when (package-installed-p 'markdown-mode)
    (setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0)))
  (setq moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0))
  (setq moe-theme-resize-rst-title '(2.0 1.7 1.5 1.3 1.1 1.0))
  (moe-dark)
  (moe-theme-set-color 'orange))

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup)
  (sml/apply-theme 'light))
(provide 'init-ui)
;;; init-ui.el ends here

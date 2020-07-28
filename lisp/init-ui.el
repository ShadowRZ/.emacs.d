;;; init-ui.el --- UI inits.
;;; Commentary:

;;; Code:
;; Fonts
(if (display-graphic-p)
    (progn
      (set-fontset-font "fontset-default" '(#xe0a0 . #xe0a2) "PowerlineSymbols")
      (set-fontset-font "fontset-default" '(#xe0b0 . #xe0b3) "PowerlineSymbols")))

(tool-bar-mode -1)
(menu-bar-mode -1)
(if (display-graphic-p)
     (scroll-bar-mode -1))

(set-frame-parameter nil 'fullscreen 'fullboth)
(setq-default cursor-type 'box)

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  :config
  (sml/setup)
  (setq sml/theme 'respectful))

(use-package base16-theme)

(provide 'init-ui)
;;; init-ui.el ends here

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

(provide 'init-ui)
;;; init-ui.el ends here

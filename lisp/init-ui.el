;;; init-ui.el --- UI inits.
;;; Commentary:

;;; Code:
;; Fonts
(set-fontset-font "fontset-default" 'symbol "Noto Emoji")
(set-fontset-font "fontset-default" 'unicode "Noto Sans Mono")
(set-fontset-font "fontset-default" 'han "Noto Sans Mono CJK SC")
(set-fontset-font "fontset-default" 'hangul "Noto Sans Mono CJK KR")
(set-fontset-font "fontset-default" 'kana "Noto Sans Mono CJK JP")
(set-fontset-font "fontset-default" 'cjk-misc "Noto Sans Mono CJK SC")

(set-fontset-font "fontset-default" '(#xe0a0 . #xe0a2) "PowerlineSymbols")
(set-fontset-font "fontset-default" '(#xe0b0 . #xe0b3) "PowerlineSymbols")

;; Theme
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq initial-frame-alist (quote ((fullscreen . maximized))))
(setq-default cursor-type 'bar)

(provide 'init-ui)
;;; init-ui.el ends here

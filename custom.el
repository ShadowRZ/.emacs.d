;;; custom.el --- Customs
;;; Commentary:

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   (quote
    ("https://www.solidot.org/index.rss" "http://planet.emacsen.org/atom.xml" "https://blog.yoitsu.moe/feeds/all.atom.xml")))
 '(elfeed-search-filter "@3-month-ago")
 '(emms-lyrics-scroll-p nil)
 '(emms-player-list (quote (emms-player-mpv emms-player-mikmod)))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/org")))
 '(org-src-fontify-natively t)
 '(package-selected-packages
   (quote
    (solarized-theme solarized-light which-key org-projectile counsel-projectile markdown-mode markdown-mode+ dashboard magit use-package yaml-mode cask-mode pallet company swiper flycheck counsel smartparens docbook popwin elfeed adoc-mode js2-mode web-mode nodejs-repl exec-path-from-shell)))
 '(recentf-max-menu-item 10)
 '(which-key-idle-delay 0.5))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight semibold :height 98 :family "Source Code Pro"))))
 '(variable-pitch ((t nil))))
;;; custom.el ends here

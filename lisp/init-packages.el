;;; init-package.el --- Packages init.
;;; Commentary:

;;; Code:

;; Requires
(use-package recentf
  :config
  (recentf-mode 1))

(use-package org)

;; - EMMS.
(use-package emms)
(use-package emms-setup
  :config
  (emms-all)
  :after (emms))
(use-package emms-lyrics
  :config
  (emms-lyrics 1)
  :after (emms))
(use-package emms-source-playlist
  :config
  (emms-default-players)
  :after (emms))

(use-package dired-x)

(use-package popwin
  :config
  (popwin-mode 1))

(use-package elfeed)

(delete-selection-mode 1)
(global-hl-line-mode 1)
(ivy-mode 1)
(counsel-mode 1)
(abbrev-mode 1)
(global-flycheck-mode 1)
(global-company-mode 1)
(global-auto-revert-mode 1)
(rcirc-track-minor-mode 1)

(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       '(("\\.html\\'" . web-mode))
       auto-mode-alist))

(provide 'init-packages)
;;; init-packages.el ends here

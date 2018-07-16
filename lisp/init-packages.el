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
  :ensure t
  :config
  (popwin-mode 1))

(use-package elfeed)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package wl
  :commands (wl wl-other-frame))

(use-package wl-draft
  :commands (wl-draft wl-user-agent-compose)
  :config
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook)))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))
(use-package counsel
  :ensure t
  :config
  (counsel-mode 1))
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode 1))
(use-package company
  :ensure t
  :config
  (global-company-mode 1))
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode 1))
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom))

(delete-selection-mode 1)
(global-hl-line-mode 1)
(abbrev-mode 1)
(global-auto-revert-mode 1)
(rcirc-track-minor-mode 1)

(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       '(("\\.html\\'" . web-mode))
       auto-mode-alist))

(provide 'init-packages)
;;; init-packages.el ends here

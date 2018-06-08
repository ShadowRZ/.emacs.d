;;; init-package.el --- Packages init.
;;; Commentary:

;;; Code:
(require 'cl)

;; Requires
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

(require 'org)
(setq org-src-fontify-natively t)

;; - EMMS.
(require 'emms)
(require 'emms-setup)
(emms-all)
(require 'emms-lyrics)
(emms-lyrics 1)
(require 'emms-source-playlist)
(emms-default-players)

;; - WanderLust.
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(autoload 'wl-user-agent-compose "wl-draft" nil t)

;; - Emacs-Muse related.
(require 'muse-mode)
(require 'muse-html)
(require 'muse-latex)
(require 'muse-context)
(require 'muse-project)

(require 'dired-x)

(require 'popwin)
(popwin-mode 1)

(delete-selection-mode 1)
(global-hl-line-mode 1)
(ivy-mode 1)
(counsel-mode 1)
(abbrev-mode 1)
(global-flycheck-mode 1)
(global-company-mode 1)
(global-auto-revert-mode 1)

(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       '(("\\.html\\'" . web-mode))
       auto-mode-alist))

(provide 'init-packages)
;;; init-packages.el ends here

;;; init-package.el --- Packages init.
;;; Commentary:

;;; Code:
(require 'cl)

(defvar ShadowRZ/packages
  '(
    company
    swiper
    flycheck
    counsel
    smartparens
    docbook
    popwin
    elfeed
    powerline
    moe-theme
    adoc-mode
    js2-mode
    web-mode
    nodejs-repl
    exec-path-from-shell
    ) "Default packages.")

;; package.el
(when (>= emacs-major-version 24)
  (setq package-archives
	'(("gnu"   . "https://elpa.gnu.org/packages/")
	  ("melpa-stable" . "https://stable.melpa.org/packages/"))))

(setq package-selected-packages ShadowRZ/packages)

(defun ShadowRZ/packages-installed-p ()
  "Check if all defined packages are installed."
  (loop for pkg in ShadowRZ/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(unless (ShadowRZ/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg ShadowRZ/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

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

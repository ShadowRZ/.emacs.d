;;; init.el --- Emacs Init file.
;;; Commentary:

;;; Code:

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'init-packages)
(require 'init-ui)
(require 'init-title)
(require 'init-bindings)
(require 'init-org)
(require 'init-elfeed)

;; Mac Hack :)
(when (memq window-system '(mac ns))
  (if (package-installed-p exec-path-from-shell)
      (package-install exec-path-from-shell t))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Constants / Loads ?
(setq custom-file "~/.emacs.d/custom.el")
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(setq auto-save-default nil)
(setq org-mime-library 'semi)
(setq inhibit-startup-echo-area-message "shadowrz")
(setq inhibit-startup-screen t)
(setq emms-player-list '(emms-player-mpv emms-player-mikmod))
(setq erc-prompt "ShadowRZ-Desktop.Infinity.Stream >")
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq emms-lyrics-scroll-p nil)
(fset 'yes-or-no-p 'y-or-n-p)
(load custom-file)

(put 'dired-find-alternate-file 'disabled nil)

;;; init.el ends here

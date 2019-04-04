;;; init.el --- Emacs Init file.
;;; Commentary:

;;; Code:

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(require 'init-packages)
(require 'init-ui)
(require 'init-title)
(require 'init-bindings)

;; Mac Hack :)
(when (memq window-system '(mac ns))
  (if (package-installed-p exec-path-from-shell)
      (package-install exec-path-from-shell t))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; Constants / Loads ?
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(setq
  make-backup-files nil
ring-bell-function 'ignore
auto-save-default nil
ivy-use-virtual-buffers t
   ivy-count-format "(%d/%d) -> "
   ivy-use-selectable-prompt t

)
(fset 'yes-or-no-p 'y-or-n-p)

(put 'dired-find-alternate-file 'disabled nil)

;;; init.el ends here

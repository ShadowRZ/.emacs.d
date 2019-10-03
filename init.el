;;; init.el --- Emacs Init file.
;;; Commentary:

;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (setq package-archives 'nil)
  (add-to-list 'package-archives
	       (cons "gnu" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")) t)
  (add-to-list 'package-archives
	       (cons "melpa-stable" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")) t))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Default loads.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; Place default.el here. (info "(emacs) Init File")
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

(setq
  make-backup-files nil
  ring-bell-function 'ignore
  auto-save-default nil
  rmail-file-name "~/.cache/rmail"
  ivy-use-virtual-buffers t
  ivy-count-format "(%d/%d) -> "
  ivy-use-selectable-prompt t
  org-time-stamp-custom-formats '("<%F>". "<%F %T%z>"))
(fset 'yes-or-no-p 'y-or-n-p)

(put 'dired-find-alternate-file 'disabled nil)

;;; init.el ends here

;;; title.el --- Set window title.
;;; Commentary:

;;; Code:
(setq-default frame-title-format
	      '(:eval
		(format "[%d]%s@%s"
			(emacs-pid)
			(user-login-name)
			(system-name))))

(provide 'init-title)
;;; title.el ends here

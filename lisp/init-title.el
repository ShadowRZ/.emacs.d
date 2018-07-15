;;; title.el --- Set window title.
;;; Commentary:

;;; Code:
(setq-default frame-title-format
	      '(:eval
		(format "[%d]@%s"
			(emacs-pid)
			(system-name))))

(provide 'init-title)
;;; title.el ends here

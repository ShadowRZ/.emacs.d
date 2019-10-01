;;; init-title.el --- Set window title.
;;; Commentary:

;;; Code:
(setq-default frame-title-format
	      '(:eval
		(format "[%d]@%s - %s"
			(emacs-pid)
			(system-name)
			(current-buffer))))

(provide 'init-title)
;;; init-title.el ends here

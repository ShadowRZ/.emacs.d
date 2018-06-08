;;; init-org.el --- Org inits.
;;; Commentary:

;;; Code:
(require 'org)
(setq org-src-fontify-natively t)

(setq org-agenda-files '("~/org"))
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-feed-alist
      '(("Solidot"
	 "https://www.solidot.org/index.rss"
	 "~/org/feeds.org"
	 "Solidot")))

(require 'ox-man)

(provide 'init-org)
;;; init-org.el ends here

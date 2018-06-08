;;; init-elfeed.el --- Elfeed Init file.
;;; Commentary:

;;; Code:
(require 'elfeed)

(setq elfeed-feeds
      '("https://www.solidot.org/index.rss"
        "http://planet.emacsen.org/atom.xml"
	"https://blog.yoitsu.moe/feeds/all.atom.xml"))

(setq-default elfeed-search-filter "@3-month-ago")

(provide 'init-elfeed)
;;; init-elfeed.el ends here

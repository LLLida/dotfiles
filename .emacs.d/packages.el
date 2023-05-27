;;
;; Additional packages that I use rarely
;;

(use-package bluetooth)

(use-package elfeed
  :init
  (setq elfeed-feeds '(("https://www.reddit.com/r/emacs.rss" reddit emacs)
                       ("https://hackaday.com/blog/feed/" hackaday linux)
                       ("https://itsfoss.com/feed/" itsfoss linux)
                       ("https://www.phoronix.com/rss.php" phoronix linux))))

(use-package olivetti)

(use-package tuareg)

(use-package magit)

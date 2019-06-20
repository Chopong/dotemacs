;;; init-elfeed.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-21,Tue,13:48:39
;;; Commentary:

;;; Code:

(use-package elfeed :ensure t :defer t
  :bind ("C-x w" . 'elfeed)
  :config
  (setq elfeed-feeds
        '(("https://chopong.github.io/sundry/feed.xml" Chopong blog)
          ("https://change-y.github.io/atom.xml" hackintosh)
          "https://chopong.github.io/sundry/feed.xml")))





(use-package elfeed-goodies :ensure t :defer t
  :config
  (elfeed-goodies/setup))


(provide 'init-elfeed)
;;; init-elfeed.el ends here

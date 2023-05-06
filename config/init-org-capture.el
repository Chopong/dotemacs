;;; init-org-capture.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-21,Tue,17:31:19
;;; Commentary:

;;; Code:

;; (require 'org-protocol-capture-html)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/todo.org" "Todo soon")
         "* TODO %? \n  %^t")
        ("i" "Idea" entry (file+headline "~/Dropbox/org/ideas.org" "Ideas")
         "* %? \n %U")
        ("e" "Tweak" entry (file+headline "~/Dropbox/org/tweaks.org" "Tweaks")
         "* %? \n %U")
        ("l" "Learn" entry (file+headline "~/Dropbox/org/learn.org" "Learn")
         "* %? \n")
        ("w" "Work note" entry (file+headline "~/Dropbox/org/work.org" "Work")
         "* %? \n")
        ("m" "Check movie" entry (file+headline "~/Dropbox/org/check.org" "Movies")
         "* %? %^g")
        ("n" "Check book" entry (file+headline "~/Dropbox/org/check.org" "Books")
         "* %^{book name} by %^{author} %^g")))

(provide 'init-org-capture)
;;; init-org-capture.el ends here

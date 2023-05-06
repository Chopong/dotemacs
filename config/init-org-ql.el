;;; init-org-ql.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-06-29,Sat,20:26:20
;;; Commentary:

;;; Code:

;; (use-package org-ql :ensure t :defer t)

(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (setq org-projectile-projects-file
          "/Volumes/MacFile/Notebook/projects.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "inbox.org" "Tasks")
             "* TODO %?\n  %i\n  %u\n  %a")
            ("n" "Note/Data" entry (file+headline "inbox.org" "Notes/Data")
             "* %?   \n  %i\n  %u\n  %a")
            ("j" "Journal" entry (file+datetree "~/org/journal.org")
             "* %?\nEntered on %U\n %i\n %a")
            ("J" "Work-Journal" entry (file+datetree "~/org/wjournal.org")
             "* %?\nEntered on %U\n %i\n %a")
            ))
    (setq org-irc-link-to-logs t)
    (push (org-projectile-project-todo-entry) org-capture-templates)
    )
  :ensure t)

(use-package org-id :defer t
  :init
  (setq org-id-link-to-org-use-id 'create-if-interactive))



(provide 'init-org-ql)
;;; init-org-ql.el ends here

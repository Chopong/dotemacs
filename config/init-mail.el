;;; init-mail.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-07-10,Wed,14:57:02
;;; Commentary:

;;; Code:

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(use-package mu4e :commands mu4e
  :config
  (setq mu4e-maildir "~/Documents/Email")
  (setq mu4e-drafts-folder "/Inbox/")
  (setq mu4e-sent-folder "/Sent/")
  (setq mu4e-trash-folder "/Trash/"))




(provide 'init-mail)
;;; init-mail.el ends here

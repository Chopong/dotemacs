;;; init-org-pomodoro.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,20:43:34
;;; Commentary:

;;; Code:


(use-package org-pomodoro :ensure t :defer t
  :after org
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))
  )



(provide 'init-org-pomodoro)
;;; init-org-pomodoro.el ends here

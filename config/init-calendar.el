;;; init-calendar.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-21,Tue,13:37:40
;;; Commentary:

;;; Code:

(display-time-mode 1)
(setq display-time-24hr-format t
      display-time-day-and-date t
      display-time-use-mail-icon t
      )





(use-package org-caldav :ensure t :defer t)

(use-package calfw :ensure t :defer t
  )
(use-package calfw-org :ensure t :defer t
  :config
  (setq cfw:org-overwrite-default-keybinding t)
  )



(provide 'init-calendar)
;;; init-calendar.el ends here

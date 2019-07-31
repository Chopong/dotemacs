;;; init-org-download.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-21,Tue,15:56:09
;;; Commentary:

;;; Code:

(use-package org-download :ensure t :defer t
  :init
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

(provide 'init-org-download)
;;; init-org-download.el ends here

;;; init-recentf.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,19:25:55
;;; Commentary:

;;; Code:

(use-package recentf :ensure nil :defer t
  :hook (after-init . recentf-mode)
  :config
  (setq-default  recentf-max-saved-items 1000
                 recentf-exclude '("/tmp/" "/ssh:"))
  (setq recentf-save-file (expand-file-name ".recentf" user-assets-directory))
  :bind ("C-x C-r" . 'recentf-open-files))

(provide 'init-recentf)
;;; init-recentf.el ends here

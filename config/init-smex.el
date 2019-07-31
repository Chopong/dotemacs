;;; init-smex.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,19:36:17
;;; Commentary:

;;; Code:

(use-package smex :ensure t :defer nil
  :config
  (setq-default smex-save-file
		(expand-file-name ".smex-items" user-assets-directory))
  :bind ([remap execute-extended-command] . 'smex))


(provide 'init-smex)
;; init-smex.el ends here

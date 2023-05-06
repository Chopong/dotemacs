;;; init-uniquify.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,00:22:21
;;; Commentary:

;;; Code:

(use-package uniquify :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
  )

(provide 'init-uniquify)
;;; init-uniquify.el ends here

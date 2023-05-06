;;; init-mmm.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,00:04:43
;;; Commentary:

;;; Code:

(use-package mmm-auto :ensure mmm-mode
  :config
  (setq mmm-global-mode 'buffers-with-submode-classes)
  (setq mmm-submode-decoration-level 2))

(use-package polymode :ensure t :defer t)
(use-package poly-markdown :ensure t :defer t)


(provide 'init-mmm)
;;; init-mmm.el ends here

;;; init-leetcode.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-06-29,Sat,21:27:25
;;; Commentary:

;;; Code:

(use-package leetcode :ensure t
  :defer t
  :config (setq leetcode-account "chopong")
  (setq leetcode-password "842071968@qq.com")
  (setq leetcode-prefer-language "python3")
  (setq leetcode-prefer-sql "mysql"))




(provide 'init-leetcode)
;;; init-leetcode.el ends here

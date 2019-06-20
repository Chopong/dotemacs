;;; init-elpa.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,19:03:31
;;; Commentary:

;;; Code:

(require 'package)

(setq package-archives
      '(("gnu"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	("melpa"        . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
	("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")))

(require 'cl-lib)


(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))



(provide 'init-elpa)
;;; init-elpa.el ends here

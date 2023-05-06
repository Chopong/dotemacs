;;; init-parameter.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,00:33:26
;;; Commentary:

;;; Code:

;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:


(setq debug-on-error nil)

;; gc
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; do not mixed up my folders.

(defconst user-assets-directory
  (expand-file-name "assets/" user-emacs-directory))
(setq-default default-directory "~/Documents/File/")
;; (setq-default default-directory user-emacs-directory)
(setq custom-file
      (expand-file-name "custom.el" user-assets-directory))

(setq user-mail-address "chopong@aliyun.com")
(setq user-full-name "Chopong")

(setq-default make-backup-files nil)
(setq-default fill-column 200)

(provide 'init-parameter)
;;; init-parameter.el ends here

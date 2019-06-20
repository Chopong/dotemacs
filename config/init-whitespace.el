;;; init-whitespace.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,11:21:22
;;; Commentary:

;;; Code:


(setq-default show-trailing-whitespace nil)


;;; Whitespace

(defun show-trailing-whitespace-func ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'show-trailing-whitespace-func))

(use-package whitespace-cleanup-mode :ensure t :defer t
  :diminish whitespace-cleanup-mode
  :hook (after-init . global-whitespace-cleanup-mode))

(global-set-key [remap just-one-space] 'cycle-spacing)




(provide 'init-whitespace)
;;; init-whitespace.el ends here

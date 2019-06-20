;;; init-dired.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,00:49:14
;;; Commentary:

;;; Code:

(setq-default dired-dwim-target t)

(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package dired   :ensure nil :defer 1
  :bind (:map dired-mode-map
	      ([mouse-2] . 'dired-find-file)
	      ("C-c C-q" . 'wdired-change-to-wdired-mode))
  :config
  (setq dired-recursive-deletes 'top))

(use-package diredfl :ensure t :defer 1
  :after dired
  :config
  (diredfl-global-mode))

(use-package diff-hl :ensure t :defer t
  :after dired
  :hook (dired-mode . diff-hl-dired-mode))


(provide 'init-dired)
;; init-dired.el ends here

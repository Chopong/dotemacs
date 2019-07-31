;;; init-mode-line.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,22:04:00
;;; Commentary:

;;; Code:

;;------------------------------------------------------------------------------
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;;------------------------------------------------------------------------------

;;; A simple visible bell which works in all terminal types

(use-package mode-line-bell :ensure t :defer 2
  :hook (after-init . mode-line-bell-mode))

(use-package nyan-mode :ensure t
  :init
  (add-to-list 'mode-line-format
               (list
                '(:eval (list (nyan-create)))
                ) t))

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (setq sml/theme 'respectful))

;; (use-package powerline :ensure t :defer t
;;   :config
;;   (powerline-default-theme)
;;   )

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

(use-package telephone-line :ensure t :defer t)

(provide 'init-mode-line)
;;; init-mode-line.el ends here

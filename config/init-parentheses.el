;;; init-parentheses.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,20:20:18
;;; Commentary:

;;; Code:

(use-package highlight-parentheses :ensure t :defer 2

  )


(use-package smartparens :ensure t :defer t
  :init
  (add-hook 'js-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  :config
  (require 'smartparens-config)
  )

;;sp-cheat-sheet

(provide 'init-parentheses)
;;; init-parentheses.el ends here

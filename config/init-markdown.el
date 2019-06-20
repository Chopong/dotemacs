;;; init-markdown.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,12:33:47
;;; Commentary:

;;; Code:

(use-package markdown-mode :ensure t :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\.html\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  (after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))
  )

(provide 'init-markdown)
;;; init-markdown.el ends here

;;; init-yasnippet.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,19:28:53
;;; Commentary:

;;; Code:


(use-package yasnippet :ensure t :defer t
  :hook ((prog-mode     . yas-minor-mode)
         (markdown-mode . yas-minor-mode)
         (python-mode   . yas-minor-mode)
	 (LaTeX-mode    . yas-minor-mode))
  :init
  (defcustom yas-snippet-dirs
    (list (expand-file-name "snippets/"
                            user-assets-directory)) "Fix the snippets dir")
  :config
  (yas-reload-all))

(use-package yasnippet-snippets :ensure t :defer t
  :after yasnippet
  )

(use-package autoinsert :ensure nil :after yasnippet
  :init
  (add-hook 'find-file-hook 'auto-insert)
  :config
  (defun yas-expand-auto-insert ()
    "auto-insert file"
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (setq auto-insert-query nil)
  (setq auto-insert-directory
        (expand-file-name "templete" user-assets-directory))
  (setq auto-insert-alist
        '(("\\.el$"       . ["templete.el"         yas-expand-auto-insert])
          ("\\.org$"      . ["templete-org.el"     yas-expand-auto-insert])
          ("\\.sh$"       . ["templete-sh.el"      yas-expand-auto-insert])
          ("[0-9]\\.md$"  . ["templete-blogmd.el"  yas-expand-auto-insert])
          ("\\.md$"       . ["templete-md.el"      yas-expand-auto-insert])
          ("\\.py$"       . ["templete-py.el"      yas-expand-auto-insert])
          ("\\.ml$"       . ["templete-ml.el"      yas-expand-auto-insert])
          ("bm\\.tex$"    . ["templete-beamer.el"  yas-expand-auto-insert])
          ("\\.tex$"      . ["templete-tex.el"     yas-expand-auto-insert])
          )))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here

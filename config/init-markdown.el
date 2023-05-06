;;; init-markdown.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,12:33:47
;;; Commentary:

;;; Code:

;; (use-package md-math :ensure nil :defer t
;;   :load-path "plugins/")

;;(add-hook 'markdown-mode-hook 'LaTeX-math-mode)
;; (add-hook 'markdown-mode-hook 'turn-on-md-math)
;; (autoload 'md-math-mode "md-math" "MdMath Mode" t)
;; (autoload 'turn-on-md-math "md-math" "MdMath Mode" nil)



(add-subdirs-to-load-path "~/.emacs.d/plugins")

(use-package markdown-mode :ensure t :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\.html\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  ;;  (use-package md-math-cd :ensure nil :defer t)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (require 'md-math-symbols)
              (setq-local prettify-symbols-alist md-math-prettify-symbols-alist)
              (require 'md-math)
              ;; (setq font-lock-defaults
              ;;       '(("?_{" . md-math-subscript)
              ;;         ("?^{" . md-math-superscript))
              ;;(md-math-mode)
              ;;(md-math-)
              ;;(markdown-preview-mode)
              )
            )
  (after-load 'whitespace-cleanup-mode
    (push 'markdown-mode whitespace-cleanup-mode-ignore-modes))

  (defun insert-br ()
    (interactive)
    (insert "<br>"))
  (define-key markdown-mode-map (kbd "C-c C-s r") 'insert-br)

  (defun insert-space ()
    (interactive)
    (insert "&nbsp;"))
  (define-key markdown-mode-map (kbd "C-c C-s b") 'insert-space)
  )

(use-package flymd :ensure t :defer t
  :init
  (defun my-flymd-browser-function (url)
    (let ((process-environment (browse-url-process-environment)))
      (apply 'start-process
             (concat "firefox " url)
             nil
             "/usr/bin/open"
             (list "-a" "firefox" url))))
  (setq flymd-browser-open-function 'my-flymd-browser-function)
  )


(provide 'init-markdown)
;;; init-markdown.el ends here

;;; init-icons.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-21,Tue,13:35:03
;;; Commentary:

;;; Code:

(add-to-list 'load-path "~/.local/share/icons-in-terminal")
;;(use-package ov :ensure t)
;; (use-package font-lock+ :ensure t :defer t)
;;(require 'sidebar)

(use-package all-the-icons :ensure t :defer t
  :init
  (setq inhibit-compacting-font-caches t))

;; (use-package company-box :ensure t :defer t)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package spaceline-all-the-icons :ensure t :defer t
  :after spaceline
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)
  (spaceline-all-the-icons--setup-package-updates)
  (spaceline-all-the-icons--setup-paradox)
  (spaceline-all-the-icons--setup-neotree))

(provide 'init-icons)
;;; init-icons.el ends here

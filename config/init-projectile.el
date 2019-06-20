;;; init-projectile.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,12:24:06
;;; Commentary:

;;; Code:

;;https://github.com/bbatsov/projectile

(use-package projectile :ensure t :defer t
  :hook (after-init . projectile-mode)
  ;; Shorter modeline
  :config
  (setq-default projectile-mode-line-prefix " Proj")
  (setq projectile-known-projects-file
        (expand-file-name ".projectile-bookmarks" user-assets-directory))
  :bind (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map)))

(use-package ibuffer-projectile :ensure t :defer 2)





(use-package projectile-rails :ensure t :defer t
  :config
  (add-hook 'projectile-mode-hook
            (lambda () (projectile-rails-global-mode projectile-mode))))

(provide 'init-projectile)
;;; init-projectile.el ends here

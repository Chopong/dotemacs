;;; init-pre-package.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,00:31:37
;;; Commentary:

;;; Code:

(use-package fullframe            :ensure t
  :config
  (fullframe list-packages quit-window))

(use-package exec-path-from-shell :ensure t
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (or (memq window-system '(mac ns x))
	    (unless (memq system-type '(ms-dos windows-nt))
	      (daemonp)))
    (exec-path-from-shell-initialize)))

;; (use-package benchmark-init :ensure t :defer nil
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package esup                 :ensure t :defer t)
(use-package scratch              :ensure t :defer t)
(use-package command-log-mode     :ensure t :defer t)
(use-package diminish             :ensure t :defer t)
;; (use-package hydra                :ensure t :defer t)

(provide 'init-pre-package)
;;; init-pre-package.el ends here

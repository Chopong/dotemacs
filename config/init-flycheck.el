;;; init-flycheck.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,00:44:01
;;; Commentary:

;;; Code:

(use-package flycheck                 :ensure t :defer nil
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  ;;(setq flycheck-global-modes '(not python-mode xx-mode))
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33)))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-emacs-lisp-package-user-dir
        (expand-file-name "elpa/" user-emacs-directory))
  ;; (use-package flycheck-color-mode-line
  ;;   :hook (flycheck-mode . flycheck-color-mode-line-mode))
  )

;; C-c ! v
;; C-c ! n
;; C-c ! p
;; C-c ! l
;; (try Shackle)

(use-package flycheck-color-mode-line :ensure t :defer t
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

;; (use-package flycheck-pos-tip
;;   :ensure t :defer t
;;   :hook (flycheck-mode . flycheck-pos-tip-mode))


;;(use-package flycheck-inline :ensure t)

(use-package flycheck-package :ensure t :defer t
  :after (flycheck elisp-mode)
  :config
  (flycheck-package-setup))

;; (use-package flycheck-pycheckers
;;   :ensure t :defer t)
;; (use-package flycheck-prospector
;;   :ensure t)
;; (use-package flycheck-pyflakes
;;   :ensure t)

(use-package flycheck-checkbashisms
  :ensure t :defer t
  :config
  (flycheck-checkbashisms-setup)
  ;; Check 'echo -n' usage
  (setq flycheck-checkbashisms-newline t)

  ;; Check non-POSIX issues but required to be supported  by Debian Policy 10.4
  ;; Setting this variable to non nil made flycheck-checkbashisms-newline effects
  ;; regardless of its value
  (setq flycheck-checkbashisms-posix t))

;;markdwon
;; (after-load 'flycheck
;;   (add-hook 'markdown-mode-hook (lambda ()
;;                                   (setq flycheck-markdown-mdl-xxx))))


(provide 'init-flycheck)
;;; init-flycheck.el ends here

;;; init-python.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,17:54:23
;;; Commentary:

;;; Code:


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode)
                ("\\.py\\'"      . python-mode))
              auto-mode-alist))

(setq python-shell-interpreter (cond (*is-a-mac* "python3")
                                     (*is-a-lin* "python3")
                                     (t "python")))


(use-package pip-requirements :ensure t :defer t)

(use-package anaconda-mode :ensure t :defer t
  :after python
  :init
  (add-hook 'python-mode-hook (lambda () (unless (file-remote-p default-directory))
                                (anaconda-mode 1)))
  :hook (anaconda-mode . anaconda-eldoc-mode)
  :config
  (define-key anaconda-mode-map (kbd "M-?") nil)
  ;;(define-key anaconda-mode-map (kbd "C-ENTER") 'python-)
  )



(use-package company-anaconda :ensure t :defer t
  :after (company python anaconda-mode)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package elpy :ensure t :defer t
  :config
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))


(use-package auto-virtualenv :ensure t :defer t
  :init
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

;; (use-package ein :ensure t :defer t)

;; (ein) (ob-ein)

(use-package ein :ensure t :defer t)

(use-package reformatter :ensure t :defer t
  :config (reformatter-define black :program "black"))

;; (setq python-shell-completion-native-enable nil) 

;; (use-package pylookup :ensure t :defer t)


(provide 'init-python)
;;; init-python.el ends here

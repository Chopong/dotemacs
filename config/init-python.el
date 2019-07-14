;;; init-python.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,17:54:23
;;; Commentary:

;;; Code:


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(use-package pip-requirements :ensure t :defer t)

(use-package anaconda-mode :ensure t :defer t
  :after python
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (define-key anaconda-mode-map (kbd "M-?") nil)
  )

(use-package company-anaconda :ensure t :defer t
  :after (company python anaconda-mode)
  :config
  (push 'company-anaconda company-backends))

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))


;; (ein) (ob-ein)

(use-package ein :ensure t :defer t)



;; (use-package pylookup :ensure t :defer t)


(provide 'init-python)
;;; init-python.el ends here

;;; init-clojure.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,01:08:07
;;; Commentary:

;;; Code:

(use-package clojure-mode   :ensure t :defer 2
  :config
  ;;  (add-hook 'clojure-mode-hook 'sanityinc/lisp-setup)
  (add-hook 'clojure-mode-hook 'subword-mode))

(use-package cljsbuild-mode :ensure t :defer 2)
(use-package elein          :ensure t :defer 2)

(use-package cider          :ensure t :defer 2
  :config
  (setq nrepl-popup-stacktraces nil)
  (after-load 'cider
    (add-hook 'cider-repl-mode-hook 'subword-mode)
    ;; (add-hook 'cider-repl-mode-hook 'paredit-mode)
    )
  )


(use-package flycheck-clojure :ensure t
  :after  (clojure-mode cider flycheck)
  :config (flycheck-clojure-setup))
  

(provide 'init-clojure)
;;; init-clojure.el ends here

;;; init-javascript.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,12:43:57
;;; Commentary:

;;; Code:

(use-package json-mode       :ensure t :defer t)
(use-package coffee-mode     :ensure t :defer t)
(use-package typescript-mode :ensure t :defer t)
(use-package prettier-js     :ensure t :defer t)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order

(use-package js2-mode        :ensure t :defer t
  :mode ("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode)
  :config
  ;; js2-mode

  ;; Change some defaults: customize them to override
  (setq-default js2-bounce-indent-p nil)
  (after-load 'js2-mode
    ;; Disable js2 mode's syntax error highlighting by default...
    (setq-default js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil)
    ;; ... but enable it if flycheck can't handle javascript
    (autoload 'flycheck-get-checker-for-buffer "flycheck")
    (defun enable-js2-checks-if-flycheck-inactive ()
      (unless (flycheck-get-checker-for-buffer)
        (setq-local js2-mode-show-parse-errors t)
        (setq-local js2-mode-show-strict-warnings t)))
    (add-hook 'js2-mode-hook 'enable-js2-checks-if-flycheck-inactive)

    (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

    (js2-imenu-extras-setup))

  (setq-default js-indent-level 2)
  ;; In Emacs >= 25, the following is an alias for js-indent-level anyway
  (setq-default js2-basic-offset 2)
  (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
  )




(use-package xref-js2 :ensure t :defer t
  :config
  (after-load 'js2-mode
    (define-key js2-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook
              (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

  )

;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(use-package js-comint :ensure t :defer t
  :bind (:map inferior-js-minor-mode-map
              ("C-x C-e" . 'js-send-last-sexp)
              ("C-c b"   . 'js-send-buffer))
  :config
  (setq js-comint-program-command "node")
  (defvar inferior-js-minor-mode-map (make-sparse-keymap))

  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    nil " InfJS" inferior-js-minor-mode-map)

  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode))
  )
;; ---------------------------------------------------------------------------
;; Alternatively, use skewer-mode
;; ---------------------------------------------------------------------------

(use-package skewer-mode :ensure t :defer t
  :config
  (after-load 'skewer-mode
    (add-hook 'skewer-mode-hook
              (lambda () (inferior-js-keys-mode -1)))))



(use-package add-node-modules-path :ensure t :defer t
  :init
  (after-load 'typescript-mode
    (add-hook 'typescript-mode-hook 'add-node-modules-path))
  (after-load 'js2-mode
    (add-hook 'js2-mode-hook 'add-node-modules-path))
  )

(provide 'init-javascript)
;;; init-javascript.el ends here

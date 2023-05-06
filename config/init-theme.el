;;; init-theme.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,21:33:25
;;; Commentary:

;;; Code:


;; (use-package highlight-symbol :ensure t :defer t
;;   :config
;;   (global-set-key [(control f3)] 'highlight-symbol)
;;   (global-set-key [f3] 'highlight-symbol-next)
;;   (global-set-key [(shift f3)] 'highlight-symbol-prev)
;;   (global-set-key [(meta f3)] 'highlight-symbol-query-replace))
;; (use-package color-identifiers-mode :ensure t :defer t)


;; (use-package poet-theme :ensure t :defer t)
;; (use-package monokai-theme :ensure t)

;; (use-package moe-theme :ensure t)
;; (use-package solarized-theme :ensure t :defer t)
;; (use-package tomorrow-theme :ensure t :defer t)
(use-package dracula-theme :ensure t :defer t)
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-dracula t)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (doom-themes-treemacs-config)

;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config)
;;   )

(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun dracula ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(dracula))
  (reapply-themes))


(use-package dimmer :ensure t :defer t
  :hook (after-init . dimmer-mode)
  :config
  (setq-default dimmer-fraction 0.15)
  (advice-add 'frame-set-background-mode
	      :after (lambda (&rest args) (dimmer-process-all))))




;; Extras for theme editing
(use-package rainbow-mode :ensure t :defer 2 :diminish rainbow-mode
  :hook (help-mode . rainbow-mode)
  :init
  (defun enable-rainbow-mode-if-theme ()
    (when (and (buffer-file-name) (string-match-p "\\(color-theme-\\|-theme\\.el\\)" (buffer-file-name)))
      (rainbow-mode)))
  (add-hook 'emacs-lisp-mode-hook 'enable-rainbow-mode-if-theme))
  


(provide 'init-theme)
;;; init-theme.el ends here

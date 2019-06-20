;;; init-pos-package.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,01:28:55
;;; Commentary:

;;; Code:

(use-package rainbow-delimiters   :ensure t :defer 1
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package crontab-mode         :ensure t :defer 2
  :mode ("\\.?cron\\(tab\\)?\\'" . 'crontab-mode))

(use-package origami              :ensure t :defer 2
  :bind (:map origami-mode-map
              ("C-c f" . 'origami-recursively-toggle-node)
              ("C-c F" . 'origami-toggle-all-nodes)))

;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "^Portfile\\'")

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'set-mode-for-new-scripts)
;;(setq auto-save-list-file-name )
(setq auto-save-list-file-prefix nil)

(defun set-mode-for-new-scripts ()
  "Invoke `normal-mode' if this file is a script and in `fundamental-mode'."
  (and
   (eq major-mode 'fundamental-mode)
   (>= (buffer-size) 2)
   (save-restriction
     (widen)
     (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
   (normal-mode)))

(use-package info-colors  :ensure t :defer t
  :after info
  :hook (Info-selection . 'info-colors-fontify-node))

;; Handle the prompt pattern for the 1password command-line interface
(after-load 'comint
  (setq comint-password-prompt-regexp
        (concat comint-password-prompt-regexp
                "\\|^Please enter your password for user .*?:\\s *\\'")))

(use-package regex-tool :ensure t :defer 2
  :config
  (setq-default regex-tool-backend 'perl))


(after-load 're-builder
  ;; Support a slightly more idiomatic quit binding in re-builder
  (define-key reb-mode-map (kbd "C-c C-k") 'reb-quit))

(add-auto-mode 'conf-mode "^Procfile\\'")

;;(pdf-tools)
;; (org-brain)
;; (dash)

(use-package dumb-jump
  :bind (("M-g o" . 'dumb-jump-go-other-window)
         ("M-g j" . 'dumb-jump-go)
         ("M-g i" . 'dumb-jump-go-prompt)
         ("M-g x" . 'dumb-jump-go-prefer-external)
         ("M-g z" . 'dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
  :ensure t :defer t)

(use-package fireplace :ensure t :defer t)
(use-package paradox :ensure t :defer t)
(use-package async :ensure t :defer t)
;;(use-package helpful :ensure t :defer t)
;; (use-package eyebrowse)
;; (use-package golden-ratio :ensure t :defer t
;;   :config
;;   (setq golden-ratio-auto-scale t))
(use-package nov :ensure t :defer t
  :mode ("\\.epub\\'" . nov-mode))

;; (use-package crux :ensure t :defer t)

;; (use-package highlight-indentation :ensure t :defer t
;;   :init
;;   (add-hook 'prog-mode-hook 'highlight-indentation-mode))
;; (use-package highlight-indent-guides :ensure t :defer t
;;  :init
;;  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;  :config
;;  (setq highlight-indent-guides-method 'character)
;;  (setq highlight-indent-guides-character ?\|)
;;  ;; (setq highlight-indent-guides-auto-odd-face-perc 15)
;;  ;; (setq highlight-indent-guides-auto-even-face-perc 15)
;;  ;; (setq highlight-indent-guides-auto-character-face-perc 20)
;;  (setq highlight-indent-guides-responsive 'stack))

(use-package indent-guide :ensure t :defer t
  :hook (after-init . indent-guide-global-mode))


(use-package perspective :ensure t :defer t)
(use-package suggest :ensure t :defer t)
(use-package visual-regexp :ensure t :defer t)
;; (use-package deferred :ensure t :defer t)
;; (require 'demo-it)
;; (use-package interleave :ensure t :defer t)
(use-package google-translate :ensure t :defer t)
(use-package emamux :ensure t :defer t)


(provide 'init-pos-package)
;;; init-pos-package.el ends here

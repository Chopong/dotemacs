;;; init-spell.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,12:08:08
;;; Commentary:

;;; Code:

(use-package ispell :ensure nil :defer t
  :config
  (when (executable-find ispell-program-name)
    ;; Add spell-checking in comments for all programming language modes
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)

    (after-load 'flyspell
      (define-key flyspell-mode-map (kbd "C-;") nil)
      (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))) )


(provide 'init-spell)
;;; init-spell.el ends here

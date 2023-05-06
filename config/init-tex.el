;;; init-tex.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-06-29,Sat,21:43:23
;;; Commentary:

;;; Code:

(use-package ebib :ensure t :defer t)

(use-package auctex :ensure t :defer t)

(use-package cdlatex :ensure t :defer t)

(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; set default tex engine to xetex, which has better support for chinese
(setq TeX-engine 'xetex)
;; generate pdf rather then dvi
(setq TeX-PDF-mode t)
(setq latex-run-command 'xetex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq-default reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
;; auto enable cdlatex-mode when use auctex
(setq reftex-plug-into-AUCTeX t)
;; add "-shell-escape" option to LaTeX command, which is needed by packages like minted
;; (eval-after-load "tex"
;;   '(setcdr (assoc "LaTeX" TeX-command-list)
;;            '("%`%l%(mode) -shell-escape%' %t"
;;              TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
;;            )
;;   )

(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (add-to-list 'TeX-command-list
                           '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' -output-directory=tmp -shell-escape%'  %t" TeX-run-TeX nil t))
              (setq TeX-command-default "XeLaTeX")
              (setq TeX-save-query nil)
              (setq TeX-show-compilation t)
              ))

(setenv "PATH" (concat "/Library/TeX/texbin:/usr/local/bin" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/usr/local/bin") exec-path))

(provide 'init-tex)
;;; init-tex.el ends here

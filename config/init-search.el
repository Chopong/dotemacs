;;; init-search.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,21:13:07
;;; Commentary:

;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

;; Show number of matches while searching

(use-package anzu     :ensure t :defer 1
  :hook (after-init . global-anzu-mode)
  :config
  (setq anzu-mode-lighter "")
  :bind (([remap query-replace-regexp] . 'anzu-query-replace-regexp)
	 ([remap query-replace]        . 'anzu-query-replace)))

;;------------------------------------------------------------------------------
(use-package wgrep    :ensure t :defer t
  :config
  (setq wgrep-auto-save-buffer t)
  (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode)))

;;------------------------------------------------------------------------------
(use-package ag       :ensure t :defer t
  :bind ("M-?" . 'ag-project)
  :config
  (setq-default ag-highlight-search t))

(use-package wgrep-ag :ensure t :defer t)

;;------------------------------------------------------------------------------
(use-package rg       :ensure t :defer t
  :bind ("M-?" . 'rg-project))
;; (use-package deadgrep :ensure t :defer t :pin melpa-stable)

;;------------------------------------------------------------------------------

(use-package isearch  :ensure nil :defer t
  :bind (:map isearch-mode-map
	      ([remap isearch-delete-char] . 'isearch-del-char)
	      ("\C-\M-w"                   . 'isearch-yank-symbol)
	      ([(control return)]          . 'isearch-exit-other-end))
  :config
  (when (fboundp 'isearch-occur)
    ;; to match ivy conventions
    (define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur))
  (defun isearch-yank-symbol ()
    "*Put symbol at current point into search string."
    (interactive)
    (let ((sym (thing-at-point 'symbol)))
      (if sym
	  (progn
	    (setq isearch-regexp t
		  isearch-string (concat "\\_<" (regexp-quote sym) "\\_>")
		  isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
		  isearch-yank-flag t))
	(ding)))
    (isearch-search-and-update))
  (defun isearch-exit-other-end ()
    "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))
  )

(use-package google-this :ensure t :defer t)


(provide 'init-search)
;; init-search.el ends here

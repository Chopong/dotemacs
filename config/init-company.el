;;; init-company.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,00:35:06
;;; Commentary:

;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(use-package company :ensure t :defer 1 :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (dolist (backend '(company-eclim company-semantic))
    (delq backend company-backends))
  (setq-default company-dabbrev-other-buffers 'all
		company-tooltip-align-annotations t)
  :bind (("M-C-/" . 'company-complete)
	 :map company-mode-map
	 ("M-/" . 'company-complete)
	 :map company-active-map
	 ("M-/" . 'company-other-backend)
	 ("C-n" . 'company-select-next)
	 ("C-p" . 'company-select-previous)))

(use-package company-quickhelp :ensure t :defer t
  :hook (company-mode . company-quickhelp-mode))


;; Suspend page-break-lines-mode while company menu is active
;; (see https://github.com/company-mode/company-mode/issues/416)
(after-load 'company
  (after-load 'page-break-lines
    (defvar-local page-break-lines-on-p nil)

    (defun page-break-lines-disable (&rest ignore)
      (when (setq page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun page-break-lines-maybe-reenable (&rest ignore)
      (when page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'page-break-lines-disable)
    (add-hook 'company-after-completion-hook 'page-break-lines-maybe-reenable)))

(use-package company-jedi :ensure t :defer t
  :config
  (add-hook 'python-mode-hook
            (lambda () (add-to-list
                   'company-backends 'company-jedi))))


(provide 'init-company)
;;; init-company.el ends here

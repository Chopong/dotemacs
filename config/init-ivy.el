;;; init-ivy.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,00:43:18
;;; Commentary:

;;; Code:
(use-package ivy :ensure t :defer t :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :config
  (setq-default ivy-use-virtual-buffers      t
		ivy-virtual-abbreviate       'fullpath
		ivy-count-format             ""
		projectile-completion-system 'ivy
		ivy-magic-tilde              nil
		ivy-dynamic-exhibit-delay-ms 150
		ivy-use-selectable-prompt    t
		ivy-initial-inputs-alist     '((Man-completion-table . "^")
					       (woman . "^")))

  
  (defun enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (use-package flx :ensure t)
    (setq-default ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
  
  :bind (:map ivy-minibuffer-map
	      ("RET"   . #'ivy-alt-done)
	      ("C-j"   . #'ivy-immediate-done)
	      ("C-RET" . #'ivy-immediate-done)
	      ("<up>"  . #'ivy-previous-line-or-history)
	      :map ivy-occur-mode-map
	      ("C-c C-q" . #'ivy-wgrep-change-to-wgrep-mode))
  )

(use-package counsel :ensure t :defer 1 :diminish counsel-mode
  :hook (after-init . counsel-mode)
  :config
  (setq-default counsel-mode-override-describe-bindings t)

  (let ((search-function (cond ((executable-find "rg") 'counsel-rg)
                               ((executable-find "ag") 'counsel-ag)
                               ((executable-find "pt") 'counsel-pt)
                               ((executable-find "ack") 'counsel-ack))))
    (when search-function
      (defun counsel-search-project (initial-input &optional use-current-dir)
        "Search using `counsel-rg' or similar from the project root for INITIAL-INPUT.
If there is no project root, or if the prefix argument
USE-CURRENT-DIR is set, then search from the current directory
instead."
        (interactive (list (thing-at-point 'symbol)
                           current-prefix-arg))
        (let ((current-prefix-arg)
              (dir (if use-current-dir
                       default-directory
                     (condition-case err
                         (projectile-project-root)
                       (error default-directory)))))
          (funcall search-function initial-input dir)))))
  (after-load 'ivy
    (add-to-list 'ivy-height-alist (cons 'counsel-ag 20)))
  (global-set-key (kbd "M-?") 'counsel-search-project)
  )


(use-package swiper :ensure t :defer 1
  :after ivy
  :config
  (defun swiper-at-point (sym)
      "Use `swiper' to search for the symbol at point."
      (interactive (list (thing-at-point 'symbol)))
      (swiper sym))
  :bind (:map ivy-mode-map
	      ("M-s /" . 'swiper-at-point)))

(use-package ivy-xref :ensure t :defer 1
  :config
  (setq xref-show-xrefs-function 'ivy-xref-show-xrefs))


(provide 'init-ivy)
;;; init-ivy.el ends here

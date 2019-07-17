;;; init-css.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,12:58:45
;;; Commentary:

;;; Code:

(use-package rainbow-mode :ensure t :defer t
  :hook (css-mode-hook html-mode-hook sass-mode-hook)
  )

(after-load 'mmm-vars
  (mmm-add-group
   'html-css
   '((css-cdata
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
      :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>"
      :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css
      :submode css-mode
      :face mmm-code-submode-face
      :front "<style[^>]*>[ \t]*\n?"
      :back "[ \t]*</style>"
      :insert ((?c css-tag nil @ "<style type=\"text/css\">"
                   @ "\n" _ "\n" @ "</style>" @)))
     (css-inline
      :submode css-mode
      :face mmm-code-submode-face
      :front "style=\""
      :back "\"")))
  (dolist (mode (list 'html-mode 'nxml-mode))
    (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css)))




;;; SASS and SCSS
(use-package sass-mode :ensure t :defer t
  :config
  (unless (fboundp 'scss-mode)
    ;; Prefer the scss-mode built into Emacs
    (use-package scss-mode :ensure t
      :config
      (setq-default scss-compile-at-save nil))
    ))



;;; LESS
(unless (fboundp 'less-css-mode)
  ;; Prefer the scss-mode built into Emacs
  (use-package less-css-mode :ensure t)
  (use-package skewer-less   :ensure t
    :hook (less-css-mode . skewer-less-mode))
  )



;; Skewer CSS
(when (fboundp 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode))


;;; Use eldoc for syntax hints
(use-package css-eldoc :ensure t :defer t
  :init (autoload 'turn-on-css-eldoc "css-eldoc")
  :hook (css-mode . 'turn-on-css-eldoc))

(provide 'init-css)
;;; init-css.el ends here

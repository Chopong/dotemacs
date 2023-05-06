;;; init-c.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Wei Shaopeng
;; Email : Chopong@qq.com
;; Date  : 2021-10-01,五,18:43:01
;;; Commentary:

;;; Code:

(use-package xcscope :ensure t :defer t
  :bind (:map c-mode-map
              ([(control f3)] . 'cscope-set-initial-directory)
              ([(control f4)] . 'cscope-unset-initial-directory)
              ([(control f5)] . 'cscope-find-this-symbol)
              ([(control f6)] . 'cscope-find-global-definition)
              ([(control f7)] . 'cscope-find-global-definition-no-prompting)
              ([(control f8)] . 'cscope-pop-mark)
              ([(control f9)] . 'cscope-find-called-functions)
              ([(control f10)] . 'cscope-find-functions-calling-this-function)
              ([(control f11)] . 'cscope-display-buffer)
              ([(control f12)] . 'cscope-display-buffer-toggle)
              )
  :config
  (defvar astyle-command-c "astyle --style=ansi --mode=c -s4 -S -Y -f -p -y -k3 -U -n -w -Y -c -xL")
  (defun astyle-region (start end)
    "Run astyle on region, formatting it in a pleasant way."
    (interactive "r")
    (save-excursion
      (shell-command-on-region start end astyle-command-c nil t))
    )
  (defun astyle-buffer ()
    "Run astyle on whole buffer, formatting it in a pleasant way."
    (interactive)
    (save-excursion
      (astyle-region (point-min) (point-max)))
    )
  (add-hook 'c-mode-common-hook
            '(lambda()
               (define-key c-mode-map "C-c r" 'astyle-region)
               (define-key c-mode-map "C-c b" 'astyle-buffer)
               (define-key c++-mode-map "C-c r" 'astyle-region)
               (define-key c++-mode-map "C-c b" 'astyle-buffer)))
  )


(provide 'init-c)
;;; init-c.el ends here

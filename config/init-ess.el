;;; init-ess.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-21,Tue,14:41:17
;;; Commentary:

;;; Code:


;; (use-package ess-site :ensure ess :defer t
;;   :mode
;;   (("\\.sp\\'"           . S-mode)
;;    ("/R/.*\\.q\\'"       . R-mode)
;;    ("\\.[qsS]\\'"        . S-mode)
;;    ("\\.ssc\\'"          . S-mode)
;;    ("\\.SSC\\'"          . S-mode)
;;    ("\\.[rR]\\'"         . R-mode)
;;    ("\\.[rR]nw\\'"       . Rnw-mode)
;;    ("\\.[sS]nw\\'"       . Snw-mode)
;;    ("\\.[rR]profile\\'"  . R-mode)
;;    ("NAMESPACE\\'"       . R-mode)
;;    ("CITATION\\'"        . R-mode)
;;    ("\\.omg\\'"          . omegahat-mode)
;;    ("\\.hat\\'"          . omegahat-mode)
;;    ("\\.lsp\\'"          . XLS-mode)
;;    ("\\.do\\'"           . STA-mode)
;;    ("\\.ado\\'"          . STA-mode)
;;    ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
;;    ("\\.jl\\'"           . ess-julia-mode)
;;    ("\\.[Ss]t\\'"        . S-transcript-mode)
;;    ("\\.Sout"            . S-transcript-mode)
;;    ("\\.[Rr]out"         . R-transcript-mode)
;;    ("\\.Rd\\'"           . Rd-mode)
;;    ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
;;    ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
;;    ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
;;    ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
;;    ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
;;    ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
;;   :commands R
;;   :config
;;   (progn

;;     (setq ess-first-continued-statement-offset 2
;;           ess-continued-statement-offset 0
;;           ess-expression-offset 2
;;           ess-nuke-trailing-whitespace-p t
;;           ess-default-style 'DEFAULT
;;           ess-ask-for-ess-directory nil
;;           ess-eval-visibly nil
;;           ess-directory user-project-directory
;;           ;; Keep global .Rhistory file.
;;           ess-history-directory "~/.R/"
;;           inferior-R-args "-q" ; I donnot want to print startup message
;;           )

;;     (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
;;     (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input)

;;     (add-hook 'ess-mode-hook 'smartparens-mode)
;;     (add-hook 'ess-mode-hook 'yas-minor-mode)
;;     (add-hook 'inferior-ess-mode-hook 'smartparens-mode)
;;     (add-hook 'ess-mode-hook 'company-mode)
;;     (add-hook 'inferior-ess-mode-hook 'company-mode)
;;     )
;;   )

(use-package ess-site
  :ensure ess
  :defer t
  :config
  (defun my-ess-start-R ()
    (interactive)
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
        (progn (delete-other-windows)
               (setq w1 (selected-window))
               (setq w1name (buffer-name))
               (setq w2 (split-window w1 nil t))
               (R)
               (set-window-buffer w2 "*R*")
               (set-window-buffer w1 w1name))))

  (defun my-ess-eval ()
    (interactive)
    (my-ess-start-R)
    (if (and transient-mark-mode mark-active)
        (call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line-and-step)))
  (add-hook 'ess-mode-hook '(lambda() (local-set-key [(shift return)] 'my-ess-eval)))
  (add-hook 'Rnw-mode-hook '(lambda() (local-set-key [(shift return)] 'my-ess-eval)))
  :init
  (dolist (hook (list 'rainbow-mode 'hs-minor-mode 'rainbow-delimiters-mode))
    (add-hook 'ess-mode-hook hook))
  (setq ess-ask-for-ess-directory nil)
  (add-hook 'inferior-ess-mode-hook '(lambda() (local-set-key [C-up] 'comint-previous-input)
                                       (local-set-key [C-down] 'comint-next-input)))
  )

(load "ess-autoloads")


;; (use-package key-combo :ensure t :defer t
;;   :init
;;   (dolist (hook '(ess-mode-hook inferior-ess-mode-hook tex-mode-hook))
;;     (add-hook hook '(lambda()
;;                       (key-combo-mode t)))
;;     )
;;   (defvar key-combo-ess-default
;;     '((">"  . (" > " " %>% " " %>>%"))
;;       ("<"  . (" <- " " < "))
;;       ("```" . ("```'''"))
;;       ("$"  . ("$" " %$% "))
;;       ("<>" . " %<>% ")
;;       ("*"  . ("*" " * " "%*%"))
;;       ("%"  . ("%" " %in% " "%%"))
;;       ("^"  . ("^" " ^ "))
;;       ("/"  . ("/" " / "))
;;       ("="  . ("=" " = " " == "))
;;       ("!"  . ("!" " != "))
;;       (","  . ("," ", "))
;;       ("~"  . ("~" " ~ "))
;;       (":"  . (":" "::" ":::"))
;;       (":=" . " := ") ; data.table
;;       ("->" . " -> ")))
;;   (defvar key-combo-tex-mode
;;     '(("{}"  . ("{}" "\{\}"))
;;       ))
;;   (key-combo-define-hook '(ess-mode-hook inferior-ess-mode-hook)
;;                          'ess-key-combo-load-default
;;                          key-combo-ess-default)
;;   (key-combo-define-hook '(tex-mode-hook)
;;                          'tex-mode-combo
;;                          key-combo-tex-mode))

(use-package poly-markdown :ensure t :defer t)

(use-package poly-R :ensure t :defer t)


(use-package polymode :ensure t
  :mode (("\\.[SR]nw\\'" . poly-noweb+r-mode)
         ("\\.Rmd\\'" . Rmd-mode))
  :init
  (progn
    (defun Rmd-mode ()
      "ESS Markdown mode for Rmd files."
      (interactive)
      ;; (setq load-path
      ;;       (append (list "path/to/polymode/" "path/to/polymode/modes/")
      ;;               load-path))
      (require 'poly-R)
      (require 'poly-markdown)
      (R-mode)
      (yaml-mode)
      (poly-markdown+r-mode))

    ;; do this in R process
    ;; library (rmarkdown); render ("file_name.Rmd")
    (defun ess-rmarkdown ()
      (interactive)
      "Compile R markdown (.Rmd). Should work for any output type."
      "http://roughtheory.com/posts/ess-rmarkdown.html"
      ;; Check if attached R-session
      (condition-case nil
          (ess-get-process)
        (error
         (ess-switch-process)))
      (save-excursion
        (let* ((sprocess (ess-get-process ess-current-process-name))
               (sbuffer (process-buffer sprocess))
               (buf-coding (symbol-name buffer-file-coding-system))
               (R-cmd
                (format "library (rmarkdown); rmarkdown::render(\"%s\")"
                        buffer-file-name))
               (message "Running rmarkdown on %s" buffer-file-name)
               (ess-execute R-cmd 'buffer nil nil)
               (switch-to-buffer rmd-buf)
               (ess-show-buffer (buffer-name-sbuffer) nil)))))))

(provide 'init-ess)
;;; init-ess.el ends here

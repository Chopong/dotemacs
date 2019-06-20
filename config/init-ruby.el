;;; init-ruby.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,12:10:16
;;; Commentary:

;;; Code:

;;; Basic ruby setup
(use-package ruby-hash-syntax :ensure t :defer 2)

(add-auto-mode 'ruby-mode
               "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Kirkfile\\'")


(add-auto-mode 'conf-mode "Gemfile\\.lock\\'")


(setq-default
 ruby-use-encoding-map nil
 ruby-insert-encoding-magic-comment nil)

(add-hook 'ruby-mode-hook 'subword-mode)

(after-load 'page-break-lines
  (push 'ruby-mode page-break-lines-modes))

(use-package rspec-mode :ensure t :defer 2)



(define-derived-mode brewfile-mode ruby-mode "Brewfile"
  "A major mode for Brewfiles, used by homebrew-bundle on MacOS.")

(add-auto-mode 'brewfile-mode "Brewfile\\'")


;;; Inferior ruby
(use-package inf-ruby :ensure t :defer 2)




;;; Ruby compilation
(use-package ruby-compilation :ensure t :defer 2)

(after-load 'ruby-mode
  (define-key ruby-mode-map [S-f7] 'ruby-compilation-this-buffer)
  (define-key ruby-mode-map [f7] 'ruby-compilation-this-test))

(after-load 'ruby-compilation
  (defalias 'rake 'ruby-compilation-rake))



;;; Robe

(use-package robe :ensure t :defer t
  :config
  (after-load 'ruby-mode
    (add-hook 'ruby-mode-hook 'robe-mode))
  (after-load 'robe
    (after-load 'company
      (push 'company-robe company-backends))))


;;; ri support
(use-package yari :ensure t :defer t
  :config
  (defalias 'ri 'yari)
  )



(use-package goto-gem :ensure t :defer t)


(use-package bundler :ensure t :defer t)


(use-package yard-mode :ensure t :defer t :diminish yard-mode
  :hook (ruby-mode . yard-mode))


;;; ERB
(use-package mmm-mode :ensure t :defer t)

(use-package derived)

(defun set-up-mode-for-erb (mode)
  (add-hook (derived-mode-hook-name mode) (lambda () (require 'mmm-erb)))
  (mmm-add-mode-ext-class mode "\\.erb\\'" 'erb))

(dolist (mode '(html-mode html-erb-mode nxml-mode))
  (set-up-mode-for-erb mode)
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-js)
  (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?\\'" 'html-css))

(mapc 'set-up-mode-for-erb
      '(coffee-mode js-mode js2-mode js3-mode markdown-mode textile-mode))

(mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

(add-auto-mode 'html-erb-mode "\\.rhtml\\'" "\\.html\\.erb\\'")
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))

(mmm-add-mode-ext-class 'yaml-mode "\\.yaml\\(\\.erb\\)?\\'" 'erb)
(set-up-mode-for-erb 'yaml-mode)

(dolist (mode (list 'js-mode 'js2-mode 'js3-mode))
  (mmm-add-mode-ext-class mode "\\.js\\.erb\\'" 'erb))


;;----------------------------------------------------------------------------
;; Ruby - my convention for heredocs containing SQL
;;----------------------------------------------------------------------------

;; Needs to run after rinari to avoid clobbering font-lock-keywords?

;; (require-package 'mmm-mode)
;; (eval-after-load 'mmm-mode
;;   '(progn
;;      (mmm-add-classes
;;       '((ruby-heredoc-sql
;;          :submode sql-mode
;;          :front "<<-?[\'\"]?\\(end_sql\\)[\'\"]?"
;;          :save-matches 1
;;          :front-offset (end-of-line 1)
;;          :back "^[ \t]*~1$"
;;          :delimiter-mode nil)))
;;      (mmm-add-mode-ext-class 'ruby-mode "\\.rb\\'" 'ruby-heredoc-sql)))

;; (add-to-list 'mmm-set-file-name-for-modes 'ruby-mode)















(provide 'init-ruby)
;;; init-ruby.el ends here

;;; init-slime.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,17:33:04
;;; Commentary:

;;; Code:

(use-package slime :ensure t :defer 2)
;; package.el compiles the contrib subdir, but the compilation order
;; causes problems, so we remove the .elc files there. See
;; http://lists.common-lisp.net/pipermail/slime-devel/2012-February/018470.html
(mapc #'delete-file
      (file-expand-wildcards (concat user-emacs-directory "elpa/slime-2*/contrib/*.elc")))

(use-package hippie-expand-slime :ensure t :defer 2)
(use-package slime-company       :ensure t :defer 2)


;;; Lisp buffers

(defun slime-setup-func ()
  "Mode setup function for slime lisp buffers."
  (set-up-slime-hippie-expand))

(after-load 'slime
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (let ((extras (when (require 'slime-company nil t)
                  '(slime-company))))
    (slime-setup (append '(slime-repl slime-fuzzy) extras)))
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (add-hook 'slime-mode-hook 'slime-setup-func))


;;; REPL

(defun slime-repl-setup ()
  "Mode setup function for slime REPL."
  (lisp-setup-func)
  (set-up-slime-hippie-expand))

(after-load 'slime-repl
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (after-load 'paredit
    (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil))

  ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
  (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)

  (add-hook 'slime-repl-mode-hook 'slime-repl-setup))



(provide 'init-slime)
;;; init-slime.el ends here

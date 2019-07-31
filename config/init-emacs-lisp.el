;;; init-emacs-lisp.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,10:23:20
;;; Commentary:

;;; Code:

(use-package elisp-slime-nav :ensure t :defer 1
  :hook ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))


;; (setq-default initial-scratch-message
;;               (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-banner-logo-title (concat "Happy hacking, " user-login-name " - Emacs ♥ you!"))
;;   (setq dashboard-startup-banner '3)
;;   (setq dashboard-center-content t)
;;   (setq dashboard-show-shortcuts nil)
;;   (setq dashboard-items '((recents  . 5)
;;                           (bookmarks . 5)
;;                           (projects . 5)
;;                           (agenda . 5)
;;                           (registers . 5))))


(defun headerise-elisp ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))
      (insert ";;; " fname " ends here\n"))))


;; Make C-x C-e run 'eval-region if the region is active
;;;###autoload
(defun eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END with PREFIX if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(use-package lisp-mode :ensure nil :defer 1
  :bind (([remap eval-expression] . 'pp-eval-expression)
	 :map emacs-lisp-mode-map
	      ("C-x C-e" . 'eval-last-sexp-or-region)))


(use-package ipretty   :ensure t :defer 1
  :hook (after-init . ipretty-mode))


(defun make-read-only (expression out-buffer-name)
  "Enable EXPRESSION `view-mode' in the OUT-BUFFER-NAME."
  (when (get-buffer out-buffer-name)
    (with-current-buffer out-buffer-name
      (view-mode 1))))
(advice-add 'pp-display-expression :after 'make-read-only)




;;;###autoload
(defun maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'maybe-set-bundled-elisp-readonly)






;; Use C-c C-z to toggle between elisp files and an ielm session
;; I might generalise this to ruby etc., or even just adopt the repl-toggle package.

(defvar-local repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")

(defvar repl-switch-function 'switch-to-buffer-other-window)

(defun switch-to-ielm ()
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall repl-switch-function "*ielm*")
      (ielm))
    (setq repl-original-buffer orig-buffer)))

(defun repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if repl-original-buffer
      (funcall repl-switch-function repl-original-buffer)
    (error "No original buffer")))

(after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'switch-to-ielm))
(after-load 'ielm
  (define-key ielm-map (kbd "C-c C-z") 'repl-switch-back))

;; ----------------------------------------------------------------------------
;; Hippie-expand
;; ----------------------------------------------------------------------------

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))


;; ----------------------------------------------------------------------------
;; Load .el if newer than corresponding .elc
;; ----------------------------------------------------------------------------
(setq load-prefer-newer t)



(use-package immortal-scratch :ensure t :defer 1
  :hook (after-init . immortal-scratch-mode))



;;; Support byte-compilation in a sub-process, as
;;; required by highlight-cl

(defun byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
       " ")))))


;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------
(defun enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defvar lispy-modes-hook
  '(;;enable-paredit-mode
    enable-check-parens-on-save)
  "Hook run in all Lisp modes.")


(use-package aggressive-indent :ensure t :defer 1
  :config
  (add-to-list 'lispy-modes-hook 'aggressive-indent-mode))


(defun lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'lispy-modes-hook))

(defun emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (set-up-hippie-expand-for-elisp))

(defconst elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst lispy-modes
  (append elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name lispy-modes))
  (add-hook hook 'lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name elispy-modes))
  (add-hook hook 'emacs-lisp-setup))

(when (boundp 'eval-expression-minibuffer-setup-hook)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

(use-package cl-lib-highlight :ensure t :defer 1
  :config
  (after-load 'lisp-mode
  (cl-lib-highlight-initialize)))

;; ----------------------------------------------------------------------------
;; Delete .elc files when reverting the .el from VC or magit
;; ----------------------------------------------------------------------------

;; When .el files are open, we can intercept when they are modified
;; by VC or magit in order to remove .elc files that are likely to
;; be out of sync.

;; This is handy while actively working on elisp files, though
;; obviously it doesn't ensure that unopened files will also have
;; their .elc counterparts removed - VC hooks would be necessary for
;; that.

(defvar vc-reverting nil
  "Whether or not VC or Magit is currently reverting buffers.")

(defun maybe-remove-elc (&rest _)
  "If reverting from VC, delete any .elc file that will now be out of sync."
  (when vc-reverting
    (when (and (eq 'emacs-lisp-mode major-mode)
               buffer-file-name
               (string= "el" (file-name-extension buffer-file-name)))
      (let ((elc (concat buffer-file-name "c")))
        (when (file-exists-p elc)
          (message "Removing out-of-sync elc file %s" (file-name-nondirectory elc))
          (delete-file elc))))))
(advice-add 'revert-buffer :after 'maybe-remove-elc)

(defun reverting (orig &rest args)
  (let ((vc-reverting t))
    (apply orig args)))
(advice-add 'magit-revert-buffers :around 'reverting)
(advice-add 'vc-revert-buffer-internal :around 'reverting)



(use-package macrostep :ensure t :defer 1
  :bind (:map emacs-lisp-mode-map
	      ("C-c e" . 'macrostep-expand)))



;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)





(use-package highlight-quoted :ensure t :defer 1
  :hook (emacs-lisp-mode . highlight-quoted-mode))



;; ERT
(after-load 'ert
  (define-key ert-results-mode-map (kbd "g") 'ert-results-rerun-all-tests))



(use-package cl-libify :ensure t :defer 1)

(use-package parinfer
  :ensure t
  :defer t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
             pretty-parens  ; different paren styles for different modes.
             evil           ; If you use Evil.
             lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
             paredit        ; Introduce some paredit commands.
             smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
             smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(provide 'init-emacs-lisp)
;;; init-emacs-lisp.el ends here

;;; init-editing-tools.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,11:26:38
;;; Commentary:

;;; Code:

(use-package unfill :ensure t :defer 1)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))


(when (eval-when-compile (version< "24.4" emacs-version))
  (add-hook 'after-init-hook 'electric-indent-mode))


(use-package list-unicode-display :ensure t :defer 2)




;;; Newline behaviour

(global-set-key (kbd "RET") 'newline-and-indent)

(defun newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'newline-at-end-of-line)




(use-package beacon :ensure t :defer 2
  :hook (after-init . beacon-mode)
  :config
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20)
  )





(setq-default blink-cursor-interval               0.4
              bookmark-default-file               (expand-file-name ".bookmarks.el" user-assets-directory)
              buffers-menu-max-size               30
              case-fold-search                    t
              column-number-mode                  t
              delete-selection-mode               t
              ediff-split-window-function         'split-window-horizontally
              ediff-window-setup-function         'ediff-setup-windows-plain
              indent-tabs-mode                    nil
              mouse-yank-at-point                 t
              save-interprogram-paste-before-kill t
              scroll-preserve-screen-position     'always
              set-mark-command-repeat-pop         t
              tooltip-delay                       1.5
              truncate-lines                      nil
              truncate-partial-width-windows      nil)


(use-package autorevert :ensure nil :defer 1 :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  )

(add-hook 'after-init-hook 'transient-mark-mode)


;; Huge files

(use-package vlf :ensure t :defer 2)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))


;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'show-paren-mode)
(setq show-paren-style 'parentheses)

;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(use-package page-break-lines :ensure t :defer t
  :pin melpa-stable
  :diminish page-break-lines-mode
  :hook (after-init . global-page-break-lines-mode))



(use-package multiple-cursors :ensure t :defer t
  :bind (("C-<"     . 'mc/mark-previous-like-this)
         ("C->"     . 'mc/mark-next-like-this)
         ("C-c m a" . 'mc/mark-all-like-this)
         ("C-c m r" . 'set-rectangular-region-anchor)
         ("C-c m l" . 'mc/edit-lines)
         ("C-c m e" . 'mc/edit-ends-of-lines)
         ("C-c m b" . 'mc/edit-beginnings-of-lines)))

(use-package hungry-delete    :ensure t :defer t
  :bind (("S-<backspace>"   . 'hungry-delete-backward)
         ("S-<delete>"      . 'hungry-delete-forward)
         ("C-S-<backspace>" . 'kill-region)))


(use-package move-dup         :ensure t :defer t
  :bind (([M-up]    . 'md-move-lines-up)
         ([M-down]  . 'md-move-lines-down)
         ("C-c d"   . 'md-duplicate-down)
         ("C-c u"   . 'md-duplicate-up)))

(after-load 'subword (diminish 'subword-mode))



(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 2)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(use-package goto-line-preview :ensure t :defer t
  :bind ([remap goto-line] . 'goto-line-preview)
  :config
  (when (fboundp 'display-line-numbers-mode)
    (defun with-display-line-numbers (f &rest args)
      (let ((display-line-numbers t))
        (apply f args)))
    (advice-add 'goto-line-preview :around #'with-display-line-numbers))
  )



(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))

(use-package symbol-overlay :ensure t :defer t
  :diminish symbol-overlay-mode
  :hook ((prog-mode html-mode yaml-mode conf-mode) . symbol-overlay-mode)
  :bind ((:map symbol-overlay-mode-map
               ("M-i" . 'symbol-overlay-put)
               ("M-I" . 'symbol-overlay-remove-all)
               ("M-n" . 'symbol-overlay-jump-next)
               ("M-p" . 'symbol-overlay-jump-prev)))
  )


;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)




(use-package browse-kill-ring :ensure t :defer t
  :bind ("M-Y" . 'browse-kill-ring)
  :init
  (setq browse-kill-ring-separator "\f")
  :bind (:map browse-kill-ring-mode-map
              ("C-g" . 'browse-kill-ring-quit)
              ("M-n" . 'browse-kill-ring-forward)
              ("M-p" . 'browse-kill-ring-previous)
              )
  :config
  (after-load 'page-break-lines
    (push 'browse-kill-ring-mode page-break-lines-modes)))

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)


;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(use-package expand-region :ensure t :defer t
  :bind ("C-=" . er/expand-region))

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
;; (cua-selection-mode t)                  ; for rectangles, CUA is nice

;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(use-package avy :ensure t :defer t
  :bind (("M-g c"   . 'avy-goto-char-timer)
         ("M-g f" . 'avy-goto-line))
  )


(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (sanityinc/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(use-package whole-line-or-region :ensure t :defer 2
  :diminish whole-line-or-region-local-mode
  :hook (after-init . whole-line-or-region-global-mode)
  )


;; Some local minor modes clash with CUA rectangle selection



(defvar-local suspended-modes-during-cua-rect nil
  "Modes that should be re-activated when cua-rect selection is done.")

(eval-after-load 'cua-rect
  (advice-add 'cua--deactivate-rectangle :after
              (lambda (&rest _)
                (dolist (m suspended-modes-during-cua-rect)
                  (funcall m 1)
                  (setq suspended-modes-during-cua-rect nil)))))

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (eval-after-load 'cua-rect
    (advice-add 'cua--activate-rectangle :after
                (lambda (&rest _)
                  (when (bound-and-true-p mode-name)
                    (push mode-name suspended-modes-during-cua-rect)
                    (funcall mode-name 0))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-local-mode)





(defun open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'open-line-with-reindent)


;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region from BEG to END randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))




(use-package highlight-escape-sequences :ensure t :defer 2
  :hook (after-init . hes-mode))

(use-package guide-key :ensure t :defer 2 :diminish guide-key-mode
  :hook (after-init . guide-key-mode)
  :config
  (setq guide-key/guide-key-sequence t)
  )

;; (use-package which-key :ensure t)


(defun disable-features-during-macro-call (orig &rest args)
  "When running a macro, disable features that might be expensive.
ORIG is the advised function, which is called with its ARGS."
  (let (post-command-hook
        font-lock-mode
        (tab-always-indent (or (eq 'complete tab-always-indent) tab-always-indent)))
    (apply orig args)))

(advice-add 'kmacro-call-macro :around 'disable-features-during-macro-call)


(provide 'init-editing-tools)
;;; init-editing-tools.el ends here

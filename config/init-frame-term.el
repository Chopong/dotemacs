;;; init-frame-term.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,21:05:20
;;; Commentary:

;;; Code:

;;------------------------------------------------------------------------------
(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Run configured hooks in response to the newly-created FRAME.
Selectively runs either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (with-selected-frame frame
    (run-hooks (if window-system
                   'after-make-window-system-frame-hooks
                 'after-make-console-frame-hooks))))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defconst initial-frame (selected-frame)
  "The frame (if any) active during Emacs initialization.")

(add-hook 'after-init-hook
          (lambda () (when initial-frame
                  (run-after-make-frame-hooks initial-frame))))
;;------------------------------------------------------------------------------

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(use-package mwheel :ensure nil :defer t
  :init (autoload 'mwheel-install "mwheel")
  :config
  (defun console-frame-setup ()
    (xterm-mouse-mode 1)
    ;; Mouse in a terminal (Use shift to paste with middle button)
    (mwheel-install))
  (add-hook 'after-make-console-frame-hooks 'console-frame-setup))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(provide 'init-frame-term)
;; init-frame-term.el ends here

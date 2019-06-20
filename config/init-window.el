;;; init-window.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,00:08:06
;;; Commentary:

;;; Code:

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(use-package winner :ensure nil :defer 1
  :hook (after-init . winner-mode))

(use-package switch-window :ensure t :defer 1
  :config
  (setq-default switch-window-shortcut-style 'quail) ;; alphabet
  (setq-default switch-window-timeout nil)
  :bind ("C-x o" . 'switch-window))

;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  "Split-window using SPLIT-FUNCTION."
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'toggle-delete-other-windows)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)


(defun show-split-window ()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'show-split-window)
      (progn
        (jump-to-register :show-split-window)
        (setq this-command 'show-unsplit-window))
    (window-configuration-to-register :show-split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'show-split-window)





(defun toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "C-c <down>") 'toggle-current-window-dedication)



(unless (memq window-system '(nt w32))
  (windmove-default-keybindings 'control))

(use-package exwm
  :ensure t
  :defer t)

(use-package ace-window :ensure t :defer t
  :bind ("M-o" . 'ace-window))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-x SPC" . 'ace-jump-mode-pop-mark)
         ("C-c SPC" . 'ace-jump-mode))
  :config
  (eval-after-load "ace-jump-mode"
    '(ace-jump-mode-enable-mark-sync)))

;; (use-package window-purpose :ensure t :defer t)
;; (use-package e2wm :ensure t :defer t)

(provide 'init-window)
;;; init-window.el ends here

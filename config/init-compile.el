;;; init-compile.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,18:57:21
;;; Commentary:

;;; Code:

(use-package alert :ensure t :defer 1
  :config
  (defun alert-after-compilation-finish (buf result)
    "Use `alert' to report compilation RESULT if BUF is hidden."
    (when (buffer-live-p buf)
      (unless (catch 'is-visible
                (walk-windows (lambda (w)
                                (when (eq (window-buffer w) buf)
                                  (throw 'is-visible t))))
                nil)
        (alert (concat "Compilation " result)
               :buffer buf
               :category 'compilation))))
  (after-load 'compile
    (add-hook 'compilation-finish-functions 'alert-after-compilation-finish))
  )

(setq-default compilation-scroll-output t)


(defvar last-compilation-buffer nil
  "The last buffer in which compilation took place.")

(after-load 'compile
  (defun save-compilation-buffer (&rest _)
    "Save the compilation buffer to find it later."
    (setq last-compilation-buffer next-error-last-buffer))
  (advice-add 'compilation-start :after 'save-compilation-buffer)

  (defun find-prev-compilation (orig &optional edit-command)
    "Find the previous compilation buffer, if present, and recompile there."
    (if (and (null edit-command)
             (not (derived-mode-p 'compilation-mode))
             last-compilation-buffer
             (buffer-live-p (get-buffer last-compilation-buffer)))
        (with-current-buffer last-compilation-buffer
          (funcall orig edit-command))
      (funcall orig edit-command)))
  (advice-add 'recompile :around 'find-prev-compilation))

(global-set-key [f6] 'recompile)

(defun shell-command-in-view-mode (start end command &optional output-buffer replace &rest other-args)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless (or output-buffer replace)
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))
(advice-add 'shell-command-on-region :after 'shell-command-in-view-mode)


(after-load 'compile
  (require 'ansi-color)
  (defun colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'colourise-compilation-buffer))


(use-package cmd-to-echo :ensure t :defer 2)



;; ----------------------------------------------------------------------------
;; Automatic byte compilation
;; ----------------------------------------------------------------------------

(use-package auto-compile :ensure t :defer t
  :hook ((after-init . auto-compile-on-save-mode)
         (after-init . auto-compile-on-load-mode)))




(provide 'init-compile)
;;; init-compile.el ends here

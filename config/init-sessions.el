;;; init-sessions.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,00:11:42
;;; Commentary:

;;; Code:
;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-assets-directory)
      desktop-auto-save-timeout 600)
;; (desktop-save-mode 1)

(defun desktop-time-restore (orig &rest args)
  "Return time to restore ORIG with ARGS."
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (time-subtract-millis (current-time)
                                               start-time)))))
(advice-add 'desktop-read :around 'desktop-time-restore)


(defun desktop-time-buffer-create (orig ver filename &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (time-subtract-millis (current-time)
                                               start-time)
               (when filename
                 (abbreviate-file-name filename))))))
(advice-add 'desktop-create-buffer :around 'desktop-time-buffer-create)


;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(use-package savehist :ensure nil :defer t
  :hook (after-init . savehist-mode)
  :init
  (setq-default history-length 1000)
  (setq savehist-file (expand-file-name ".savehist" user-assets-directory))
  )


(use-package session :ensure t :defer 2)

(setq session-save-file (expand-file-name ".session" user-assets-directory))
(setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
(setq session-save-file-coding-system 'utf-8)

(add-hook 'after-init-hook 'session-initialize)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 100)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (ivy-history              . 100)
        (magit-revision-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        tags-file-name
        tags-table-list))



(provide 'init-sessions)
;;; init-sessions.el ends here

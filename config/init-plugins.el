;;; init-plugins.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,20:51:27
;;; Commentary:

;;; Code:


(eval-when-compile (require 'cl))
(defun add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

(add-subdirs-to-load-path
 (expand-file-name "plugins/" user-emacs-directory))

;;; Utilities for grabbing upstream libs

(defun plugins-dir-for (name)
  (expand-file-name (format "plugins/%s" name) user-emacs-directory))

(defun plugins-library-el-path (name)
  (expand-file-name (format "%s.el" name) (plugins-dir-for name)))

(defun download-plugins-module (name url)
  (let ((dir (plugins-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (plugins-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun ensure-lib-from-url (name url)
  (unless (plugins-library-loadable-p name)
    (byte-compile-file (download-plugins-module name url))))

(defun plugins-library-loadable-p (name)
  "Return whether or not the library `name' can be loaded from a
source file under ~/.emacs.d/plugins/name/"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (plugins-dir-for name)) f))))

(provide 'init-plugins)
;; init-plugins.el ends here

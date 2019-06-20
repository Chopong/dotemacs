;;; init-http.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,13:10:35
;;; Commentary:

;;; Code:

(use-package httprepl :ensure t :defer t)

(use-package restclient :ensure t :defer t
  :mode ("\\.rest\\'" . restclient-mode)
  :config
  (defun restclient-here ()
    (interactive)
    (with-current-buffer (get-buffer-create "*restclient*")
      (restclient-mode)
      (pop-to-buffer (current-buffer))))
  )


(provide 'init-http)
;;; init-http.el ends here

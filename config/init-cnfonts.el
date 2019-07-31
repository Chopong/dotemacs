;;; init-cnfonts.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-21,Tue,16:26:26
;;; Commentary:

;;; Code:

(use-package cnfonts :ensure t :defer t
  :config
  (setq cnfonts-use-face-font-rescale t))

(use-package pangu-spacing :ensure t :defer 2)


(provide 'init-cnfonts)
;;; init-cnfonts.el ends here

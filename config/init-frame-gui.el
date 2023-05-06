;;; init-frame-gui.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,21:54:02
;;; Commentary:

;;; Code:

;;--------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;--------------------------------------------------------------------------
(defun maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'maybe-suspend-frame)

;;--------------------------------------------------------------------------
;; Suppress GUI features
;;--------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;;--------------------------------------------------------------------------
;; Window size and features
;;--------------------------------------------------------------------------

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(setq column-number-mode t)
(setq line-number-mode t)
;; (global-linum-mode t)

(global-font-lock-mode 't)
(setq font-lock-maximum-decoration t)
(setq font-lock-global-modes '(not shell-mode text-mode))
(setq font-lock-verbose t)

;; I generally prefer to hide the menu bar, but doing this on OS X
;; simply makes it update unreliably in GUI frames, so we make an
;; exception.
(if *is-a-mac*
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (set-frame-parameter frame 'menu-bar-lines
                                     (if (display-graphic-p frame)
                                         1 0))))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))


(defun adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(defun chopong-default-alpha ()
  "This func is built for init whindow hook with alpha 90."
  (modify-frame-parameters nil `((alpha . 90)))
  )

(defun chopong-default-frame ()
  "This func is built for init frame with frame size."
  (interactive)
  (if window-system
      (progn
        (set-frame-position (selected-frame) 100 50)
        (if (> (x-display-pixel-width) 2000)
            (set-frame-width (selected-frame) 240)
          (set-frame-width (selected-frame) 120))
        (if (> (x-display-pixel-height) 1000)
            (set-frame-height (selected-frame) 90)
          (set-frame-height (selected-frame) 60))
        )))


(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "<f12>") 'toggle-frame-fullscreen))


;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(when *is-a-mac*
  (use-package ns-auto-titlebar :ensure t :defer nil
    :config
    (ns-auto-titlebar-mode)))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))


;; Change global font size easily

(use-package default-text-scale :ensure t
  :hook (after-init . default-text-scale-mode))


;; (use-package disable-mouse :ensure t)
;; (set-cursor-color "gray")
;; (set-mouse-color "gold1")

;; (use-package thumbs :ensure nil :defer t
;;   :init
;;   (setq)
;;   (auto-image-file-mode t))





(provide 'init-frame-gui)
;;; init-frame-gui.el ends here

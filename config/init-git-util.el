;;; init-git-util.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,21:43:53
;;; Commentary:

;;; Code:
;; (use-package git-gutter :ensure t)
(use-package git-blamed           :ensure t :defer t)
;; (use-package gitignore-mode       :ensure t :defer t)
;; (use-package gitconfig-mode       :ensure t :defer t)
(use-package git-timemachine      :ensure t :defer t
  :bind ("C-x v t" . 'git-timemachine-toggle))
(use-package git-commit           :ensure t :defer t
  :hook (git-commit-mode . goto-address-mode))



(use-package magit                :ensure t :defer t
  :bind (("C-x g"      . 'magit-status)
	 ([(meta f12)] . 'magit-status)
	 ("C-x M-g"    . 'magit-dispatch)
	 :map magit-status-mode-map
	 ("C-M-<up>"   . 'magit-section-up))
  :config
  (setq-default magit-diff-refine-hunk t)
  ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
  ;; quickly open magit on any one of your projects.

  (defun magit-or-vc-log-file (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))
  (after-load 'vc
    (define-key vc-prefix-map (kbd "l") 'magit-or-vc-log-file)
    (define-key vc-prefix-map (kbd "f") 'vc-git-grep))
  (when *is-a-mac*
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))
  (fullframe magit-status magit-mode-quit-window)
  )

;; (use-package magithub :ensure t :defer t
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/Downloads/"))

(use-package magit-todos          :ensure t :defer t)


;;------------------------------------------------------------------------------
;; github
;;------------------------------------------------------------------------------
(use-package yagist               :ensure t :defer t)
(use-package bug-reference-github :ensure t :defer t
  :hook (prog-mode . bug-reference-prog-mode))
(use-package github-clone         :ensure t :defer t)
(use-package forge                :ensure t :defer t :pin melpa
  :init
  (setq forge-database-file (expand-file-name "forge-database.sqlite" user-assets-directory)))
(use-package github-review        :ensure t :defer t)





(use-package diff-hl :ensure t
  :hook (after-init         . global-diff-hl-mode)
  :bind (:map diff-hl-mode-map
              ("<left-fringe><mouse-1>" . 'diff-hl-diff-goto-hunk))
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

(use-package browse-at-remote :ensure t :defer t)

;;; git-svn support

;; (when (maybe-require-package 'magit-svn)
;;   (require-package 'magit-svn)
;;   (autoload 'magit-svn-enabled "magit-svn")
;;   (defun sanityinc/maybe-enable-magit-svn-mode ()
;;     (when (magit-svn-enabled)
;;       (magit-svn-mode)))
;;   (add-hook 'magit-status-mode-hook #'sanityinc/maybe-enable-magit-svn-mode))

(after-load 'compile
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands")
(defun git-svn--available-commands ()
  (or git-svn--available-commands
      (setq git-svn--available-commands
            (sanityinc/string-all-matches
             "^  \\([a-z\\-]+\\) +"
             (shell-command-to-string "git svn help") 1))))

(autoload 'vc-git-root "vc-git")

(defun git-svn (dir command)
  "Run a git svn subcommand in DIR."
  (interactive (list (read-directory-name "Directory: ")
                     (completing-read "git-svn command: " (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn " command))))

(provide 'init-git-util)
;;; init-git-util.el ends here

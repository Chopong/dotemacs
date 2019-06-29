;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-lin* (eq system-type 'gnu/linux))
(defconst *is-a-win* (eq system-type 'windows-nt))

(setq user-emacs-directory (cond (*is-a-lin* "~/.emacs.d/")
				 (*is-a-mac* "/Volumes/SHARE/DotEmacs/")
                                 (*is-a-win* "E:/DotEmacs/")
				 (t "~/.emacs.d/")))
(add-to-list 'load-path
	     (expand-file-name "config" user-emacs-directory))
;;------------------------------------------------------------------------------
(require 'init-parameter)
(require 'init-plugins)
;;(package-initialize)
(require 'init-elpa)
(require 'init-functions)
(require 'init-pre-package)
(require 'init-locale)
;;------------------------------------------------------------------------------
(require 'init-frame-term)
(require 'init-frame-gui)
(require 'init-buffer)
(require 'init-window)
(require 'init-uniquify)
(require 'init-compile)

(require 'init-mode-line)
(require 'init-sidebar)
(require 'init-theme)
(require 'init-icons)
(require 'init-osx-keys)
(require 'init-search)
(require 'init-recentf)
(require 'init-sessions)
(require 'init-ivy)
(require 'init-mmm)


(require 'init-dired)
(require 'init-company)
(require 'init-xtags)
(require 'init-flycheck)
(require 'init-whitespace)
(require 'init-smex)
(require 'init-spell)
(require 'init-yasnippet)
(require 'init-git-util)
(require 'init-projectile)
(require 'init-editing-tools)
(require 'init-parentheses)
;;------------------------------------------------------------------------------
;;counsel-org-clock
;;(use-package org-page
;; :ensure t)
(require 'init-org)
(require 'init-org-agenda)
(require 'init-org-capture)
(require 'init-org-download)
(require 'init-org-mindmap)
(require 'init-org-pomodoro)
(require 'init-org-ql)
(require 'init-writeroom)


(require 'init-calendar)
(require 'init-elfeed)
;;------------------------------------------------------------------------------
(require 'init-http)
(require 'init-xxxml)
(require 'init-csv)
(require 'init-markdown)
(require 'init-slime)
(require 'init-common-lisp)
(require 'init-emacs-lisp)
(require 'init-clojure)
(require 'init-css)
;; (require 'init-javascript)
(require 'init-python)
(require 'init-ruby)
(require 'init-sql)
(require 'init-ess)

;;------------------------------------------------------------------------------
;; (require 'init-cnfonts)
;; (require 'init-game)
(require 'init-pos-package)
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here

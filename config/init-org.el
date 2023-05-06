;;; init-org.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-20,Mon,12:42:43
;;; Commentary:

;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including

;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX

;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.

;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:
(use-package org-bullets :ensure t :defer t
  :init
  (setq org-bullets-bullet-list
        '("◉" "○"))
  (add-hook 'org-mode-hook 'org-bullets-mode)
  )

(add-hook 'org-mode-hook (lambda ()
                           (org-bullets-mode t)
                           (yas-minor-mode-on)))
(setq org-ellipsis "⤵")
;;(use-package org-noter :ensure t :defer t)

(when *is-a-mac*
  (use-package grab-mac-link :ensure t))

;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; TODO: fail gracefully
(defun grab-ditaa (url jar-name)
  "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
  ;; TODO: handle errors
  (message "Grabbing %s for org." jar-name)
  (let ((zip-temp (make-temp-name "emacs-ditaa")))
    (unwind-protect
        (progn
          (when (executable-find "unzip")
            (url-copy-file url zip-temp)
            (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
                                   " " (shell-quote-argument jar-name) " > "
                                   (shell-quote-argument org-ditaa-jar-path)))))
      (when (file-exists-p zip-temp)
        (delete-file zip-temp)))))

(after-load 'ob-ditaa
  (unless (and (boundp 'org-ditaa-jar-path)
               (file-exists-p org-ditaa-jar-path))
    (let ((jar-name "ditaa0_9.jar")
          (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
      (setq org-ditaa-jar-path (expand-file-name jar-name user-assets-directory))
      (unless (file-exists-p org-ditaa-jar-path)
        (grab-ditaa url jar-name)))))

(after-load 'ob-plantuml
  (let ((jar-name "plantuml.jar")
        (url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (setq org-plantuml-jar-path (expand-file-name jar-name user-assets-directory))
    (unless (file-exists-p org-plantuml-jar-path)
      (url-copy-file url org-plantuml-jar-path))))
;; (use-package planyuml-mode :ensure t :defer t)




(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "STARTED(a!)" "WAIT(w@/!)" "OTHERS(o!)" "|" "DONE(d)" "CANCELLED(c)")
        ))

(setq org-tag-persistent-alist
      '(("@phone" . ?p)
        ("@computer" . ?c)
        ("@websurfing" . ?w)
        ("@errands" . ?e)
        ("@outdoors" . ?o)
        ("MIT" . ?m)
        ("BIGROCK" . ?b)
        ("CONTACTS" . ?C)
        ("INBOX" . ?i)
        ))

(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path 'file)

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file)
  )


(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                "tell application \"org-clock-statusbar\" to clock out"))))





;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")
;;; (setq org-default-notes-file "/Volumes/MacFile/Notebook/default.org")



(after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))





(after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (jupyter . t)
     (ruby . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . t)
     (sqlite . t))))


(use-package org-books :ensure t :defer t)

(use-package org-pdftools :ensure t :defer t)

(use-package org-fragtog :ensure t :defer t)

(setq org-src-fontify-natively t)

;; Org src coding hightlight
(setq org-src-fontify-natively t)
(use-package htmlize :ensure t :defer t
  :init
  (setq org-html-doctype "html5")
  (setq org-html-xml-declaration nil)
  (setq org-html-postamble nil)
  (setq org-html-head "<link rel='stylesheet' href='http://cdn.bootcss.com/bootstrap/3.3.0/css/bootstrap.min.css'>\n<link rel='stylesheet' href='http://cdn.bootcss.com/bootstrap/3.3.0/css/bootstrap-theme.min.css'>\n<script src='http://cdn.bootcss.com/jquery/1.11.1/jquery.min.js'>\n</script><script src='http://cdn.bootcss.com/bootstrap/3.3.0/js/bootstrap.min.js'></script>")) ;; for exporting html with hightlight
;; (use-package ox-latex :defer t
;;   :init
;;   (add-to-list 'org-latex-packages-alist '("" "minted"))
;;   (setq org-latex-listings 'minted)) ;; for exporting pdf with highlight

;; set latex to xelatex
(setq org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f"
                              "xelatex -shell-escape -interaction nonstopmode %f"))

;; #+LATEX_HEADER: \usepackage{xeCJK}
;; #+LATEX_HEADER: \setCJKmainfont{Songti SC}

(defun my-org-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq filename
        (concat (file-name-directory (buffer-file-name))
                "imgs/"
                (file-name-base (buffer-file-name))
                "_"
                basename
                ".png"))
  (call-process "screencapture" nil nil nil "-s" filename)
  (insert "#+CAPTION:")
  (insert basename)
  (insert "\n")
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(defadvice org-html-paragraph (before fsh-org-html-paragraph-advice
                                      (paragraph contents info) activate)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
  (let ((fixed-contents)
        (orig-contents (ad-get-arg 1))
        (reg-han "[[:multibyte:]]"))
    (setq fixed-contents (replace-regexp-in-string
                          ;; 这一行是匹配上一行末和下一行头都是中文的情况, 但是这样的话遇上"中文\nenglish"就仍然有空格
                          ;; (concat "\\(" reg-han "\\) *\n *\\(" reg-han "\\)")
                          (concat "\\(" reg-han "\\) *\n *")
                          "\\1" orig-contents))
    (ad-set-arg 1 fixed-contents)))

(provide 'init-org)
;;; init-org.el ends here

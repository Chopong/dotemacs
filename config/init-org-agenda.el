;;; init-org-agenda.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-21,Tue,16:06:39
;;; Commentary:

;;; Code:

(use-package org-super-agenda :ensure t :defer t)

;; (let ((org-super-agenda-groups
;;        '(;; Each group has an implicit boolean OR operator between its selectors.
;;          (:name "Today"  ; Optionally specify section name
;;                 :time-grid t  ; Items that appear on the time grid
;;                 :todo "TODAY")  ; Items that have this TODO keyword
;;          (:name "Important"
;;                 ;; Single arguments given alone
;;                 :tag "bills"
;;                 :priority "A")
;;          ;; Set order of multiple groups at once
;;          (:order-multi (2 (:name "Shopping in town"
;;                                  ;; Boolean AND group matches items that match all subgroups
;;                                  :and (:tag "shopping" :tag "@town"))
;;                           (:name "Food-related"
;;                                  ;; Multiple args given in list with implicit OR
;;                                  :tag ("food" "dinner"))
;;                           (:name "Personal"
;;                                  :habit t
;;                                  :tag "personal")
;;                           (:name "Space-related (non-moon-or-planet-related)"
;;                                  ;; Regexps match case-insensitively on the entire entry
;;                                  :and (:regexp ("space" "NASA")
;;                                                ;; Boolean NOT also has implicit OR between selectors
;;                                                :not (:regexp "moon" :tag "planet")))))
;;          ;; Groups supply their own section names when none are given
;;          (:todo "WAITING" :order 8)  ; Set order of this section
;;          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
;;                 ;; Show this group at the end of the agenda (since it has the
;;                 ;; highest number). If you specified this group last, items
;;                 ;; with these todo keywords that e.g. have priority A would be
;;                 ;; displayed in that group instead, because items are grouped
;;                 ;; out in the order the groups are listed.
;;                 :order 9)
;;          (:priority<= "B"
;;                       ;; Show this section after "Today" and "Important", because
;;                       ;; their order is unspecified, defaulting to 0. Sections
;;                       ;; are displayed lowest-number-first.
;;                       :order 1)
;;          ;; After the last group, the agenda will display items that didn't
;;          ;; match any of these groups, with the default order position of 99
;;          )))
;;   (org-agenda nil "a"))


(setq org-log-done 'time)
(setq org-agenda-start-on-weekday 0)

(setq org-agenda-files (list "/Volumes/MacFile/Notebook/org/inbox.org"
                             "/Volumes/MacFile/Notebook/org/email.org"
                             "/Volumes/MacFile/Notebook/org/tasks.org"
                             "/Volumes/MacFile/Notebook/org/wtasks.org"
                             "/Volumes/MacFile/Notebook/org/journal.org"
                             "/Volumes/MacFile/Notebook/org/wjournal.org"
                             "/Volumes/MacFile/Notebook/org/kb.org"
                             "/Volumes/MacFile/Notebook/org/wkb.org"
                             ))
(setq org-agenda-text-search-extra-files
      (list "/Volumes/MacFile/Notebook/org/someday.org"
            "/Volumes/MacFile/Notebook/org/config.org"
            ))

(setq org-refile-targets '((nil :maxlevel . 2)
                           (org-agenda-files :maxlevel . 2)
                           ("/Volumes/MacFile/Notebook/org/someday.org" :maxlevel . 2)
                           ("/Volumes/MacFile/Notebook/org/templates.org" :maxlevel . 2)
                           )
      )
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path 'file)

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here

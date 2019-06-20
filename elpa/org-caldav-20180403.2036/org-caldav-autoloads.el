;;; org-caldav-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-caldav" "org-caldav.el" (0 0 0 0))
;;; Generated autoloads from org-caldav.el

(autoload 'org-caldav-sync "org-caldav" "\
Sync Org with calendar.

\(fn)" t nil)

(autoload 'org-caldav-import-ics-buffer-to-org "org-caldav" "\
Add ics content in current buffer to `org-caldav-inbox'.

\(fn)" nil nil)

(autoload 'org-caldav-import-ics-to-org "org-caldav" "\
Add ics content in PATH to `org-caldav-inbox'.

\(fn PATH)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-caldav" '("org-caldav-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-caldav-autoloads.el ends here

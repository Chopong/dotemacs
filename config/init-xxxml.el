;;; init-xxxml.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Chopong
;; Email : chopong@aliyun.com
;; Date  : 2019-05-19,Sun,19:30:20
;;; Commentary:

;;; Code:


;; haml nxml sgml toml yaml html

(use-package web-mode :ensure t :defer t
  :mode ("\\.html?\\'" . 'web-mode)
  )

(use-package haml-mode :ensure t :defer 2
  :bind (:map haml-mode-map
	      ("C-o" . 'open-line)))

(use-package nxml-mode :ensure nil :defer t
  :init
  (add-auto-mode 'nxml-mode (concat "\\."
                                    (regexp-opt
                                     '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss"
                                       "gpx" "tcx" "plist"))
                                    "\\'"))
  (setq magic-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
  (fset 'xml-mode 'nxml-mode)
  (setq nxml-slash-auto-complete-flag t)
  :config
  
  ;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
  (defun pp-xml-region (beg end)
    "Pretty format XML markup in region. The function inserts
linebreaks to separate tags that have nothing but whitespace
between them.  It then indents the markup by using nxml's
indentation rules."
    (interactive "r")
    (unless (use-region-p)
      (setq beg (point-min)
	    end (point-max)))
    ;; Use markers because our changes will move END
    (setq beg (set-marker (make-marker) beg)
	  end (set-marker (make-marker) end))
    (save-excursion
      (goto-char beg)
      (while (search-forward-regexp "\>[ \\t]*\<" end t)
	(backward-char) (insert "\n"))
      (nxml-mode)
      (indent-region beg end)))
  ;;----------------------------------------------------------------------------
  ;; Integration with tidy for html + xml
  ;;----------------------------------------------------------------------------

  (defun tidy-buffer-xml (beg end)
    "Run \"tidy -xml\" on the region from BEG to END, or whole buffer."
    (interactive "r")
    (unless (use-region-p)
      (setq beg (point-min)
	    end (point-max)))
    (shell-command-on-region beg end "tidy -xml -q -i" (current-buffer) t "*tidy-errors*" t))
  )

(use-package sgml-mode :ensure nil :defer 2
  :init (use-package tagedit :ensure t :defer t)
  :mode ("\\.\\(jsp\\|tmpl\\)\\'" . 'html-mode)
  :hook (sgml-mode . tagedit-mode)
  :bind (:map tagedit-mode-map
	      ("M-?" . nil)
	      ("M-s" . nil))
  :config
  (tagedit-add-paredit-like-keybindings))

(use-package toml-mode :ensure t :defer 2
  :hook (toml-mode . goto-address-prog-mode))

(use-package yaml-mode :ensure t :defer 2
  :mode ("\\.yml\\.erb\\'" . yaml-mode)
  :hook (yaml-mode . goto-address-prog-mode))


(provide 'init-xxxml)
;; init-xxxml.el ends here

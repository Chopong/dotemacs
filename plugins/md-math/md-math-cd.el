;;; md-math-cd.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Wei Shaopeng
;; Email : Chopong@qq.com
;; Date  : 2021-08-02,一,21:32:03
;;; Commentary:

;;; Code:



(eval-when-compile (require 'cl-lib))

;;; Begin of Configuration Section ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configuration Variables and User Options for Md-Math ------------------

(defgroup md-math nil
  "Markdown Math support."
  :tag "md-math"
  :link '(url-link :tag "Home Page" "") ;; http://zon.astro.uva.nl/~dominik/Tools/
  :prefix "md-math-"
  :group 'markdown)

(defun md-math-customize ()
  "Call the customize function with md-math as argument."
  (interactive)
  (cond
   ((fboundp 'customize-browse)
    (customize-browse 'md-math))
   ((fboundp 'customize-group)
    (customize-group 'md-math))
   (t (error "No customization available"))))

(defun md-math-create-customize-menu ()
  "Create a full customization menu for Md-Math."
  (interactive)
  (if (fboundp 'customize-menu-create)
      (progn
        (easy-menu-change
         '("MDMATH") "Customize"
         `(["Browse Md-Math group" md-math-customize t]
           "---"
           ,(customize-menu-create 'md-math)
           ["Set" Custom-set t]
           ["Save" Custom-save t]
           ["Reset to Current" Custom-reset-current t]
           ["Reset to Saved" Custom-reset-saved t]
           ["Reset to Standard Settings" Custom-reset-standard t]))
        (message "\"MDMATH\"-menu now contains full customization menu"))
    (error "Cannot expand menu (outdated version of cus-edit.el)")))

;; Configuration of KEYWORD commands ------------------------------------

(defgroup md-math-keyword-commands nil
  "How to type a keyword in the buffer and hit TAB to execute."
  :group 'md-math)

(defcustom md-math-command-alist nil
  "List of abbrev-like commands, available with keyword and TAB.
See `md-math-command-alist-default' for examples.  This list only
defines additons to the defaults.  For a full list of active commands,
press \\[md-math-command-help].
Each element of this list is again a list with the following items:
0. KEYWORD     The key that has to be typed into the text.
1. DOCSTRING   A documentation string, less than 60 characters long.
2. REPLACE     The text to be substituted for the keyword, if any.
3. HOOK        A function to be called.
4. ARGS        Optional list of arguments to the function.
5. TEXTFLAG    non-nil means this keyword command is active in textmode.
6. MATHFLAG    non-nil means this keyword command is active in math mode."
  :group 'md-math-keyword-commands
  :type '(repeat
          (list (string   :tag "Keyword    ")
                (string   :tag "Docstring  ")
                (string   :tag "Replacement")
                (function :tag "Hook       ")
                (sexp     :tag "Arguments  ")
                (boolean  :tag "Available in Text mode")
                (boolean  :tag "Available in Math mode"))))

(defcustom md-math-tab-hook nil
  "List of functions called by TAB before the default command is executed.
These functions are called each time TAB is pressed.  They may parse the
environment and take an action.  The function should return t when it
successful executed an action and other TAB actions should *not* be tried.
When a return value is nil, other hook functions are tried, followed by the
default action of TAB (see documentation of the command `md-math-tab'."
  :group 'md-math-keyword-commands
  :type '(repeat (function :tag "Function" :value nil)))

;; Configuration of environment templates -------------------------------

(defgroup md-math-environment-support nil
  "Template-based insertion of LaTeX environments."
  :group 'md-math)

(defcustom md-math-env-alist nil
  "Association list of LaTeX environments and the corresponding templates.
The car of the list is a keyword to identify the environment.
the two following items in the list are the templates for environment
and item.  See `md-math-env-alist-default' for examples.  Any entries
in this variable will be added to the default definitions."
  :group 'md-math-environment-support
  :type '(repeat
          (list :tag ""
                (string :format "ENVIRONMENT %v" "")
                (text   :format "ENVIRONMENT TEMPLATE\n%v" "")
                (choice :tag "ITEM"
                        (const :tag "none" nil)
                        (text  :tag "template" :format "TEMPLATE\n%v" "")))))

(defcustom md-math-insert-auto-labels-in-env-templates t
  "*Non-nil means the environment templates Md-Math will contain labels.
This variable may be set to t, nil, or a string of label type letters
indicating the label types for which it should be true."
  :group 'md-math-making-and-inserting-labels
  :type '(choice :tag "Insert labels in templates"
                 (const  :tag "always" t)
                 (const  :tag "never" nil)
                 (string :tag "selected label types" "")))

;; Configuration of Math character insertion and accents ----------------

(defgroup md-math-math-support nil
  "Support for mathematical symbols and accents in Md-Math."
  :group 'md-math)

(defcustom md-math-math-symbol-prefix ?`
  "Prefix key for `md-math-math-symbol'.
This may be a character, a string readable with read-kbd-macro, or a
lisp vector."
  :group 'md-math-math-support
  :type '(choice
          (character)
          (string :value "" :tag "kbd readable string")
          (sexp :value [] :tag "a lisp vector")))

(defcustom md-math-math-symbol-direct-bindings '(nil nil nil)
  "How to bind the math symbols directly.
This is a list of key binding descriptions for different levels of
math symbols.  First entry for level 1 etc.
Each entry consists of a prefix key and a list of modifiers for the
character.  The prefix key can be nil, or any of a character, a
read-kbd-macro readable string or a vector.
Examples:
`((nil alt))'                   bind `\\delta' to `A-d'.
`((\"C-c C-f\"))'               bind `\\delta' to `C-c C-f d'.
`((nil alt) (nil alt control))' bind `\\delta' to `A-d' and
                                `\\partial' (which is on level 2)
                                to `A-C-d'"
  :group 'md-math-math-support
  :type '(repeat
          (choice
           (const :tag "No binding of this level" nil)
           (cons
            :tag "Specify a binding"
            :value (nil alt)
            (choice
             (const :tag "No prefix" nil)
             (character :value ?@)
             (string :value "" :tag "kbd readable string")
             (sexp :value [] :tag "a lisp vector"))
            (set :tag "Modifiers for the final character" :greedy t
                 (const control)
                 (const meta)
                 (const alt)
                 (const super)
                 (const hyper))))))

(defcustom md-math-math-symbol-alist nil
  "Key characters and math symbols for fast access with the prefix key.
First element is a character, followed by a number of strings attached to
this key.  When the string contains a question mark, this is where the
cursor will be positioned after insertion of the string into the buffer.
See `md-math-math-symbol-alist-default' for an example.  Any entry defined
here will replace the corresponding entry of the default list.  The
defaults implement 3 levels of symbols so far: Level 1 for greek letters
and standard symbols, level 2 for variations of level 1, and level 3 for
functions and opperators."
  :group 'md-math-math-support
  :type '(repeat
          (list
           (character ?a)
           (repeat (string :tag "macro" "")))))

(defcustom md-math-math-modify-prefix ?'
  "Prefix key for `md-math-math-modify'.
It can be a character, a string interpretable with `read-kbd-macro',
or a lisp vector."
  :group 'md-math-math-support
  :type '(choice
          (character)
          (string :value "" :tag "kbd readable string")
          (sexp :value [] :tag "a lisp vector")))

(defcustom md-math-modify-backwards t
  "*Non-nil means, `md-math-math-modify' modifies char before point.
Nil means, always insert only an empty modification form.  This is also
the case if the character before point is white or some punctuation. "
  :group 'md-math-math-support
  :type 'boolean)

(defcustom md-math-math-modify-alist nil
  "List description of the LaTeX math accents.
See `md-math-math-modify-alist-default' for an example.  Any entries in this
variable will be added to the default.
Each element contains 6 items:
0. key:      The character that is the key for a the accent.
1. mathcmd:  The LaTeX command associated with the accent in math mode
2. textcmd:  The LaTeX command associated with the accent in text mode
3. type:     t   if command with argument (e.g. \\tilde{a}).
             nil if style (e.g. {\\cal a}).
4. rmdot:    t   if the dot on i and j has to be removed.
5. it        t   if italic correction is required."
  :group 'md-math-math-support
  :type '(repeat
          (list (character :tag "Key character ")
                (choice :tag "TeX macro inside  math mode"
                        (string "")
                        (const :tag "none" nil))
                (choice :tag "TeX macro outside math mode"
                        (string "")
                        (const :tag "none" nil))
                (boolean :tag "Type             " :on "Command" :off "Style")
                (boolean :tag "Remove dot in i/j")
                (boolean :tag "Italic correction"))))

(defcustom md-math-make-sub-superscript-roman-if-pressed-twice nil
  "*Non-nil means, pressing `^` or `_' twice inserts roman sub/superscript."
  :group 'md-math-math-support
  :type 'boolean)

(defcustom md-math-use-dollar-to-ensure-math t
  "*Non-nil means, use $...$ to force a math mode setting where needed.
When nil, use \\(...\\) instead."
  :group 'md-math-math-support
  :type '(boolean))

;; Miscellaneous configurations -----------------------------------------

(defgroup md-math-miscellaneous-configurations nil
  "Collection of further configurations."
  :group 'md-math)

(defcustom md-math-use-fonts t
  "*Non-nil means, use fonts in label menu and on-the-fly help.
Font-lock must be loaded as well to actually get fontified display."
  :group 'md-math-miscellaneous-configurations
  :type '(boolean))

(defcustom md-math-takeover-parenthesis t
  "*Non-nil means, md-math is allowed to take over the parenthesis insertion.
THis means it will redefine the `(', `{', and `[' keys."
  :group 'md-math-miscellaneous-configurations
  :type '(boolean))

(defcustom md-math-takeover-dollar t
  "*Non-nil means, md-math is allowed to take over the $.
THis means it will redefine the `$' keys."
  :group 'md-math-miscellaneous-configurations
  :type '(boolean))

(defcustom md-math-takeover-subsuperscript t
  "*Non-nil means, md-math is allowed to take over the ^ and _ keys."
  :group 'md-math-miscellaneous-configurations
  :type '(boolean))

(defcustom md-math-paired-parens "$[{"
  "*String with the opening parens you want to have inserted paired.
The following parens are allowed here: `$([{|<'.
I recommend to set this to '$[{' as these have syntactical meaning in
TeX and are required to be paired.  TAB is a good way to move out of paired
parens."
  :group 'md-math-miscellaneous-configurations
  :type '(string :tag "Opening delimiters"))

(defcustom md-math-simplify-sub-super-scripts t
  "*Non-nil means, TAB will simplify sub- and superscripts at point.
When you use TAB to exit from a sub- or superscript which is a single
letter, the parenthesis will be removed."
  :group 'md-math-miscellaneous-configurations
  :type '(boolean))

(defcustom md-math-sub-super-scripts-outside-math-mode t
  "*Non-nil means, inserting ^ or _ will add dollars outside math environment.
So in text mode surrounding dollars and braces will be added with `_' and `^'.
When nil, `_' and `^' will just self-insert."
  :group 'md-math-miscellaneous-configurations
  :type '(boolean))

(defcustom md-math-auto-help-delay 1.5
  "Number of idle seconds before display of auto-help.
When executing md-math-math-symbol or md-math-math-modify, display
automatic help when idle for more than this amount of time."
  :group 'md-math-miscellaneous-configurations
  :type 'number)

(require 'texmathp)

;;;============================================================================
;;;
;;; Define the formal stuff for a minor mode named Md-Math.
;;;

(defun md-math-show-commentary ()
  "Use the finder to view the file documentation from `md-math.el'."
  (interactive)
  (require 'finder)
  (finder-commentary "md-math.el"))

(defconst md-math-version "0"
  "Version string for Md-Math.")

(defvar md-math-mode nil
  "Determines if Md-Math minor mode is active.")
(make-variable-buffer-local 'md-math-mode)

(defvar md-math-mode-map (make-sparse-keymap)
  "Keymap for Md-Math minor mode.")
(defvar md-math-mode-menu nil)

;;;###autoload
(defun turn-on-md-math ()
  "Turn on Md-Math minor mode."
  (md-math-mode t))

;;;###autoload
(defun md-math-mode (&optional arg)
  "Minor mode for editing scientific LaTeX documents.  Here is a
list of features: \\<md-math-mode-map>

                           KEYWORD COMMANDS
                           ----------------
Many Md-Math commands are activated with an abbrev-like mechanism.
When a keyword is typed followed `\\[md-math-tab]', the keyword is deleted
from the buffer and a command is executed.  You can get a full list
of these commands with `\\[md-math-command-help]'.
For example, when you type `fr<TAB>', Md-Math will insert `\\frac{}{}'.

When inserting templates like '\\frac{}{}', the cursor is positioned
properly.  Use `\\[md-math-tab]' to move through templates.  `\\[md-math-tab]' also kills
unnecessary braces around subscripts and superscripts at point.

                     MATH CHARACTERS AND ACCENTS
                     ---------------------------
\\[md-math-math-symbol]  followed by any character inserts a LaTeX math character
      e.g. \\[md-math-math-symbol]e   => \\epsilon
\\[md-math-math-symbol]\\[md-math-math-symbol] followed by any character inserts other LaTeX math character
      e.g. \\[md-math-math-symbol]\\[md-math-math-symbol]e  => \\varepsilon
\\[md-math-math-modify]  followed by character puts a math accent on a letter or symbol
      e.g. \\[md-math-math-symbol]a\\[md-math-math-modify]~ => \\tilde{\\alpha}

Md-Math is aware of the math environments in LaTeX and modifies the
workings of some functions according to the current status.

                             ONLINE HELP
                             -----------
After pressing \\[md-math-math-symbol] or \\[md-math-math-modify], Md-Math waits for a short time for the second character.
If that does not come, it will pop up a window displaying the available
characters and their meanings.

                             KEY BINDINGS
                             ------------
\\{md-math-mode-map}

Under X, many functions will be available also in a menu on the menu bar.

Entering md-math-mode calls the hook md-math-mode-hook.
------------------------------------------------------------------------------"

  (interactive "P")
  (setq md-math-mode (not (or (and (null arg) md-math-mode)
                              (<= (prefix-numeric-value arg) 0))))

                                        ; Add or remove the menu, and run the hook
  (if md-math-mode
      (progn
        (easy-menu-add md-math-mode-menu)
        (run-hooks 'md-math-mode-hook)
        (md-math-compute-tables))
    (easy-menu-remove md-math-mode-menu)))

(or (assoc 'md-math-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(md-math-mode " MdMat") minor-mode-alist)))

(or (assoc 'md-math-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'md-math-mode md-math-mode-map)
                minor-mode-map-alist)))


;;; ===========================================================================
;;;
;;; Functions that check out the surroundings

(defun md-math-dollars-balanced-to-here (&optional from)
  ;; Return t if the dollars are balanced between start of paragraph and point.
  (save-excursion
    (let ((answer t) (pos (point)))
      (if from
          (goto-char from)
        (backward-paragraph 1))
      (if (not (bobp)) (backward-char 1))
      (while (re-search-forward "[^\\]\\$+" pos t)
        (if (/= (char-after (match-beginning 0)) ?\\)
            (setq answer (not answer))))
      (setq answer answer))))

(defun md-math-number-of-backslashes-is-odd ()
  ;; Count backslashes before point and return t if number is odd.
  (let ((odd nil))
    (save-excursion
      (while (equal (preceding-char) ?\\)
        (progn
          (forward-char -1)
          (setq odd (not odd)))))
    (setq odd odd)))

;; ============================================================================
;;
;; Some generally useful functions

(defun md-math-get-kbd-vector (obj)
  (cond ((vectorp obj) obj)
        ((integerp obj) (vector obj))
        ((stringp obj) (read-kbd-macro obj))
        ((and (fboundp 'characterp) (characterp obj))
         (vector obj))        ; XEmacs only
        (t nil)))

(defun md-math-uniquify (alist &optional keep-list)
  ;; Return a list of all elements in ALIST, but each car only once.
  ;; Elements of KEEP-LIST are not removed even if duplicate.
  (let (new elm)
    (while alist
      (setq elm (car alist)
            alist (cdr alist))
      (if (or (member (car elm) keep-list)
              (not (assoc (car elm) new)))
          (setq new (cons elm new))))
    (setq new (nreverse new))
    new))

(defun md-math-use-fonts ()
  ;; Return t if we can and want to use fonts.
  (and window-system
       md-math-use-fonts
       (boundp 'font-lock-keyword-face)))

;;; ---------------------------------------------------------------------------
;;;
;;; Insert pairs of $$ (), etc.

;; Alist connection opening with closing delimiters
(defconst md-math-parens-pairs '(("(".")") ("["."]") ("{"."}")
                                 ("|"."|") ("<".">")))

(defun md-math-pbb ()
  "Insert a pair of parens, brackets or braces."
  (interactive)
  (let ((paren (char-to-string (event-basic-type last-command-event))))
    (if (and (stringp md-math-paired-parens)
             (string-match (regexp-quote paren) md-math-paired-parens)
             (not (md-math-number-of-backslashes-is-odd)))
        (progn
          (insert paren)
          (insert (cdr (assoc paren md-math-parens-pairs)))
          (forward-char -1))
      (insert paren))))

(defun md-math-ensure-math ()
  ;; Make sure we are in math
  (unless (texmathp)
    (if md-math-use-dollar-to-ensure-math
        (md-math-dollar)
      (insert "\\(\\)")
      (backward-char 2))))

(defun md-math-dollar (&optional arg)
  "Insert a pair of dollars unless number of backslashes before point is odd.
With arg, insert pair of double dollars."
  (interactive "P")
  (if (md-math-number-of-backslashes-is-odd)
      (insert "$")
    (if (texmathp)
        (if (and (stringp (car texmathp-why))
                 (equal (substring (car texmathp-why) 0 1) "\$"))
            (progn
              (insert (car texmathp-why))
              (save-excursion
                (goto-char (cdr texmathp-why))
                (if (pos-visible-in-window-p)
                    (sit-for 1))))
          (message "No dollars inside a math environment!")
          (ding))
      (if (and (stringp md-math-paired-parens)
               (string-match "\\$" md-math-paired-parens))
          (if arg
              (if (bolp)
                  (progn (insert "\$\$\n\n\$\$\n") (backward-char 4))
                (insert "\$\$  \$\$") (backward-char 3))
            (insert "$$") (backward-char 1))
        (if arg
            (if (bolp) (insert "$$\n") (insert "$$"))
          (insert "$"))))))

(defun md-math-sub-superscript ()
  "Insert ^{} or _{} unless the number of backslashes before point is odd.
When not in LaTeX math environment, _{} and ^{} will have dollars.
When pressed twice, make the sub/superscript roman."
  (interactive)
  (if (and md-math-make-sub-superscript-roman-if-pressed-twice
           (equal this-command last-command))
      (insert "\\rm ")
    (if (md-math-number-of-backslashes-is-odd)
        ;; Quoted
        (insert (event-basic-type last-command-event))
      ;; Check if we are in math mode, if not switch to or only add _ or ^
      (if (and (not (texmathp))
               (not md-math-sub-super-scripts-outside-math-mode))
          (insert (event-basic-type last-command-event))
        (if (not (texmathp)) (md-math-ensure-math))
        ;; Insert the normal template.
        (insert (event-basic-type last-command-event))
        (insert "{}")
        (forward-char -1)))))

(defun md-math-lr-pair ()
  "Insert a \\left-\\right pair of parens."
  (interactive)
  (let* ((paren (char-to-string (preceding-char)))
         (close (cdr (assoc paren md-math-parens-pairs)))
         (paren1 paren)
         (close1 close))
    (if (string= paren "<") (setq paren1 "\\langle" close1 "\\rangle"))
    (if (string= paren "{") (setq paren1 "\\{" close1 "\\}"))
    (backward-delete-char 1)
    (if (and (stringp md-math-paired-parens)
             (string-match (regexp-quote paren) md-math-paired-parens)
             (string= close (char-to-string (following-char))))
                                        ; parens are inserted paired, and there is already a closing parenthesis
        (delete-char 1))
    (insert "\\left" paren1 " ? \\right" close1)
    (md-math-position-cursor)))

;;; ===========================================================================
;;;
;;; Keyword controlled commands and cursor movement

(defvar md-math-command-alist-comb nil)

(defun md-math-tab ()
  "This function is intended to do many cursor movements.
It is bound to the tab key since tab does nothing useful in a TeX file.

This function first calls all functions in `md-math-tab-hook', which see.

If none of those functions returns t, the command  first tries to expand
any command keyword before point.

If there is none, it cleans up short subscripts and superscripts at point.
I.e. it changes a^{2} into a^2, since this is more readable.  This feature
can be disabled by setting `md-math-simplify-sub-super-scripts' to nil.

Then it jumps to the next point in a LaTeX text where one would reasonably
expect that more input can be put in.
To do that, the cursor is moved according to the following rules:

The cursor stops...
- before closing brackets if preceding-char is any of -({[]})
- after  closing brackets, but not if following-char is any of ({[_^
- just after $, if the cursor was before that $.
- at end of non-empty lines
- at the beginning of empty lines
- before a SPACE at beginning of line
- after first of several SPACE

Sounds strange?  Try it out!"
  (interactive)
  (catch 'stop

    ;; try hook stuff
    (let ((funcs md-math-tab-hook))
      (while funcs (if (funcall (pop funcs)) (throw 'stop t))))

    ;; try command expansion
    (let ((pos (point)) exp math-mode)
      (backward-word 1)
      (while (eq (following-char) ?$) (forward-char 1))
      (setq exp (buffer-substring-no-properties (point) pos))
      (setq exp (assoc exp md-math-command-alist-comb))
      (when exp
        (setq math-mode (texmathp))
        (when (or (and (not math-mode) (nth 5 exp))
                  (and math-mode (nth 6 exp)))
          (delete-char (- pos (point)))
          (insert (nth 2 exp))
          ;; call the function if there is one defined
          (and (nth 3 exp)
               (if (nth 4 exp)
                   (apply (nth 3 exp) (nth 4 exp))
                 (funcall (nth 3 exp))))
          (throw 'stop t)))
      (goto-char pos))

    ;; Check for simplification of sub and superscripts
    (cond
     ((looking-at "}\\|\\]\\|)")
      (forward-char -3)
      (if (and (looking-at "[_^]{[-+0-9a-zA-Z]}")
               md-math-simplify-sub-super-scripts)
          ;; simplify sub/super script
          (progn (forward-char 1)
                 (delete-char 1)
                 (forward-char 1)
                 (delete-char 1))
        (forward-char 4))
      (if (looking-at "[^_^({[]")
          ;; stop after closing bracket, unless ^_[{( follow
          (throw 'stop t)))
     ((= (following-char) ?$)
      (while (= (following-char) ?$) (forward-char 1))
      (throw 'stop t))
     ((= (following-char) ?\ )
      ;; stop after first of many spaces
      (forward-char 1)
      (re-search-forward "[^ ]")
      (if (/= (preceding-char) ?\n) (forward-char -1)))
     (t
      (forward-char 1)))

    ;; move to next possible stopping site and check out the place
    (while (re-search-forward "[ )}\n]\\|\\]" (point-max) t)
      (forward-char -1)
      (cond
       ((= (following-char) ?\ )
        ;; stop at first space or b-o-l
        (if (not (bolp)) (forward-char 1)) (throw 'stop t))
       ((= (following-char) ?\n)
        ;; stop at line end, but not after \\
        (if (and (bolp) (not (eobp)))
            (throw 'stop t)
          (if (equal "\\\\" (buffer-substring-no-properties
                             (- (point) 2) (point)))
              (forward-char 1)
            (throw 'stop t))))
       (t
        ;; Stop before )}] if preceding-char is any parenthesis
        (if (or (= (char-syntax (preceding-char)) ?\()
                (= (char-syntax (preceding-char)) ?\))
                (= (preceding-char) ?-))
            (throw 'stop t)
          (forward-char 1)
          (if (looking-at "[^_\\^({\\[]")
              ;; stop after closing bracket, unless ^_[{( follow
              (throw 'stop t))))))))

(defun md-math-command-help (&optional arg)
  "Show the available md-math commands in the help buffer."
  (interactive "P")
  (if arg
      (call-interactively 'TeX-documentation-texdoc)
    (with-output-to-temp-buffer " *Md-Math Help*"
      (princ "                    AVAILABLE KEYWORD COMMANDS WITH Md-Math\n")
      (princ "                    --------------------------------------\n")
      (princ "To execute, type keyword into buffer followed by TAB.\n\n")
      (let ((cmdlist md-math-command-alist-comb) item key doc text math)
        (while cmdlist
          (setq item (car cmdlist)
                cmdlist (cdr cmdlist)
                key (car item)
                doc (nth 1 item)
                text (nth 5 item)
                math (nth 6 item))
          (princ (format "%-10.10s %-58.58s %4s/%4s\n" key
                         (if (> (length doc) 59)
                             (substring doc 0 59)
                           doc)
                         (if text "TEXT" "")
                         (if math "MATH" ""))))))))

;;; ---------------------------------------------------------------------------
;;;
;;; Cursor position after insertion of forms

(defun md-math-position-cursor ()
  ;; Search back to question mark, delete it, leave point there
  (if (search-backward "\?" (- (point) 100) t)
      (delete-char 1)))

;;; ---------------------------------------------------------------------------
;;;
;;; Environments
;;;
;;; The following code implements insertion of LaTeX environments
;;; I prefer these environment over AUCTeX's definitions, since they give
;;; my memory more support and don't prompt for anything.

(defvar md-math-env-alist-comb nil)

(defun md-math-environment (&optional environment item)
  "Complete the name of an ENVIRONMENT and insert it.
If the environment is not found in the list, a \\begin \\end pair is
inserted.  Any keywords AUTOLABEL will be replaced by an automatic label
statement.  Any keywords AUTOFILE will prompt the user for a file name
\(with completion) and insert that file names.  If a template starts with
\"\\\\\", the function will make sure that a double backslash occurs before
the template.  This is mainly useful for \"items\" of environments, where
\"\\\\\" is often needed as separator."
  (interactive)
  (if (md-math-region-active-p)
      (md-math-wrap-environment environment)
    (let ((env environment) begpos (endmarker (make-marker))
          (auto-label md-math-insert-auto-labels-in-env-templates)
          template)
      (if (not env)
          (setq env (completing-read "Environment: " md-math-env-alist-comb nil nil "")))
      (if (not (bolp)) (newline))
      (setq begpos (point))
      (if (try-completion env md-math-env-alist-comb)
          (progn
            (setq template (nth (if item 2 1)
                                (assoc env md-math-env-alist-comb)))
            (if (string= (substring template 0 2) "\\\\")
                ;; Need a double backslash to teminate previous item
                (progn
                  (setq template (substring template 2))
                  (if (not (save-excursion
                             (re-search-backward "\\\\\\\\[ \t\n]*\\="
                                                 (- (point) 20) t)))
                      (save-excursion
                        (skip-chars-backward " \t\n")
                        (insert "\\\\")))))
            (insert template))
        (insert "\\begin{" env "}\n?\n\\end{" env "}\n"))
      (move-marker endmarker (point))

      ;; Look for AUTOFILE requests
      (goto-char begpos)
      (while (search-forward "AUTOFILE" (marker-position endmarker) t)
        (backward-delete-char 8)
        (call-interactively 'md-math-insert-filename))

      ;; Look for AUTOLABEL requests
      (goto-char begpos)
      (while (search-forward "AUTOLABEL" (marker-position endmarker) t)
        (backward-delete-char 9)
        (if (and auto-label (fboundp 'reftex-label))
            (reftex-label env)
          (save-excursion
            (beginning-of-line 1)
            (if (looking-at "[ \t]*\n")
                (kill-line 1)))))

      ;; Position cursor at the first question-mark
      (goto-char begpos)
      (if (search-forward "?" (marker-position endmarker) t)
          (backward-delete-char 1)))))

(defun md-math-wrap-environment (&optional environment)
  "Wrap the active region into ENVIRONMENT.
If the environment is not given, ask for it using completion."
  (let ((env environment)
        (beg (move-marker (make-marker) (region-beginning)))
        (end (move-marker (make-marker) (region-end))))
    (if (not env)
        (setq env (completing-read "Environment: "
                                   md-math-env-alist-comb nil nil "")))
    (goto-char beg)
    (if (not (looking-back "^[ \t]*" (point-at-bol))) (newline))
    (insert "\\begin{" env "}\n")
    (goto-char end)
    (if (not (looking-back "^[ \t]*" (point-at-bol))) (newline))
    (insert "\\end{" env "}\n")
    ))

(defun md-math-item ()
  "Insert an \\item and provide a label if the environments supports that.
In eqnarrays this inserts a new line with two ampersands.  It will also
add two backslashes to the previous line if required."
  (interactive)
  (let* ((env (car (car (reftex-what-environment t))))
         (envl (assoc env md-math-env-alist-comb)))

    (if (not env) (error "No open environment at point."))
    (if (or (< (length envl) 3)
            (null (nth 2 envl))
            (and (stringp (nth 2 envl))
                 (string= (nth 2 envl) "")))
        (error "No item defined for %s environment." env))
    (md-math-environment env t)))

(defun md-math-comment-at-point ()
  ;; Return t if point is inside a TeX comment
  (let ((end (point))
        (start (progn (beginning-of-line 1) (point))))
    (goto-char end)
    (save-match-data
      (string-match "^%\\|[^\\]%" (buffer-substring start end)))))

(defun md-math-insert-filename (&optional absolute)
  (interactive "P")
  "Insert a file name, with completion.
The path to the file will be relative to the current directory if the file
is in the current directory or a subdirectory.  Otherwise, the link will
be as completed in the minibuffer (i.e. normally relative to the users
HOME directory).
With optional prefix ABSOLUTE, insert the absolute path."
  (let ((file (read-file-name "File: " nil "")))
    (let ((pwd (file-name-as-directory (expand-file-name "."))))
      (cond
       (absolute
        (insert (expand-file-name file)))
       ((string-match (concat "^" (regexp-quote pwd) "\\(.+\\)")
                      (expand-file-name file))
        (insert (match-string 1 (expand-file-name file))))
       (t (insert (expand-file-name file)))))))


;;; ===========================================================================
;;;
;;; Math characters and modifiers

;; The actual value of the following variable is calculated
;; by `md-math-compute-tables'.  It holds the number of levels of math symbols
(defvar md-math-math-symbol-no-of-levels 1)
(defvar md-math-math-symbol-alist-comb nil)
(defvar md-math-math-modify-alist-comb nil)

(defvar zmacs-regions)
(defun md-math-region-active-p ()
  "Is `transient-mark-mode' on and the region active?
Works on both Emacs and XEmacs."
  (if (featurep 'xmeacs)
      (and zmacs-regions (region-active-p))
    (and transient-mark-mode mark-active)))

(defun md-math-math-symbol ()
  "Read a char from keyboard and insert corresponding math char.
The combinations are defined in `md-math-math-symbol-alist'.  If not in a LaTeX
math environment, you also get a pair of dollars."
  (interactive)
  (let* ((cell (md-math-read-char-with-help
                md-math-math-symbol-alist-comb
                1 md-math-math-symbol-no-of-levels
                "Math symbol level %d of %d: "
                "AVAILABLE MATH SYMBOLS.  [%c]=next level "
                md-math-math-symbol-prefix
                (get 'md-math-math-symbol-alist-comb 'md-math-bindings)))
         (char (car cell))
         (level (cdr cell))
         (entry (assoc char md-math-math-symbol-alist-comb))
         (symbol (nth level entry)))

    (if (or (not symbol)
            (not (stringp symbol))
            (equal symbol ""))
        (error "No such math symbol %c on level %d" char level))

    (if (or (not (texmathp))
            (md-math-number-of-backslashes-is-odd))
        (md-math-ensure-math))

    (insert symbol)
    (when (string-match "\\?" symbol)
      (md-math-position-cursor))))

(defun md-math-read-char-with-help (alist start-level max-level prompt-format
                                          header-format prefix bindings)
  "Read a char from keyboard and provide help if necessary."
  (interactive)
  (let (char (help-is-on nil)
             (level start-level))
    (catch 'exit
      (save-window-excursion
        (while t
          (if help-is-on
              (progn
                (md-math-turn-on-help
                 (concat (format header-format prefix)
                         (if (assoc level bindings)
                             (concat "  Direct binding are `"
                                     (cdr (assoc level bindings)) "' etc.")
                           ""))
                 level alist help-is-on nil)))
          (message prompt-format level max-level)
          (if (and (not help-is-on)
                   (sit-for md-math-auto-help-delay))
              (setq char ?\?)
            (setq char (read-char)))
          (cond
           ((= char ?\C-g)
            (keyboard-quit))
           ((= char ?\?)
            (if help-is-on
                (progn
                  (setq help-is-on (+ help-is-on (- (window-height) 1)))
                  (if (> help-is-on (count-lines (point-min) (point-max)))
                      (setq help-is-on 1)))
              (setq help-is-on 1)))
           ((or (= char ?\ )
                (equal char prefix))
            (setq level (if (= level md-math-math-symbol-no-of-levels)
                            1
                          (1+ level))))
           (t (throw 'exit (cons char level)))))))))

;;; The following code implements the possibility to modify a character
;;; by an accent or style when point is behind it.  This is more naturally
;;; then the usual way.  E.g. \tilde{a}  can be typed as a'~

(defun md-math-math-modify (arg)
  "Modify previous char/group/macro with math accent/style.
This macro modifies the character or TeX macro or TeX group BEFORE point
with a math accent or a style.
If the character before point is white space, an empty modifying form
is inserted and the cursor positioned properly.
If the object before point looks like word, this macro modifies the last
character of it.
All this happens only, when the cursor is actually inside a LaTeX math
environment.  In normal text, it does just a self-insert.
The accent and style commands and their properties are defined in the
constant `md-math-math-modify-alist'."
  (interactive "P")
  (catch 'exit

    (let ((inside-math (texmathp))
          (win (selected-window))
          char (help-is-on nil) ass acc rmdot it cmd extrabrac)
      (catch 'exit1
        (save-window-excursion
          (while t
            (if help-is-on
                (progn
                  (md-math-turn-on-help
                   "AVAILABLE MODIFIERS. (?=SCROLL)"
                   (if inside-math 1 2)
                   md-math-math-modify-alist-comb help-is-on t)
                  (message "Math modify: "))
              (message "Math modify: (?=HELP)"))

            (if (and (not help-is-on)
                     (sit-for md-math-auto-help-delay))
                (setq char ?\?)
              (setq char (read-char)))

            (cond
             ((= char ?\C-g)
              (keyboard-quit))
             ((= char ?\?)
              (if help-is-on
                  (progn
                    (setq help-is-on (+ help-is-on (- (window-height) 1)))
                    (if (> help-is-on (count-lines (point-min) (point-max)))
                        (setq help-is-on 1)))
                (setq help-is-on 1)))
             ((equal char md-math-math-modify-prefix)
              (select-window win)
              (insert md-math-math-modify-prefix)
              (message "")
              (throw 'exit t))
             (t (throw 'exit1 t))))))
      (message "")
      (setq ass (assoc char md-math-math-modify-alist-comb))
      (if (not ass)
          (progn
            (insert md-math-math-modify-prefix char)
            (throw 'exit t)))
      (setq ass    (cdr ass))
      (setq cmd    (nth (if inside-math 0 1) ass))
      (setq acc    (nth 2 ass))
      (setq rmdot  (nth 3 ass))
      (setq it     (nth 4 ass))
      (if (not cmd) (error "No such modifier `%c' %s math mode." char
                           (if inside-math "inside" "outside")))
      (cond
       ((md-math-region-active-p)
        (let ((beg (min (region-beginning) (region-end)))
              (end (max (region-beginning) (region-end))))
          (goto-char end)
          (point-to-register ?x)
          (goto-char beg)
          (insert "{")
          (if acc (forward-char -1))
          (insert cmd)
          (if (not acc) (insert " "))
          (register-to-point ?x)
          (insert "}")))
       (arg
        (point-to-register ?x)
        (backward-word arg)
        (insert "{")
        (if acc (forward-char -1))
        (insert cmd)
        (if (not acc) (insert " "))
        (register-to-point ?x)
        (insert "}"))
       ((or (bolp)
            (not md-math-modify-backwards)
            (memq (preceding-char) '(?\  ?$ ?- ?{ ?\( )))
        ;; Just insert empty form and position cursor
        (if acc
            (insert cmd "{?")
          (insert "{" cmd " ?"))
        (if it (insert "\\/"))
        (insert "}")
        (search-backward "?")
        (delete-char 1))
       (t
        ;; Modify preceding character or word
        (point-to-register ?x)
        (if (= (preceding-char) ?\})
            ;; its a group
            (progn (setq extrabrac nil)
                   (backward-list 1)
                   (if (not acc) (forward-char 1)))
          ;; not a group
          (forward-char -1)
          (if (looking-at "[a-zA-Z]")
              ;; a character: look if word or macro
              (progn
                (setq extrabrac t)
                (re-search-backward "[^a-zA-Z]")
                (cond
                 ((= (following-char) ?\\))
                 ((not inside-math) (forward-char 1))
                 (t (register-to-point ?x)
                    (forward-char -1)
                    (if (and rmdot (looking-at "[ij]"))
                        (progn (insert "\\")
                               (forward-char 1)
                               (insert "math")
                               (point-to-register ?x)
                               (forward-char -6))))))
            (setq extrabrac t)))
        (if extrabrac (progn (insert "{")
                             (if acc (forward-char -1))))
        (insert cmd)
        (if (not acc) (insert " "))
        (register-to-point ?x)
        (if extrabrac (insert "}")))))))

;;; And here is the help function for the symbol insertions stuff

(defun md-math-turn-on-help (header level alist offset &optional sparse)
  ;; Show help-window for alist
  (let ((cnt 0) (all-chars "")
        (flock (md-math-use-fonts)) this-char value)
    (if sparse
        (setq all-chars (concat (mapcar 'car alist)))
      (setq all-chars "aA0 bB1!cC2@dD3#eE4$fF5%gG6^hH7&iI8
jJ9?kK+~lL-_mM*|nN/\\oO=\"pP()qQ[]rR{}sS<>tT`'uU.:vV

wW

xX

yY

zZ

"))
    (if (get-buffer-window " *Md-Math Help*")
        (select-window (get-buffer-window " *Md-Math Help*"))
      (switch-to-buffer-other-window " *Md-Math Help*"))
    (if buffer-read-only (read-only-mode 'toggle))
    (erase-buffer)
    (make-local-variable 'truncate-lines)
    (setq truncate-lines t)
    (insert (concat header "\n\n"))

    (while (not (equal "" all-chars))
      (setq cnt (1+ cnt))
      (setq this-char (string-to-char all-chars))
      (setq all-chars (substring all-chars 1))
      (cond
       ( (= this-char ?\?)
         (setq value "SCROLL"))
       ( (= this-char ?\C-m)
         (setq this-char ?\ )
         (setq value ""))
       ( t
         (setq value (nth level (assoc this-char alist)))
         (if (not value) (setq value ""))))
      (setq this-char (char-to-string this-char)
            value (if (> (length value) 15)
                      (concat (substring value 0 13) "..")
                    (substring (concat value "               ") 0 15)))
      (if flock
          (put-text-property 0  15
                             'face 'font-lock-keyword-face value))

      (insert this-char "  " value "  ")
      (if (= (* 4 (/ cnt 4)) cnt) (insert "\n")))
    (unless (one-window-p t)
      (enlarge-window (1+(- (count-lines 1 (point)) (window-height)))))
    (goto-char (point-min)) (forward-line (1- offset))
    (beginning-of-line 1)
    (recenter 0)))

;;; ---------------------------------------------------------------------------
;;;
;;; Data Section: Definition of large constants

(defconst md-math-command-alist-default
  '(
    ("pref"      "Make page reference"
     "" reftex-reference nil t t)
    ("ref"       "Make reference"
     "" reftex-reference nil t t)

    ("lbl"       "Insert automatic label at point"
     "" reftex-label nil t t)

    ("ct"        "Insert \\cite"
     "\\cite{?}" md-math-position-cursor nil t nil)
    ("cte"       "Make a citation interactively"
     "" reftex-citation nil t nil)
    ("cite{"       "Make a citation interactively"
     "cite{" reftex-citation nil t nil)

    ("beg"       "Complete an environment name and insert template"
     "" md-math-environment nil t t)
    ("env"       "Complete an environment name and insert template"
     "" md-math-environment nil t t)
    ("it"        "New item in current environment"
     "" md-math-item nil t t)
    ("ite"       "Insert an ITEMIZE environment template"
     "" md-math-environment ("itemize") t nil)
    ("enu"       "Insert an ENUMERATE environment template"
     "" md-math-environment ("enumerate") t nil)
    ("equ"       "Insert an EQUATION environment template"
     "" md-math-environment ("equation") t nil)
    ("eqn"       "Insert an EQUATION environment template"
     "" md-math-environment ("eqnarray") t nil)
    ("ali"       "Insert an ALIGN environment template"
     "" md-math-environment ("align") t nil)
    ("ali*"      "Insert an ALIGN* environment template"
     "" md-math-environment ("align*") t nil)
    ("alit"      "Insert an ALIGNAT environment template"
     "" md-math-environment ("alignat") t nil)
    ("alit*"     "Insert an ALIGNAT* environment template"
     "" md-math-environment ("alignat*") t nil)
    ("xal"       "Insert a XALIGNAT environment template"
     "" md-math-environment ("xalignat") t nil)
    ("xal*"      "Insert a XALIGNAT* environment template"
     "" md-math-environment ("xalignat*") t nil)
    ("xxa"       "Insert a XXALIGNAT environment template"
     "" md-math-environment ("xxalignat") t nil)
    ("xxa*"      "Insert a XXALIGNAT environment template"
     "" md-math-environment ("xxalignat") t nil)
    ("mul"       "Insert a MULTINE environment template"
     "" md-math-environment ("multline") t nil)
    ("mul*"      "Insert a MULTINE* environment template"
     "" md-math-environment ("multline*") t nil)
    ("gat"       "Insert a GATHER environment template"
     "" md-math-environment ("gather") t nil)
    ("gat*"      "Insert a GATHER* environment template"
     "" md-math-environment ("gather*") t nil)
    ("spl"       "Insert SPLIT environment template"
     "" md-math-environment ("split") t nil)
    ("fla"       "Insert a FLALIGN environment template"
     "" md-math-environment ("flalign") t nil)
    ("fla*"      "Insert a FLALIGN* environment template"
     "" md-math-environment ("flalign*") t nil)
    ("fg"        "Insert a FIGURE environment template"
     "" md-math-environment ("figure") t nil)

    ("sn"        "Insert a \\section{} statement"
     "\\section{?}" md-math-position-cursor nil t nil)
    ("ss"        "Insert a \\subsection{} statement"
     "\\subsection{?}" md-math-position-cursor nil t nil)
    ("sss"       "Insert a \\subsubsection{} statement"
     "\\subsubsection{?}" md-math-position-cursor nil t nil)
    ("pf"        "Insert a \\paragraph{} statement"
     "\\paragraph{?}" md-math-position-cursor nil t nil)
    ("sp"        "Insert a \\subparagraph{} statement"
     "\\subparagraph{?}" md-math-position-cursor nil t nil)
    ("ssp"       "Insert a \\subsubparagraph{} statement"
     "\\subsubparagraph{?}" md-math-position-cursor nil t nil)

    ("cl"        "Insert \\centerline"
     "\\centerline{?}" md-math-position-cursor nil t nil)
    ("inc"        "Insert \\includegraphics with file name"
     "\\includegraphics[]{?}" (lambda ()
                                (md-math-position-cursor)
                                (call-interactively 'md-math-insert-filename)
                                (forward-char 1))
     nil t nil)
    ("lr("       "Insert a \\left( \\right) pair"
     "(" md-math-lr-pair nil nil t)
    ("lr["        "Insert a \\left[ \\right] pair"
     "[" md-math-lr-pair nil nil t)
    ("lr{"        "Insert a \\left{ \\right} pair"
     "{" md-math-lr-pair nil nil t)
    ("lr<"        "Insert a \\left\\langle \\right\\rangle pair"
     "<" md-math-lr-pair nil nil t)
    ("lr|"        "Insert a \\left| \\right| pair"
     "|" md-math-lr-pair nil nil t)
    ("caseeq"     "Insert a = { construct"
     "\\left\\{ \n\\begin{array}{l@{\\quad:\\quad}l}\n? & \\\\\n & \n\\end{array}\\right." md-math-position-cursor nil nil t)
    ("fr"         "Insert \\frac{}{}"
     "\\frac{?}{}"           md-math-position-cursor nil nil t)
    ("sq"         "Insert \\sqrt{}"
     "\\sqrt{?}"             md-math-position-cursor nil nil t)
    ("intl"       "Insert \\int\\limits_{}^{}"
     "\\int\\limits_{?}^{}"  md-math-position-cursor nil nil t)
    ("suml"       "Insert \\sum\\limits_{}^{}"
     "\\sum\\limits_{?}^{}"  md-math-position-cursor nil nil t)
    ("nonum"      "Insert \\nonumber\\\\"
     "\\nonumber\\\\\n"      nil nil nil t)
    ("fn"         "Make a footnote"
     "\\footnote{?}" md-math-position-cursor nil t nil)
    ("qq"         "Insert \\quad"
     "\\quad"        nil nil t t)
    ("qqq"        "Insert \\qquad"
     "\\qquad"       nil nil t t)
    )
  "Default for md-math-command-alist.")

(defconst md-math-math-modify-alist-default
  '(
    ( ?\.   "\\dot"               nil        t   t   nil )
    ( ?\:   "\\ddot"              nil        t   t   nil )
    ( ?\~   "\\tilde"             nil        t   t   nil )
    ( ?N    "\\widetilde"         nil        t   t   nil )
    ( ?^    "\\hat"               nil        t   t   nil )
    ( ?H    "\\widehat"           nil        t   t   nil )
    ( ?\-   "\\bar"               nil        t   t   nil )
    ( ?T    "\\overline"          nil        t   nil nil )
    ( ?\_   "\\underline"         nil        t   nil nil )
    ( ?\{   "\\overbrace"         nil        t   nil nil )
    ( ?\}   "\\underbrace"        nil        t   nil nil )
    ( ?\>   "\\vec"               nil        t   t   nil )
    ( ?/    "\\grave"             nil        t   t   nil )
    ( ?\\   "\\acute"             nil        t   t   nil )
    ( ?v    "\\check"             nil        t   t   nil )
    ( ?u    "\\breve"             nil        t   t   nil )
    ( ?m    "\\mbox"              nil        t   nil nil )
    ( ?c    "\\mathcal"           nil        t   nil nil )
    ( ?r    "\\mathrm"            "\\textrm" t   nil nil )
    ( ?i    "\\mathit"            "\\textit" t   nil nil )
    ( ?l    nil                   "\\textsl" t   nil nil )
    ( ?b    "\\mathbf"            "\\textbf" t   nil nil )
    ( ?e    "\\mathem"            "\\emph"   t   nil nil )
    ( ?y    "\\mathtt"            "\\texttt" t   nil nil )
    ( ?f    "\\mathsf"            "\\textsf" t   nil nil )
    ( ?0    "\\textstyle"         nil        nil nil nil )
    ( ?1    "\\displaystyle"      nil        nil nil nil )
    ( ?2    "\\scriptstyle"       nil        nil nil nil )
    ( ?3    "\\scriptscriptstyle" nil        nil nil nil )
    )
  "Default for md-math-math-modify-alist.")

(defconst md-math-math-symbol-alist-default
  '(
    ( ?a  ("\\alpha"          ))
    ( ?A  ("\\forall"         "\\aleph"))
    ( ?b  ("\\beta"           ))
    ( ?B  (""                 ))
    ( ?c  (""                 ""                "\\cos"))
    ( ?C  (""                 ""                "\\arccos"))
    ( ?d  ("\\delta"          "\\partial"))
    ( ?D  ("\\Delta"          "\\nabla"))
    ( ?e  ("\\epsilon"        "\\varepsilon"    "\\exp"))
    ( ?E  ("\\exists"         ""                "\\ln"))
    ( ?f  ("\\phi"            "\\varphi"))
    ( ?F  (""                 ))
    ( ?g  ("\\gamma"          ""                "\\lg"))
    ( ?G  ("\\Gamma"          ""                "10^{?}"))
    ( ?h  ("\\eta"            "\\hbar"))
    ( ?H  (""                 ))
    ( ?i  ("\\in"             "\\imath"))
    ( ?I  (""                 "\\Im"))
    ( ?j  (""                 "\\jmath"))
    ( ?J  (""                 ))
    ( ?k  ("\\kappa"          ))
    ( ?K  (""                 ))
    ( ?l  ("\\lambda"         "\\ell"           "\\log"))
    ( ?L  ("\\Lambda"         ))
    ( ?m  ("\\mu"             ))
    ( ?M  (""                 ))
    ( ?n  ("\\nu"             ""                "\\ln"))
    ( ?N  ("\\nabla"          ""                "\\exp"))
    ( ?o  ("\\omega"          ))
    ( ?O  ("\\Omega"          "\\mho"))
    ( ?p  ("\\pi"             "\\varpi"))
    ( ?P  ("\\Pi"             ))
    ( ?q  ("\\theta"          "\\vartheta"))
    ( ?Q  ("\\Theta"          ))
    ( ?r  ("\\rho"            "\\varrho"))
    ( ?R  (""                 "\\Re"))
    ( ?s  ("\\sigma"          "\\varsigma"      "\\sin"))
    ( ?S  ("\\Sigma"          ""                "\\arcsin"))
    ( ?t  ("\\tau"            ""                "\\tan"))
    ( ?T  (""                 ""                "\\arctan"))
    ( ?u  ("\\upsilon"        ))
    ( ?U  ("\\Upsilon"        ))
    ( ?v  ("\\vee"            ))
    ( ?V  ("\\Phi"            ))
    ( ?w  ("\\xi"             ))
    ( ?W  ("\\Xi"             ))
    ( ?x  ("\\chi"            ))
    ( ?X  (""                 ))
    ( ?y  ("\\psi"            ))
    ( ?Y  ("\\Psi"            ))
    ( ?z  ("\\zeta"           ))
    ( ?Z  (""                 ))
    ( ?   (""                 ))
    ( ?0  ("\\emptyset"       ))
    ( ?1  (""                 ))
    ( ?2  (""                 ))
    ( ?3  (""                 ))
    ( ?4  (""                 ))
    ( ?5  (""                 ))
    ( ?6  (""                 ))
    ( ?7  (""                 ))
    ( ?8  ("\\infty"          ))
    ( ?9  (""                 ))
    ( ?!  ("\\neg"            ))
    ( ?@  (""                 ))
    ( ?#  (""                 ))
    ( ?$  (""                 ))
    ( ?%  (""                 ))
    ( ?^  ("\\uparrow"        ))
    ( ?&  ("\\wedge"          ))
    ( ?\? (""                 ))
    ( ?~  ("\\approx"         "\\simeq"))
    ( ?_  ("\\downarrow"      ))
    ( ?+  ("\\cup"            ))
    ( ?-  ("\\leftrightarrow" "\\longleftrightarrow" ))
    ( ?*  ("\\times"          ))
    ( ?/  ("\\not"            ))
    ( ?|  ("\\mapsto"         "\\longmapsto"))
    ( ?\\ ("\\setminus"       ))
    ( ?\" (""                 ))
    ( ?=  ("\\Leftrightarrow" "\\Longleftrightarrow"))
    ( ?\( ("\\langle"         ))
    ( ?\) ("\\rangle"         ))
    ( ?\[ ("\\Leftarrow"      "\\Longleftarrow"))
    ( ?\] ("\\Rightarrow"     "\\Longrightarrow"))
    ( ?{  ("\\subset"         ))
    ( ?}  ("\\supset"         ))
    ( ?<  ("\\leftarrow"      "\\longleftarrow"     "\\min"))
    ( ?>  ("\\rightarrow"     "\\longrightarrow"    "\\max"))
    ( ?`  (""                 ))
    ( ?'  ("\\prime"          ))
    ( ?.  ("\\cdot"           ))
    )
  "Default for md-math-math-symbol-alist."
  )

;;; ---------------------------------------------------------------------------

(defconst md-math-env-alist-default
  '(
    ;;------------------------------------
    ( "abstract"
      "\\begin{abstract}
?
\\end{abstract}"
      nil
      )
    ;;------------------------------------
    ( "appendix"
      "\\begin{appendix}
?
\\end{appendix}"
      nil
      )
    ;;------------------------------------
    ( "array"
      "\\begin{array}[tb]{?lcrp{width}*{num}{lcrp{}}|}
 & & & \\\\
\\end{array}"
      " & & &"
      )
    ;;------------------------------------
    ( "center"
      "\\begin{center}
? \\\\
\\end{center}"
      nil
      )
    ;;------------------------------------
    ( "deflist"
      "\\begin{deflist}{width-text}
\\item ?
\\end{deflist}"
      "\\item ?"
      )
    ;;------------------------------------
    ( "description"
      "\\begin{description}
\\item[?]
\\end{description}"
      "\\item[?] "
      )
    ;;------------------------------------
    ( "displaymath"
      "\\begin{displaymath}
?
\\end{displaymath}"
      nil
      )
    ;;------------------------------------
    ( "document"
      "\\begin{document}
?
\\end{document}"
      nil
      )
    ;;------------------------------------
    ( "enumerate"
      "\\begin{enumerate}
\\itemAUTOLABEL ?
\\end{enumerate}"
      "\\itemAUTOLABEL ?"
      )
    ;;------------------------------------
    ( "eqnarray"
      "\\begin{eqnarray}
AUTOLABEL
? &  & \\\\
\\end{eqnarray}"
      "\\\\AUTOLABEL
? &  & "
      )
    ;;------------------------------------
    ( "eqnarray*"
      "\\begin{eqnarray*}
? & & \\\\
\\end{eqnarray*}"
      "\\\\? & & "
      )
    ;;------------------------------------
    ( "equation"
      "\\begin{equation}
AUTOLABEL
?
\\end{equation}"
      nil
      )
    ;;------------------------------------
    ( "figure"
      "\\begin{figure}[htbp]
\\centerline{\\includegraphics[]{AUTOFILE}}
\\caption[]{AUTOLABEL ?}
\\end{figure}"
      nil
      )
    ;;------------------------------------
    ( "figure*"
      "\\begin{figure*}[htbp]
\\centerline{\includegraphics[]{AUTOFILE}
\\end{figure*}"
      nil
      )
    ;;------------------------------------
    ( "flushleft"
      "\\begin{flushleft}
? \\\\
\\end{flushleft}"
      "\\\\?"
      )
    ;;------------------------------------
    ( "flushright"
      "\\begin{flushright}
? \\\\
\\end{flushright}"
      "\\\\?"
      )
    ;;------------------------------------
    ( "fussypar"
      "\\begin{fussypar}
?
\\end{fussypar}"
      nil
      )
    ;;------------------------------------
    ( "itemize"
      "\\begin{itemize}
\\item ?
\\end{itemize}"
      "\\item ?"
      )
    ;;------------------------------------
    ( "letter"
      "\\begin{letter}
?
\\end{letter}"
      nil
      )
    ;;------------------------------------
    ( "list"
      "\\begin{list}{}{}
\\item ?
\\end{list}"
      "\\item ?"
      )
    ;;------------------------------------
    ( "math"
      "\\begin{math}
?
\\end{math}"
      nil
      )
    ;;------------------------------------
    ( "minipage"
      "\\begin{minipage}[bt]{?.cm}

\\end{minipage}"
      nil
      )
    ;;------------------------------------
    ( "picture"
      "\\begin{picture}(,)(,)
?
\\end{picture}"
      nil
      )
    ;;------------------------------------
    ( "quotation"
      "\\begin{quotation}
?
\\end{quotation}"
      nil
      )
    ;;------------------------------------
    ( "quote"
      "\\begin{quote}
?
\\end{quote}"
      nil
      )
    ;;------------------------------------
    ( "sloppypar"
      "\\begin{sloppypar}
?
\\end{sloppypar}"
      nil
      )
    ;;------------------------------------
    ( "tabbing"
      "\\begin{tabbing}
? \\=  \\=  \\=  \\= \\\\[0.5ex]
 \\>  \\>  \\>  \\> \\\\
\\end{tabbing}"
      "\\\\?"
      )
    ;;------------------------------------
    ( "table"
      "\\begin{table}[htbp]
\\caption[]{AUTOLABEL ?}
\\vspace{4mm}

\\end{table}"
      nil
      )
    ;;------------------------------------
    ( "tabular"
      "\\begin{tabular}[tb]{lcrp{width}*{num}{lcrp{}}|}
?
\\end{tabular}"
      nil
      )
    ;;------------------------------------
    ( "tabular*"
      "\\begin{tabular*}{width}[tb]{lcrp{width}*{num}{lcrp{}}|}
?
\\end{tabular*}"
      nil
      )

    ;;------------------------------------
    ( "theindex"
      "\\begin{theindex}
?
\\end{theindex}"
      nil
      )
    ;;------------------------------------
    ( "verbatim"
      "\\begin{verbatim}
?
\\end{verbatim}"
      nil
      )
    ;;------------------------------------
    ( "verbatim*"
      "\\begin{verbatim*}
?
\\end{verbatim*}"
      nil
      )
    ;;------------------------------------
    ( "verse"
      "\\begin{verse}
? \\\\
\\end{verse}"
      nil
      )
    ;;------------------------------------
    ;; AMS-LaTeX
    ( "align"
      "\\begin{align}
AUTOLABEL
?
\\end{align}"
      "\\\\AUTOLABEL
?")
    ;;------------------------------------
    ( "align*"
      "\\begin{align*}
?
\\end{align*}"
      "\\\\?")
    ;;------------------------------------
    ( "alignat"
      "\\begin{alignat}{?}
AUTOLABEL

\\end{alignat}"
      "\\\\AUTOLABEL
?")
    ;;------------------------------------
    ( "alignat*"
      "\\begin{alignat*}{?}

\\end{alignat*}"
      "\\\\?")
    ;;------------------------------------
    ( "xalignat"
      "\\begin{xalignat}{?}
AUTOLABEL

\\end{xalignat}"
      "\\\\AUTOLABEL
?")
    ;;------------------------------------
    ( "xalignat*"
      "\\begin{xalignat*}{?}

\\end{xalignat*}"
      "\\\\?")
    ;;------------------------------------
    ( "xxalignat"
      "\\begin{xxalignat}{?}

\\end{xxalignat}"
      "\\\\?")
    ;;------------------------------------
    ("multline"
     "\\begin{multline}
AUTOLABEL
?
\\end{multline}"
     "\\\\AUTOLABEL
?")
    ;;------------------------------------
    ("multline*"
     "\\begin{multline*}
?
\\end{multline*}"
     "?")
    ;;------------------------------------
    ( "flalign"
      "\\begin{flalign}
AUTOLABEL
?
\\end{flalign}"
      "\\\\AUTOLABEL
?"
      )
    ;;------------------------------------
    ( "flalign*"
      "\\begin{flalign*}
?
\\end{flalign*}"
      "\\\\?"
      )
    ;;------------------------------------
    ( "gather"
      "\\begin{gather}
AUTOLABEL
?
\\end{gather}"
      "\\\\AUTOLABEL
?")
    ;;------------------------------------
    ( "gather*"
      "\\begin{gather*}
?
\\end{gather*}"
      "\\\\?")
    ;;------------------------------------
    ( "split"
      "\\begin{split}
?
\\end{split}"
      "\\\\?")
    ;;------------------------------------
;;; SOME NON-STANDARD ENVIRONMENTS
    ;; figure environment for the epsf macro package
    ( "epsfigure"
      "\\begin{figure}[htbp]
\\centerline{\\epsfxsize=\\textwidth \\epsffile{?.eps}}
\\caption[]{AUTOLABEL}
\\end{figure}"
      nil
      )
    ;;------------------------------------
    ;; table environment for AASTeX
    ( "deluxetable"
      "\\begin{deluxetable}{?lcrp{width}*{num}{lcrp{}}}
\\tablecolumns{}
\\tablewidth{0pt}
\\tablecaption{AUTOLABEL }
\\tablehead{ \\colhead{} & \colhead{} & \\multicolumn{3}{c}{} }
\\startdata
 &  & \\nl
\\enddata
\\end{deluxetable}"
      nil
      )
    ;;------------------------------------
    ;; figure environments for A&A
    ( "aafigure"
      "\\begin{figure}
\\resizebox{\\hsize}{!}{\\includegraphics{?.eps}}
\\caption[]{AUTOLABEL}
\\end{figure}"
      nil
      )
    ;;------------------------------------
    ( "aafigure*"
      "\\begin{figure*}
\\resizebox{12cm}{!}{\\includegraphics{?.eps}}
\\caption[]{AUTOLABEL}
\\end{figure*}"
      nil
      )
    ))

;;; ---------------------------------------------------------------------------
;;;
;;; Functions to compile the tables, reset the mode etc.

(defun md-math-reset-mode ()
  "Reset Md-Math Mode.  Required to implement changes to some list variables.
This function will compile the information in `md-math-label-alist' and similar
variables.  It is called when Md-Math is first used, and after changes to
these variables via `md-math-add-to-label-alist'."
  (interactive)
  (md-math-compute-tables))


(defun md-math-compute-tables ()
  ;; Update tables not connected with ref and cite support

  (setq md-math-env-alist-comb
        (md-math-uniquify
         (append md-math-env-alist
                 md-math-env-alist-default)))
  ;; Merge the user specified lists with the defaults
  (setq md-math-command-alist-comb
        (md-math-uniquify
         (append md-math-command-alist
                 md-math-command-alist-default)))
  (setq md-math-math-symbol-alist-comb
        (md-math-uniquify
         (mapcar (lambda (x)
                   (if (listp (nth 1 x))
                       (cons (car x) (nth 1 x))
                     x))
                 (append md-math-math-symbol-alist
                         md-math-math-symbol-alist-default))))
  (setq md-math-math-modify-alist-comb
        (md-math-uniquify
         (append md-math-math-modify-alist
                 md-math-math-modify-alist-default)))

  ;; find out how many levels are needed for the math symbol stuff
  (let ((maxlev 0) (list md-math-math-symbol-alist-comb) entry)
    (while (setq entry (pop list))
      (setq maxlev (max maxlev (length (nth 1 list)))
            list (cdr list)))
    (setq md-math-math-symbol-no-of-levels (1- maxlev)))

  ;; The direct key bindings.
  (let (map dummy-map prefix modifiers symbol bindings)
    (cl-loop for level from 1 to md-math-math-symbol-no-of-levels do
             (setq dummy-map (make-sparse-keymap))
             (setq prefix (car (nth (1- level)
                                    md-math-math-symbol-direct-bindings)))
             (setq modifiers (cdr
                              (nth (1- level)
                                   md-math-math-symbol-direct-bindings)))
             (when (or prefix modifiers)
               (cond
                ((stringp prefix) (setq prefix (read-kbd-macro prefix)))
                ((integerp prefix) (setq prefix (vector prefix))))

               (if (null prefix)
                   (setq map md-math-mode-map)
                 (setq map (make-keymap))
                 (define-key md-math-mode-map prefix (setq map
                                                           (make-keymap))))
               (defun md-math-nop () (interactive))
               (define-key dummy-map
                 (vector (append modifiers (list ?a))) 'md-math-nop)
               (push (cons level (substitute-command-keys
                                  "\\<dummy-map>\\[md-math-nop]"))
                     bindings)
               (mapc (lambda (entry)
                       (setq symbol (nth level entry))
                       (when (and symbol (stringp symbol)
                                  (not (equal "" symbol)))
                         (define-key
                           map (vector (append modifiers (list (car entry))))
                           (list 'lambda '() '(interactive)
                                 (list 'md-math-insert-math symbol)))))
                     md-math-math-symbol-alist-comb)))
    (put 'md-math-math-symbol-alist-comb 'md-math-bindings bindings)))

(defun md-math-insert-math (string)
  (md-math-ensure-math)
  (insert string)
  (if (string-match "\\?" string)
      (md-math-position-cursor)))

;;; Keybindings --------------------------------------------------------------

(if md-math-takeover-dollar
    (define-key md-math-mode-map  "$"         'md-math-dollar))
(if md-math-takeover-parenthesis
    (progn
      (define-key md-math-mode-map  "("         'md-math-pbb)
      (define-key md-math-mode-map  "{"         'md-math-pbb)
      (define-key md-math-mode-map  "["         'md-math-pbb)
      (define-key md-math-mode-map  "|"         'md-math-pbb)
      (define-key md-math-mode-map  "<"         'md-math-pbb)))
(if md-math-takeover-subsuperscript
    (progn
      (define-key md-math-mode-map  "^"         'md-math-sub-superscript)
      (define-key md-math-mode-map  "_"         'md-math-sub-superscript)))

(define-key md-math-mode-map  "\t"        'md-math-tab)
(define-key md-math-mode-map  "\C-c?"     'md-math-command-help)
(define-key md-math-mode-map  "\C-c{"     'md-math-environment)
(define-key md-math-mode-map  [(control return)] 'md-math-item)

(define-key md-math-mode-map
  (md-math-get-kbd-vector md-math-math-symbol-prefix)
  'md-math-math-symbol)
(define-key md-math-mode-map
  (md-math-get-kbd-vector md-math-math-modify-prefix)
  'md-math-math-modify)

;;; Menus --------------------------------------------------------------------

;; Define a menu for the menu bar if Emacs is running under X

(require 'easymenu)

(easy-menu-define
  md-math-mode-menu md-math-mode-map
  "Menu used in Md-Math mode"
  '("CDLTX"
    ["\\begin{...} \\label"   md-math-environment t]
    ["\\item \\label"         md-math-item t]
    "----"
    ["Insert Math Symbol"     md-math-math-symbol t]
    ["Modify Math Symbol"     md-math-math-modify t]
    "----"
    ("Customize"
     ["Browse Md-Math group" md-math-customize t]
     "---"
     ["Build Full Customize Menu" md-math-create-customize-menu
      (fboundp 'customize-menu-create)])
    "----"
    ["Show documentation"      md-math-show-commentary t]
    ["Help with KEYWORD Cmds" md-math-command-help t]
    ["Reset Md-Math Mode"       md-math-reset-mode t]))

                                        ;(eval-after-load "cus-edit"
                                        ;  '(and (fboundp 'customize-menu-create) (md-math-create-customize-menu)))

;;; Run Hook ------------------------------------------------------------------

(run-hooks 'md-math-load-hook)

;;; That's it! ----------------------------------------------------------------

                                        ; Make sure tabels are compiled
(md-math-compute-tables)



(provide 'md-math-cd)
;;; md-math-cd.el ends here

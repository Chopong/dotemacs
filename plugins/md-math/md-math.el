;;; md-math.el --- -*- lexical-binding: t -*-
;;; Info
;; Author: Wei Shaopeng
;; Email : Chopong@qq.com
;; Date  : 2021-08-02,一,15:54:32
;;; Commentary:

;;; Code:


;; ------------------------------------------------------------------------------------------


(defcustom md-math-2-fontify-script t
  "If non-nil, fontify subscript and superscript strings.

By default, super/subscripts are raised/lowered if this variable
is non-nil.  This fontification only affects one level of
scripts, e.g., in x^{y^z}, the y and the z have the same size and
are equally raised over x.

If this variable is set to the symbol `multi-level', then y is
raised above x, and z is raised above y.  With many script
levels, the text might become too small to be readable, thus
there is the option `font-latex-fontify-script-max-level'.  (The
factors for text shrinking are defined in the faces
`font-latex-superscript-face' and `font-latex-subscript-face' and
the raise/lower factor in `font-latex-script-display'.)

If this variable is set to the symbol `invisible', then the
effect is essentially like `multi-level' but additionally the
script operators ^ and _ are not displayed."
  :type '(choice (boolean :tag "Enabled")
                 (const :tag "Multiple levels" multi-level)
                 (const :tag "Hide ^ and _" invisible))
  :group 'md-math)
(put 'md-math-2-fontify-script 'safe-local-variable
     (lambda (val)
       (or (booleanp val)
           (memq val '(multi-level invisible)))))

(defcustom md-math-2-fontify-script-max-level 3
  "Maximum scriptification level for which script faces are applied."
  :group 'md-math
  :type 'integer)

(defcustom md-math-2-script-display '((raise -0.5) . (raise 0.5))
  "Display specification for subscript and superscript content.
  The car is used for subscript, the cdr is used for superscripts."
  :group 'md-math
  :type '(cons (choice (sexp :tag "Subscript form")
                       (const :tag "No lowering" nil))
               (choice (sexp :tag "Superscript form")
                       (const :tag "No raising" nil))))

(defun md-math-2-faces-present-p (faces &optional pos)
  "Return t if FACES are present at position POS.
  FACES may be a single face or a list of faces.
  If POS is omitted, the current position of point is used."
  (let* ((faces (if (listp faces) faces (list faces)))
         (pos (or pos (point)))
         (prop (get-text-property pos 'face))
         (prop-list (if (listp prop) prop (list prop))))
    (catch 'member
      (dolist (item prop-list)
        (when (memq item faces)
          (throw 'member t))))))

(defun md-math-2-find-closing-brace (&optional depth limit)
  "Return the position of the closing brace in a TeX group.
  The function assumes that point is inside the group, i.e. after
  an opening brace.  With optional DEPTH>=1, find that outer level.
  If LIMIT is non-nil, do not search further down than this
  position in the buffer."
  (md-math-2-find-balanced-brace 1 depth limit))

(defun md-math-2-find-balanced-brace (&optional count depth limit)
  "Return the position of a balanced brace in a TeX group.
  The function scans forward COUNT parenthetical groupings.
  Default is 1.  If COUNT is negative, it searches backwards.  With
  optional DEPTH>=1, find that outer level.  If LIMIT is non-nil,
  do not search further than this position in the buffer."
  (let ((count (if count
                   (if (= count 0) (error "COUNT has to be <> 0") count)
                 1))
        (depth (if depth
                   (if (< depth 1) (error "DEPTH has to be > 0") depth)
                 1)))
    (save-restriction
      (when limit
        (if (> count 0)
            (narrow-to-region (point-min) limit)
          (narrow-to-region limit (point-max))))
      (with-syntax-table (md-math-2-search-syntax-table ?\{ ?\})
        (condition-case nil
            (scan-lists (point) count depth)
          (error nil))))))

(defun md-math-2-search-syntax-table (&rest args)
  "Return a syntax table for searching purposes.
  ARGS may be a list of characters.  For each of them the
  respective predefined syntax is set.  Currently the parenthetical
  characters ?{, ?}, ?[, ?], ?\(, ?\), ?<, and ?> are supported.
  The syntax of each of these characters not specified will be
  reset to \" \"."
  (let ((char-syntax-alist '((?\{ . "(}") (?\} . "){")
                             (?\[ . "(]") (?\] . ")[")
                             (?\( . "()") (?\) . ")(")
                             (?\< . "(>") (?\> . ")<"))))
    ;; Clean entries possibly set before.
    (modify-syntax-entry ?\\ " " md-math-2-search-syntax-table)
    (modify-syntax-entry ?@ " " md-math-2-search-syntax-table)
    (modify-syntax-entry ?\% " " md-math-2-search-syntax-table)
    ;; Preset mode-dependent syntax entries.  (Mode-independent entries
    ;; are set when the variable `TeX-search-syntax-table' is created.)
    (modify-syntax-entry (string-to-char md-math-esc) "\\" md-math-2-search-syntax-table)
    (unless (eq major-mode 'texinfo-mode)
      (modify-syntax-entry ?\% "<" md-math-2-search-syntax-table))
    ;; Clean up the entries which can be specified as arguments.
    (dolist (elt char-syntax-alist)
      (modify-syntax-entry (car elt) " " md-math-2-search-syntax-table))
    ;; Now set what we got.
    (dolist (elt args)
      (unless (assoc elt char-syntax-alist) (error "Char not supported"))
      (modify-syntax-entry elt (cdr (assoc elt char-syntax-alist))
                           md-math-2-search-syntax-table))
    ;; Return the syntax table.
    md-math-2-search-syntax-table))


(defun md-math-2-match-script (limit)
  "Match subscript and superscript patterns up to LIMIT."
  (when (and md-math-2-fontify-script
             (re-search-forward "[_^] *\\([^\n\\{}]\\|\
\\\\\\([a-zA-Z@]+\\|[^ \t\n]\\)\\|\\({\\)\\)" limit t))
    (if (and (not (memq md-math-2-fontify-script '(multi-level invisible)))
             (md-math-2-faces-present-p '(md-math-subscript
                                          md-math-superscript)))
        ;; Apply subscript and superscript highlighting only once (in case
        ;; font-latex-fontify-script is not 'multi-level) in order to prevent
        ;; the font size becoming too small.  We set an empty match to do that.
        (let ((point (point)))
          (store-match-data (list point point point point)))
      (when (match-end 3)
        (let ((beg (match-beginning 3))
              (end (md-math-2-find-closing-brace
                    ;; Don't match groups spanning more than one line
                    ;; in order to avoid visually wrong indentation in
                    ;; subsequent lines.
                    nil (line-end-position))))
          (store-match-data (if end
                                (list (match-beginning 0) end beg end)
                              (list beg beg beg beg))))))
    t))

(defun md-math-2-match-script-chars (limit)
  "Match subscript and superscript chars up to LIMIT."
  (catch 'found
    (while (re-search-forward "[^_^]\\([_^]\\)" limit t)
      (let ((pos (match-beginning 1)))
        (when (and (md-math-2-faces-present-p 'md-math-math pos)
                   (not (md-math-2-faces-present-p '(font-lock-constant-face
                                                     font-lock-builtin-face
                                                     font-lock-comment-face
                                                     md-math-verbatim) pos))
                   ;; Check for backslash quoting
                   (not (let ((odd nil)
                              (pos pos))
                          (while (eq (char-before pos) ?\\)
                            (setq pos (1- pos) odd (not odd)))
                          odd)))
          (throw 'found t))))))

(defun md-math-2-get-script-props (pos script-type)
  (let* ((old-raise (or (plist-get (get-text-property pos 'display) 'raise) 0.0))
         (new-level (1+ (or (get-text-property pos 'script-level) 0)))
         (disp-props (copy-sequence (cl-case script-type
                                      (:super (cdr md-math-2-script-display))
                                      (:sub   (car md-math-2-script-display)))))
         (new-disp-props (let ((raise (plist-get disp-props 'raise))
                               (nl new-level))
                           (if raise
                               ;; This polynom approximates that the factor
                               ;; which is multiplied with raise is 1 for nl=1,
                               ;; 0.8 for nl=2, 0.64 for nl=3, etc. (so always
                               ;; about 80% of the previous value).
                               (plist-put disp-props 'raise
                                          (+ old-raise
                                             (* raise
                                                (+ 1.1965254857142873
                                                   (* nl -0.21841226666666758)
                                                   (* nl nl 0.012018514285714385)))))
                             disp-props))))
    `(face ,(if (<= new-level md-math-2-fontify-script-max-level)
                (cl-case script-type
                  (:super 'md-math-superscript)
                  (:sub   'md-math-subscript))
              nil)
           script-level ,new-level
           display ,new-disp-props)))

;; Copy and adaption of `tex-font-lock-suscript' from tex-mode.el in
;; GNU Emacs on 2004-07-07.
(defun md-math-2-script (pos)
  "Return face and display spec for subscript and superscript content."
  (when (and (md-math-2-faces-present-p 'md-math-math pos)
             (not (md-math-2-faces-present-p '(font-lock-constant-face
                                               font-lock-builtin-face
                                               font-lock-comment-face
                                               md-math-verbatim) pos))
             ;; Check for backslash quoting
             (not (let ((odd nil)
                        (pos pos))
                    (while (eq (char-before pos) ?\\)
                      (setq pos (1- pos) odd (not odd)))
                    odd)))
    (if (eq (char-after pos) ?_)
        (md-math-2-get-script-props pos :sub)
      (md-math-2-get-script-props pos :super))))

(defun md-math-2-script-char (_pos)
  "Return face and display spec for subscript and superscript character at POS."
  `(face md-math-script-char
         ,@(when (eq md-math-2-fontify-script 'invisible)
             '(invisible t))))

;;; ------------------------------------------------------------------------------------------

(defvar md-math-handle-escaped-parens t)



(defvar md-math-superscript 'md-math-superscript)
;; (defface md-math-superscript
;;   '((t :height md-math-suscript-height)) ;; :raise 0.2
;;   "Face used for superscripts."
;;   :group 'md-math)

(defvar md-math-subscript   'md-math-subscript)
;; (defface md-math-subscript
;;   '((t :height md-math-suscript-height)) ;; :raise -0.2
;;   "Face used for subscripts."
;;   :group 'md-math)


(defface md-math-superscript
  '((t (:height 0.85)))
  "Face used for superscripts."
  :group 'font-latex-highlighting-faces)

(defface md-math-subscript
  '((t (:height 0.85)))
  "Face used for subscripts."
  :group 'font-latex-highlighting-faces)

(defvar md-math-math 'md-math-math)
;; (defface md-math-math
;;   '((t :inherit font-lock-string-face))
;;   "Face used to highlight Md-Math math expressions."
;;   :group 'md-math)
(defface md-math-math
  (let ((font '(:inherit underline)))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight math."
  :group 'font-latex-highlighting-faces)


(defvar md-math-verbatim 'md-math-verbatim)
;; (defface md-math-verbatim
;;   '((t :inherit fixed-pitch-serif))
;;   "Face used to highlight Md-Math verbatim environments."
;;   :group 'md-math)
(defface md-math-verbatim
  (let ((font '(:inherit fixed-pitch)))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown" ,@font))
      (((class color) (background dark))
       (:foreground "burlywood" ,@font))
      (t (,@font))))
  "Face used to highlight TeX verbatim environments."
  :group 'font-latex-highlighting-faces)


(defvar md-math-script-char 'md-math-script-char)
(defface md-math-script-char
  (let ((font '(:inherit underline)))
    `((((class grayscale) (background light))
       (:foreground "DarkGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "gray" ,@font))
      (((class color) (background light))
       (:foreground "salmon"))
      (((class color) (background dark))
       (:foreground "DarkRed"))
      (t (,@font))))
  "Face used for the script chars ^ and _."
  :group 'md-math)

(defun md-math-2-syntactic-face-function (state)
  (if (nth 3 state)
      'md-math-verbatim
    'font-lock-comment-face))

(defun md-math-font-lock-syntactic-face-function (state)
  (let ((char (nth 3 state)))
    (cond
     ((not char)
      (if (eq 2 (nth 7 state))
          'md-math-verbatim
        font-lock-comment-face))
     ((eq char ?$) 'md-math-math)
     (t 'md-math-verbatim))))
(defvar md-math-prettify-symbols-alist
  '( ;; Lowercase Greek letters.
    ("\\alpha" . ?𝛼)
    ("\\beta" . ?𝛽)
    ("\\gamma" . ?𝛾)
    ("\\delta" . ?𝛿)
    ("\\epsilon" . ?𝜖)
    ("\\zeta" . ?𝜁)
    ("\\eta" . ?𝜂)
    ("\\theta" . ?𝜃)
    ("\\iota" . ?𝜄)
    ("\\kappa" . ?𝜅)
    ("\\lambda" . ?𝜆)
    ("\\mu" . ?𝜇)
    ("\\nu" . ?𝜈)
    ("\\xi" . ?𝜉)
    ;; There is no \omicron because it looks like a latin o.
    ("\\pi" . ?𝜋)
    ("\\rho" . ?𝜌)
    ("\\sigma" . ?𝜎)
    ("\\tau" . ?𝜏)
    ("\\upsilon" . ?𝜐)
    ("\\phi" . ?𝜙)
    ("\\chi" . ?𝜒)
    ("\\psi" . ?𝜓)
    ("\\omega" . ?𝜔)
    ;; Uppercase Greek letters.
    ("\\Gamma" . ?Γ)
    ("\\Delta" . ?Δ)
    ("\\Lambda" . ?Λ)
    ("\\Phi" . ?Φ)
    ("\\Pi" . ?Π)
    ("\\Psi" . ?Ψ)
    ("\\Sigma" . ?Σ)
    ("\\Theta" . ?Θ)
    ("\\Upsilon" . ?Υ)
    ("\\Xi" . ?Ξ)
    ("\\Omega" . ?Ω)

    ;; Other math symbols (taken from leim/quail/latin-ltx.el).
    ("\\Box" . ?◻)
    ("\\Bumpeq" . ?≎)
    ("\\Cap" . ?⋒)
    ("\\Cup" . ?⋓)
    ("\\Diamond" . ?◊)
    ("\\Downarrow" . ?⇓)
    ("\\H{o}" . ?ő)
    ("\\Im" . ?ℑ)
    ("\\Join" . ?⋈)
    ("\\Leftarrow" . ?⇐)
    ("\\Leftrightarrow" . ?⇔)
    ("\\Ll" . ?⋘)
    ("\\Lleftarrow" . ?⇚)
    ("\\Longleftarrow" . ?⟸)
    ("\\Longleftrightarrow" . ?⟺)
    ("\\Longrightarrow" . ?⟹)
    ("\\Lsh" . ?↰)
    ("\\Re" . ?ℜ)
    ("\\Rightarrow" . ?⇒)
    ("\\Rrightarrow" . ?⇛)
    ("\\Rsh" . ?↱)
    ("\\Subset" . ?⋐)
    ("\\Supset" . ?⋑)
    ("\\Uparrow" . ?⇑)
    ("\\Updownarrow" . ?⇕)
    ("\\Vdash" . ?⊩)
    ("\\Vert" . ?‖)
    ("\\Vvdash" . ?⊪)
    ("\\aleph" . ?ℵ)
    ("\\amalg" . ?⨿)
    ("\\angle" . ?∠)
    ("\\approx" . ?≈)
    ("\\approxeq" . ?≊)
    ("\\ast" . ?∗)
    ("\\asymp" . ?≍)
    ("\\backcong" . ?≌)
    ("\\backepsilon" . ?∍)
    ("\\backprime" . ?‵)
    ("\\backsim" . ?∽)
    ("\\backsimeq" . ?⋍)
    ("\\backslash" . ?\\)
    ("\\barwedge" . ?⊼)
    ("\\because" . ?∵)
    ("\\beth" . ?ℶ)
    ("\\between" . ?≬)
    ("\\bigcap" . ?⋂)
    ("\\bigcirc" . ?◯)
    ("\\bigcup" . ?⋃)
    ("\\bigstar" . ?★)
    ("\\bigtriangledown" . ?▽)
    ("\\bigtriangleup" . ?△)
    ("\\bigvee" . ?⋁)
    ("\\bigwedge" . ?⋀)
    ("\\blacklozenge" . ?⧫)
    ("\\blacksquare" . ?◼)
    ("\\blacktriangle" . ?▴)
    ("\\blacktriangledown" . ?▾)
    ("\\blacktriangleleft" . ?◂)
    ("\\blacktriangleright" . ?▸)
    ("\\bot" . ?⊥)
    ("\\bowtie" . ?⋈)
    ("\\boxminus" . ?⊟)
    ("\\boxplus" . ?⊞)
    ("\\boxtimes" . ?⊠)
    ("\\bullet" . ?∙)
    ("\\bumpeq" . ?≏)
    ("\\cap" . ?∩)
    ("\\cdots" . ?⋯)
    ("\\centerdot" . ?·)
    ("\\checkmark" . ?✓)
    ("\\chi" . ?𝜒)
    ("\\cdot" . ?⋅)
    ("\\cdots" . ?⋯)
    ("\\circ" . ?∘)
    ("\\circeq" . ?≗)
    ("\\circlearrowleft" . ?↺)
    ("\\circlearrowright" . ?↻)
    ("\\circledR" . ?®)
    ("\\circledS" . ?Ⓢ)
    ("\\circledast" . ?⊛)
    ("\\circledcirc" . ?⊚)
    ("\\circleddash" . ?⊝)
    ("\\clubsuit" . ?♣)
    ("\\coloneq" . ?≔)
    ("\\complement" . ?∁)
    ("\\cong" . ?≅)
    ("\\coprod" . ?∐)
    ("\\cup" . ?∪)
    ("\\curlyeqprec" . ?⋞)
    ("\\curlyeqsucc" . ?⋟)
    ("\\curlypreceq" . ?≼)
    ("\\curlyvee" . ?⋎)
    ("\\curlywedge" . ?⋏)
    ("\\curvearrowleft" . ?↶)
    ("\\curvearrowright" . ?↷)
    ("\\dag" . ?†)
    ("\\dagger" . ?†)
    ("\\daleth" . ?ℸ)
    ("\\dashv" . ?⊣)
    ("\\ddag" . ?‡)
    ("\\ddagger" . ?‡)
    ("\\ddots" . ?⋱)
    ("\\diamond" . ?⋄)
    ("\\diamondsuit" . ?♢)
    ("\\divideontimes" . ?⋇)
    ("\\doteq" . ?≐)
    ("\\doteqdot" . ?≑)
    ("\\dotplus" . ?∔)
    ("\\dotsquare" . ?⊡)
    ("\\downarrow" . ?↓)
    ("\\downdownarrows" . ?⇊)
    ("\\downleftharpoon" . ?⇃)
    ("\\downrightharpoon" . ?⇂)
    ("\\ell" . ?ℓ)
    ("\\emptyset" . ?∅)
    ("\\eqcirc" . ?≖)
    ("\\eqcolon" . ?≕)
    ("\\eqslantgtr" . ?⪖)
    ("\\eqslantless" . ?⪖)
    ("\\equiv" . ?≡)
    ("\\exists" . ?∃)
    ("\\fallingdotseq" . ?≒)
    ("\\flat" . ?♭)
    ("\\forall" . ?∀)
    ("\\frown" . ?⌢)
    ("\\ge" . ?≥)
    ("\\geq" . ?≥)
    ("\\geqq" . ?≧)
    ("\\geqslant" . ?⩾)
    ("\\gets" . ?←)
    ("\\gg" . ?≫)
    ("\\ggg" . ?⋙)
    ("\\gimel" . ?ℷ)
    ("\\gnapprox" . ?⪊)
    ("\\gneq" . ?⪈)
    ("\\gneqq" . ?≩)
    ("\\gnsim" . ?⋧)
    ("\\gtrapprox" . ?⪆)
    ("\\gtrdot" . ?⋗)
    ("\\gtreqless" . ?⋛)
    ("\\gtreqqless" . ?⋛)
    ("\\gtrless" . ?≷)
    ("\\gtrsim" . ?≳)
    ("\\gvertneqq" . ?≩)
    ("\\hbar" . ?ℏ)
    ("\\heartsuit" . ?♡)
    ("\\hookleftarrow" . ?↩)
    ("\\hookrightarrow" . ?↪)
    ("\\iff" . ?⟺)
    ("\\imath" . ?ı)
    ("\\in" . ?∈)
    ("\\infty" . ?∞)
    ("\\int" . ?∫)
    ("\\intercal" . ?⊺)
    ("\\langle" . 10216)          ; 10216 Literal ?⟨ breaks indentation.
    ("\\lbrace" . ?{)
    ("\\lbrack" . ?\[)
    ("\\lceil" . ?⌈)
    ("\\ldots" . ?…)
    ("\\le" . ?≤)
    ("\\leadsto" . ?⇝)
    ("\\leftarrow" . ?←)
    ("\\leftarrowtail" . ?↢)
    ("\\leftharpoondown" . ?↽)
    ("\\leftharpoonup" . ?↼)
    ("\\leftleftarrows" . ?⇇)
    ;; ("\\leftparengtr" ?〈), see bug#12948.
    ("\\leftrightarrow" . ?↔)
    ("\\leftrightarrows" . ?⇆)
    ("\\leftrightharpoons" . ?⇋)
    ("\\leftrightsquigarrow" . ?↭)
    ("\\leftthreetimes" . ?⋋)
    ("\\leq" . ?≤)
    ("\\leqq" . ?≦)
    ("\\leqslant" . ?⩽)
    ("\\lessapprox" . ?⪅)
    ("\\lessdot" . ?⋖)
    ("\\lesseqgtr" . ?⋚)
    ("\\lesseqqgtr" . ?⪋)
    ("\\lessgtr" . ?≶)
    ("\\lesssim" . ?≲)
    ("\\lfloor" . ?⌊)
    ("\\lhd" . ?⊲)
    ("\\rhd" . ?⊳)
    ("\\ll" . ?≪)
    ("\\llcorner" . ?⌞)
    ("\\lnapprox" . ?⪉)
    ("\\lneq" . ?⪇)
    ("\\lneqq" . ?≨)
    ("\\lnsim" . ?⋦)
    ("\\longleftarrow" . ?⟵)
    ("\\longleftrightarrow" . ?⟷)
    ("\\longmapsto" . ?⟼)
    ("\\longrightarrow" . ?⟶)
    ("\\looparrowleft" . ?↫)
    ("\\looparrowright" . ?↬)
    ("\\lozenge" . ?◊)
    ("\\lq" . ?‘)
    ("\\lrcorner" . ?⌟)
    ("\\ltimes" . ?⋉)
    ("\\lvertneqq" . ?≨)
    ("\\maltese" . ?✠)
    ("\\mapsto" . ?↦)
    ("\\measuredangle" . ?∡)
    ("\\mho" . ?℧)
    ("\\mid" . ?∣)
    ("\\models" . ?⊧)
    ("\\mp" . ?∓)
    ("\\multimap" . ?⊸)
    ("\\nLeftarrow" . ?⇍)
    ("\\nLeftrightarrow" . ?⇎)
    ("\\nRightarrow" . ?⇏)
    ("\\nVDash" . ?⊯)
    ("\\nVdash" . ?⊮)
    ("\\nabla" . ?∇)
    ("\\napprox" . ?≉)
    ("\\natural" . ?♮)
    ("\\ncong" . ?≆)
    ("\\ne" . ?≠)
    ("\\nearrow" . ?↗)
    ("\\neg" . ?¬)
    ("\\neq" . ?≠)
    ("\\nequiv" . ?≢)
    ;;("\\newline" . ? )
    ("\\nexists" . ?∄)
    ("\\ngeq" . ?≱)
    ("\\ngeqq" . ?)
    ("\\ngeqslant" . ?)
    ("\\ngtr" . ?≯)
    ("\\ni" . ?∋)
    ("\\nleftarrow" . ?↚)
    ("\\nleftrightarrow" . ?↮)
    ("\\nleq" . ?≰)
    ("\\nleqq" . ?)
    ("\\nleqslant" . ?)
    ("\\nless" . ?≮)
    ("\\nmid" . ?∤)
    ;; ("\\not" ?̸)              ;FIXME: conflict with "NOT SIGN" ¬.
    ("\\notin" . ?∉)
    ("\\nparallel" . ?∦)
    ("\\nprec" . ?⊀)
    ("\\npreceq" . ?)
    ("\\nrightarrow" . ?↛)
    ("\\nshortmid" . ?∤)
    ("\\nshortparallel" . ?∦)
    ("\\nsim" . ?≁)
    ("\\nsimeq" . ?≄)
    ("\\nsubset" . ?⊄)
    ("\\nsubseteq" . ?⊈)
    ("\\nsubseteqq" . ?⊈)
    ("\\nsucc" . ?⊁)
    ("\\nsucceq" . ?)
    ("\\nsupset" . ?⊅)
    ("\\nsupseteq" . ?⊉)
    ("\\nsupseteqq" . ?)
    ("\\ntriangleleft" . ?⋪)
    ("\\ntrianglelefteq" . ?⋬)
    ("\\ntriangleright" . ?⋫)
    ("\\ntrianglerighteq" . ?⋭)
    ("\\nvDash" . ?⊭)
    ("\\nvdash" . ?⊬)
    ("\\nwarrow" . ?↖)
    ("\\odot" . ?⊙)
    ("\\oint" . ?∮)
    ("\\ominus" . ?⊖)
    ("\\oplus" . ?⊕)
    ("\\oslash" . ?⊘)
    ("\\otimes" . ?⊗)
    ;; ("\\par" . ? )
    ("\\parallel" . ?∥)
    ("\\partial" . ?∂)
    ("\\perp" . ?⊥)
    ("\\pitchfork" . ?⋔)
    ("\\prec" . ?≺)
    ("\\precapprox" . ?⪷)
    ("\\preceq" . ?⪯)
    ("\\precnapprox" . ?⋨)
    ("\\precnsim" . ?⪹)
    ("\\precsim" . ?≾)
    ("\\prime" . ?′)
    ("\\prod" . ?∏)
    ("\\propto" . ?∝)
    ("\\qed" . ?∎)
    ("\\qquad" . ?⧢)
    ("\\quad" . ?␣)
    ("\\rangle" . 10217)            ; Literal ?⟩ breaks indentation.
    ("\\rbrace" . ?})
    ("\\rbrack" . ?\])
    ("\\rceil" . ?⌉)
    ("\\rfloor" . ?⌋)
    ("\\rightarrow" . ?→)
    ("\\rightarrowtail" . ?↣)
    ("\\rightharpoondown" . ?⇁)
    ("\\rightharpoonup" . ?⇀)
    ("\\rightleftarrows" . ?⇄)
    ("\\rightleftharpoons" . ?⇌)
    ;; ("\\rightparengtr" ?⦔) ;; Was ?〉, see bug#12948.
    ("\\supset" . ?⊃)
    ("\\supseteq" . ?⊇)
    ("\\supseteqq" . ?⫆)
    ("\\supsetneq" . ?⊋)
    ("\\supsetneqq" . ?⫌)
    ("\\surd" . ?√)
    ("\\swarrow" . ?↙)
    ("\\therefore" . ?∴)
    ("\\thickapprox" . ?≈)
    ("\\thicksim" . ?∼)
    ("\\to" . ?→)
    ("\\top" . ?⊤)
    ("\\triangle" . ?△)
    ("\\triangledown" . ?▿)
    ("\\triangleleft" . ?◃)
    ("\\trianglelefteq" . ?⊴)
    ("\\triangleq" . ?≜)
    ("\\triangleright" . ?▹)
    ("\\trianglerighteq" . ?⊵)
    ("\\twoheadleftarrow" . ?↞)
    ("\\twoheadrightarrow" . ?↠)
    ("\\ulcorner" . ?⌜)
    ("\\uparrow" . ?↑)
    ("\\updownarrow" . ?↕)
    ("\\upleftharpoon" . ?↿)
    ("\\uplus" . ?⊎)
    ("\\uprightharpoon" . ?↾)
    ("\\upuparrows" . ?⇈)
    ("\\urcorner" . ?⌝)
    ("\\u{i}" . ?ĭ)
    ("\\vDash" . ?⊨)
    ("\\varepsilon" . ?𝜀)
    ("\\varphi" . ?𝜑)
    ("\\varprime" . ?′)
    ("\\varpropto" . ?∝)
    ("\\varrho" . ?𝜚)
    ("\\varsigma" . ?𝜍)
    ("\\vartriangleleft" . ?⊲)
    ("\\vartriangleright" . ?⊳)
    ("\\vdash" . ?⊢)
    ("\\vdots" . ?⋮)
    ("\\vee" . ?∨)
    ("\\veebar" . ?⊻)
    ("\\vert" . ?|)
    ("\\wedge" . ?∧)
    ("\\wp" . ?℘)
    ("\\wr" . ?≀)
    ("\\Bbb{N}" . ?ℕ)			; AMS commands for blackboard bold
    ("\\Bbb{P}" . ?ℙ)			; Also sometimes \mathbb.
    ("\\Bbb{Q}" . ?ℚ)
    ("\\Bbb{R}" . ?ℝ)
    ("\\Bbb{Z}" . ?ℤ)
    ("--" . ?–)
    ("---" . ?—)
    ("\\ordfeminine" . ?ª)
    ("\\ordmasculine" . ?º)
    ("\\lambdabar" . ?ƛ)
    ("\\celsius" . ?℃)
    ("\\textmu" . ?µ)
    ("\\textfractionsolidus" . ?⁄)
    ("\\textbigcircle" . ?⃝)
    ("\\textmusicalnote" . ?♪)
    ("\\textdied" . ?✝)
    ("\\textcolonmonetary" . ?₡)
    ("\\textwon" . ?₩)
    ("\\textnaira" . ?₦)
    ("\\textpeso" . ?₱)
    ("\\textlira" . ?₤)
    ("\\textrecipe" . ?℞)
    ("\\textinterrobang" . ?‽)
    ("\\textpertenthousand" . ?‱)
    ("\\textbaht" . ?฿)
    ("\\textnumero" . ?№)
    ("\\textdiscount" . ?⁒)
    ("\\textestimated" . ?℮)
    ("\\textopenbullet" . ?◦)
    ("\\textlquill" . 8261)		; Literal ?⁅ breaks indentation.
    ("\\textrquill" . 8262)             ; Literal ?⁆ breaks indentation.
    ("\\textcircledP" . ?℗)
    ("\\textreferencemark" . ?※))
  "A `prettify-symbols-alist' usable for (La)TeX modes.")

(defun md-math-prettify-symbols-compose-p (_start end _match)
  (or
   ;; If the matched symbol doesn't end in a word character, then we
   ;; simply allow composition.  The symbol is probably something like
   ;; \|, \(, etc.
   (not (eq ?w (char-syntax (char-before end))))
   ;; Else we look at what follows the match in order to decide.
   (let* ((after-char (char-after end))
          (after-syntax (char-syntax after-char)))
     (not (or
           ;; Don't compose \alpha@foo.
           (eq after-char ?@)
           ;; The \alpha in \alpha2 or \alpha-\beta may be composed but
           ;; of course \alphax may not.
           (and (eq after-syntax ?w)
                (not (memq after-char
                           '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?+ ?- ?' ?\"))))
           ;; Don't compose inside verbatim blocks.
           (eq 2 (nth 7 (syntax-ppss))))))))


;;; ------------------------------------------------------------------------------------------

(defvar font-md-math-multiline-boundary 5000
  "Size of region to search for the start or end of a multiline construct.")

(defvar font-md-math-updated-region-end nil
  ;; During font lock operation, matched range sometimes exceeds the
  ;; given end limit. So record the actual end in this variable to
  ;; notify the font lock machinery.
  ;; Match functions should do the following two if the end of the
  ;; actual match goes beyond the limit:
  ;; 1. If the value of this variable is smaller than limit, set this
  ;;    variable to that limit.
  ;; 2. When the end of the actual match exceeds this variable,
  ;;    - apply `font-lock-unfontify-region' between the value of this
  ;;      variable and the end of the actual match
  ;;    - update this variable to the end of the actual match
  ;; See implementation of `font-latex-match-math-env' for actual usage.
  "Record the end of fontification.")

(defun font-md-math-match-dollar-math (limit)
  "Match inline math $...$ or display math $$...$$ before LIMIT."
  (catch 'match
    (let (beg num)
      (while (font-md-math-find-dollar-math limit)
        ;; Found "$" which starts $...$ or $$...$$.
        (setq beg (point)
              ;; Go inside the math expression.
              num (skip-chars-forward "$" limit))
        ;; If those are three or more consecutive $, ignore them and
        ;; search again.
        (if (< num 3)
            (if ;; Let's find the same number of live dollar signs.
                (font-md-math-find-dollar-math
                 ;; Hope that limit+font-latex-multiline-boundary
                 ;; doesn't fall just inside single "$$".
                 (min (point-max) (+ limit font-md-math-multiline-boundary))
                 num)
                ;; Found.
                (progn
                  (forward-char num)
                  (let ((p (point)))
                    (if (< font-md-math-updated-region-end limit)
                        (setq font-md-math-updated-region-end limit))
                    (when (< font-md-math-updated-region-end p)
                      (font-lock-unfontify-region
                       font-md-math-updated-region-end p)
                      (setq font-md-math-updated-region-end p))
                    (set-match-data (list beg p)))
                  (throw 'match t))
              ;; Not found.
              ;; That $ or $$ is probably unclosed in the buffer.
              (throw 'match nil)))))))

(defvar md-math-esc "\\" "The TeX escape character.")
(make-variable-buffer-local 'md-math-esc)


(defun md-math-escaped-p (&optional pos)
  "Return t if the character at position POS is escaped.
If POS is omitted, examine the character at point.
A character is escaped if it is preceded by an odd number of
escape characters, such as \"\\\" in LaTeX."
  (save-excursion
    (when pos (goto-char pos))
    (not (zerop (mod (skip-chars-backward (regexp-quote md-math-esc)) 2)))))

(defun font-md-math-find-dollar-math (limit &optional num)
  "Find dollar sign(s) before LIMIT.
Set point just before the found $. Ignore escaped $ (\"\\$\").
Optional argument NUM, if non-nil, specifies the number of dollar
signs to follow the point and must be 1 or 2.
LIMIT must not exceed the end of buffer."
  (catch 'found
    (while (progn
             (skip-chars-forward "^$" limit)
             (< (point) limit))
      ;; Found "$".
      ;; If that "$" is not our target, skip over it and search
      ;; again.
      (cond
       ;; check 1: Are we in a verbatim construct or comment?
       ((let ((ppss (syntax-ppss)))
          (or (nth 3 ppss)
              ;; Ignore $ in comments...
              (and (nth 4 ppss)
                   ;; ... except if we're looking for the end of the
                   ;; inline math.  We need to consider this %$
                   ;; comments because they are the workaround for
                   ;; falsely triggered math mode due to valid,
                   ;; non-math occurrences of $.  (bug#48365)
                   (not num))))
        (skip-chars-forward "$" limit))
       ;; check 2: Else, is "$" escaped?
       ((md-math-escaped-p)
        (forward-char 1))
       ;; check 3: Else, is the number of the following "$" wrong?
       ;; This check cannot precede check 2 because "$1+2\$$" is
       ;; legal.
       ((and (eq num 2) (not (eq (char-after (1+ (point))) ?$)))
        ;; If double dollars ($$) are followed by $, skip over that $.
        ;; We need not care the case that single dollar ($) is
        ;; followed by $$ because expressions like "$1+1$$2+2$" and
        ;; "$1+2$$$3+3$$" are legal.
        (forward-char 1))
       (t
        ;; That "$" is live one.
        (throw 'found t))))))

(defun font-md-math-match-math-env (limit)
  "Match math pattern up to LIMIT.
Used for patterns like:
\\( F = ma \\)
\\=\\[ F = ma \\] but not \\\\=\\[len]"
  (catch 'match
    (while (re-search-forward "\\(\\\\(\\)\\|\\(\\\\\\[\\)" limit t)
      (unless (save-excursion
                (goto-char (match-beginning 0))
                ;; \\[ does not start a math environment
                (/= (mod (skip-chars-backward "\\\\") 2) 0))
        (let ((beg (match-beginning 0))
              (open-tag (if (match-beginning 1) "\\(" "\\["))
              (close-tag (if (match-beginning 1) "\\)" "\\]")))
          ;; Search for both opening and closing tags in order to be
          ;; able to avoid erroneously matching stuff like "\(foo \(bar\)".
          (if (and (re-search-forward (concat "[^\\]\\(?:\\\\\\\\\\)*\\("
                                              (regexp-quote open-tag) "\\|"
                                              (regexp-quote close-tag) "\\)")
                                      (+ limit font-md-math-multiline-boundary)
                                      'move)
                   (string= (match-string 1) close-tag))
              ;; Found closing tag.
              (let ((p (point)))
                (if (< font-md-math-updated-region-end limit)
                    ;; *-extend-region-functions have extended the
                    ;; limit already.
                    (setq font-md-math-updated-region-end limit))
                ;; If the closing tag is beyond the current end of
                ;; region, take care of it.
                (when (< font-md-math-updated-region-end p)
                  (font-lock-unfontify-region font-md-math-updated-region-end p)
                  (setq font-md-math-updated-region-end p))
                (store-match-data (list beg beg beg p)))
            ;; Did not find closing tag.
            (goto-char (+ beg 2))
            (store-match-data (list beg (point) (point) (point))))
          (throw 'match t))))))


;; (defface ne-keyword
;;   '((t :foreground "blue"
;;        ;; :background "aquamarine"
;;        :weight bold
;;        :underline t
;;        ))
;;   "No")

;; (defvar ne-keyword 'ne-keyword)

(defvar md-math-2-updated-region-end nil)

(defun md-math-2-fontify-region (beg end &optional verbose)
  "Fontify region from BEG to END.
Take care when the actually fonfified region was extended beyond END."
  (setq md-math-2-updated-region-end end)
  (let ((res (font-lock-default-fontify-region beg end verbose)))
    ;; COMPATIBILITY for older emacsen. Return value for jit-lock
    ;; is meaningful for only newer emacsen.
    (if (eq (car-safe res) 'jit-lock-bounds)
        `(jit-lock-bounds ,(cadr res) .
                          ,(max (cddr res) md-math-2-updated-region-end)))))

;; Copy and adaption of `tex-font-lock-unfontify-region' from
;; tex-mode.el in GNU Emacs on 2004-08-04.
;; (XEmacs passes a third argument to the function.)
(defun md-math-2-unfontify-region (beg end &rest _ignored)
  "Unfontify region from BEG to END."
  (font-lock-default-unfontify-region beg end)
  (remove-list-of-text-properties beg end '(script-level invisible))
  (while (< beg end)
    (let ((next (next-single-property-change beg 'display nil end))
          (prop (get-text-property beg 'display)))
      (if (and (eq (car-safe prop) 'raise)
               (null (cddr prop)))
          (put-text-property beg next 'display nil))
      (setq beg next))))


(setq mymath-seq "Sin\\|Cos\\|Sum")
(dolist (var md-math-prettify-symbols-alist)
  (setq mymath-seq (concat mymath-seq "\\|" (char-to-string (cdr var)))))
(setq mymath-highlights '(("Pi\\|Infinity" . font-lock-constant-face)
                          ("Hello World" . font-lock-keyword-face)
                          ("bold" . 'bold)
                          ))
;; (push (list mymath-seq . font-lock-function-name-face) mymath-highlights)

;; (dolist (var md-math-font-lock-keywords-3)
;;   (push var mymath-highlights))

(push (cons mymath-seq font-lock-constant-face) mymath-highlights)

;; (push '(md-math-font-lock-match-suscript
;;         (1 (md-math-font-lock-suscript (match-beginning 0)) append)) mymath-highlights)

(dolist (var markdown-mode-font-lock-keywords)
  (push var mymath-highlights))

;; (dolist (var markdown-mode-font-lock-keywords-math)
;;   (push var mymath-highlights))

(dolist (item
         '(
           ;; (md-math-2-match-dollar-math 0 'md-math-math keep)
           (md-math-2-match-script
            (1 (md-math-2-script (match-beginning 0)) append))
           (md-math-2-match-script-chars
            (1 (md-math-2-script-char (match-beginning 1)) append))))
  (add-to-list 'mymath-highlights item t))


;; (push mymath-highlights font-lock-defaults)
;; (dolist (var mymath-highlights)
;;   (push var font-lock-defaults)
;;   )


(defun md-math-init-func ()
  (setq-local font-lock-defaults
              '((mymath-highlights
                 ;; markdown-mode-font-lock-keywords
                 ;; markdown-mode-font-lock-keywords-math
                 ;; md-math-font-lock-keywords-1
                 ;; md-math-font-lock-keywords-2
                 ;; md-math-font-lock-keywords-3
                 )
                nil nil nil nil
                (font-lock-mark-block-function . mark-paragraph)
                (font-lock-syntactic-face-function . md-math-2-syntactic-face-function)
                (font-lock-default-unfontify-region . md-math-font-lock-unfontify-region)
                ))

  (setq-local prettify-symbols-alist md-math-prettify-symbols-alist)
  (add-function :override (local 'prettify-symbols-compose-predicate)
                #'md-math-prettify-symbols-compose-p)
  ;; (setq-local syntax-propertize-function
  ;;             (syntax-propertize-rules md-math-syntax-propertize-rules))
  )

(define-minor-mode md-math-mode "MMath"
  (md-math-init-func)
  )


(provide 'md-math)
;;; md-math.el ends here

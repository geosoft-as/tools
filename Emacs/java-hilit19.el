;; java-hilit19.el --- Java syntax coloring for Gnu Emacs based on hilit19
;;
;; Author: Carl Manning (caroma@ai.mit.edu)
;; Date:   1998.4.12
;; URL:    http://www.ai.mit.edu/people/caroma/tools/java-hilit19.el
;;
;; Java-Hilit19 uses Gnu Emacs hilit19 to color Java source code to
;; make the source code text easier to visually navigate and read
;; within Emacs.  In addition to coloring Java keywords, comments, and
;; strings, it also colors type names (almost always), names of class,
;; interface, etc. declarations, and names of field, constructor, &
;; method definitions preceded by an access keyword (public,
;; protected, private, etc.).  The colored names of declarations and
;; definitions are much easier to locate when visually scanning large
;; declarations, or definitions preceded by many modifier keywords and
;; long type names.  (Reliably coloring names of field, constructor,
;; and methods without access keywords requires more parsing
;; capability than the regular expressions used by Emacs/hilit19
;; provide.)
;;
;; Use: java-hilit19 colors text within Emacs buffers in java-mode.
;; hilit19 does not operate continuously, but only during large 
;; changes like reading in a new file or yanking text.  You can force
;; (re)coloring to occur with control-L (refresh).
;;
;; Installation:  Save this file and put the following your .emacs file
;(cond (window-system
;        (load "~/.java-hilit19.el") ; load this file
;        (setq hilit-mode-enable-list  '(not text-mode)
;              hilit-background-mode   'light            ;; 'light or 'dark
;              hilit-inhibit-hooks     nil
;              hilit-inhibit-rebinding nil)
;	(let ((num-colors 'many))            ;; 'many, or 'few for 16 color LCD
;	  ;; customize color mapping, as you wish.
;         ;; Coloring strategies you may wish to keep in mind:
;         ;;  o Reserve more noticeable color (e.g., reds) just for words 
;         ;;    that you must scan for from greater distances, e.g., 
;         ;;    method definition names, not words within methods.
;         ;;  o Use low contrast colors for close-distance highlighting:
;         ;;    Preserve horizontal readability of program text by coloring 
;         ;;    keywords, types, and literals with colors that are similar 
;         ;;    in intensity to the default text color, not high contrast 
;         ;;    colors.  Garish, high-contrast coloring distracts the eye 
;         ;;    into noticing the pattern formed by the colors, e.g., grouping
;         ;;    magenta words on adjacent lines, making them more difficult to
;         ;;    read horizontally.  You can also reduce this effect by reducing
;         ;;    the coloring, e.g., by leaving types the default color.
;	  ;; note: on NT italic doesn't work in emacs 19.34.6
;	  (cond ((and (eq num-colors 'many) (eq hilit-background-mode 'light))
;		 ;; for Black text on White, FloralWhite, etc. background
;		 (hilit-translate comment     'ForestGreen-italic
;				  doccomment  'DarkGreen-italic
;				  string      'DarkGoldenrod4
;				  literal     'DarkGoldenrod4
;				  declaration 'Firebrick
;				  keyword     'Blue3
;				  type        'Purple3
;				  default     'Black));; used for uncoloring
;		((and (eq num-colors 'many) (eq hilit-background-mode 'dark))
;		 ;; for White text on Black background
;		 (hilit-translate comment     'PaleGreen-italic
;				  doccomment  'Green2-italic
;				  string      'Khaki2
;				  literal     'Khaki2
;				  declaration 'LightSalmon1
;				  keyword     'LightBlue1
;				  type        'Thistle1
;				  default     'White));; used for uncoloring
;		((and (eq num-colors 'few) (eq hilit-background-mode 'light))
;		 ;; few colors (e.g., 16 color LCD),  Black text on White
;		 (hilit-translate comment     'ForestGreen-italic
;				  doccomment  'ForestGreen-italic
;				  string      'Goldenrod
;				  literal     'Goldenrod
;				  declaration 'Red
;				  keyword     'Navy
;				  type        'Brown
;				  default     'Black));; used for uncoloring
;		((and (eq num-colors 'few) (eq hilit-background-mode 'dark))
;		 ;; few colors (e.g., 16 color LCD), White text on Black
;		 (hilit-translate comment     'Green-italic
;				  doccomment  'DarkGreen-italic
;				  string      'Yellow
;				  literal     'Yellow
;				  declaration 'Magenta
;				  keyword     'Cyan
;				  type        'Grey
;				  default     'White));; used for uncoloring
;		((eq hilit-background-mode 'mono)
;		 ;; hilit bug: using default doesn't unitalicize, etc.
;		 (hilit-translate comment     'default-italic
;				  doccomment  'default-bold-italic
;				  string      'default-bold
;				  literal     'default-bold
;				  declaration 'default-bold-italic
;				  keyword     'default-bold
;				  type        'default-italic))
;		)
;	)))

;; Below fixes the syntax table in Java mode so _ and $ are treated as letters;
;; this will keep emacs from coloring the keyword parts of identifiers like
;; "for_each" or "on_return"
(require 'cc-mode) ;; java-mode is embedded in cc-mode
(cond ((boundp 'java-mode-syntax-table)
       (modify-syntax-entry ?_ "w" java-mode-syntax-table)
       (modify-syntax-entry ?$ "w" java-mode-syntax-table))
      (else (print "Warning: Java-mode-syntax-table not loaded")))
       
(require 'hilit19)
(hilit-set-mode-patterns
 'java-mode
 `(;; documentation comment -- before comment to override it
   ("/\\*\\*" "\\*/" doccomment)  
   ;; comments -- before rest to override
   ("/\\*" "\\*/" comment)
   ("//.*$" nil comment)
   ("^/.*$" nil comment)
   ;; strings -- put before others to override them
   (hilit-string-find ?' string)
   ;; key words -- must be before others to recolor nested keywords.
   ;; (omit reserved words const and goto as they aren't really in Java.)
   ("\\<\\(return\\|for\\|while\\|do\\|if\\|else\\|case\\|default\\|switch\\|break\\|continue\\|new\\|throw\\|catch\\|try\\|finally\\|synchronized\\|instanceof\\|this\\|super\\|public\\|protected\\|private\\|static\\|final\\|abstract\\|native\\|transient\\|volatile\\|throws\\|interface\\|class\\|implements\\|extends\\|import\\|package\\)\\>" nil keyword)
   ;; primitive types
   ;("\\<\\(void\\|boolean\\|int\\|byte\\|char\\|short\\|long\\|float\\|double\\)\\>" nil type)
   ;; primitive literals
   ("\\<\\(null\\|true\\|false\\)\\>" nil literal)
   ("\\<[0-9]+\\([.][0-9]*\\)?\\([eE]?[-+]?[0-9]+\\)?[fFdD]?\\>" nil literal)
   ("\\<[0-9]+[lL]?\\>" nil literal)
   ("\\<0[xX][0-9A-Fa-f]+\\>" nil literal)
   ;; class/interface decls (must be before uncoloring field/method types)
   ;; (color starting with class/interface keyword (optionally preceded with 
   ;;  class modifiers), until open brace or semicolon)
   ("^[ \t]*\\<\\(\\(public\\|abstract\\|final\\)[ \t\n]*\\)*\\(interface\\|class\\|package\\)\\>[^{;\\]*" nil declaration)
   ;; uncolor non-types: words before instanceof; after import, package;
   ;; (must be before types)
   ("\\<\\(\\w\\|[][.]\\)+[ \t\n]+instanceof\\>" nil default)
   ("\\<\\(import\\|package\\)[ \t\n]+\\(\\w\\|[.]\\)+\\>" nil default)
   ;; types 
   ;; (3. color words that are before another word. [keywords recolored above.]
   ;;  2. (casts) followed by word boundary [fails to color casts followed by 
   ;;     parenthesized expression]
   ;;  1. words after instanceof)
   ("\\(\\(\\w\\|[][.]\\)+[ \t\n]+\\<\\)+" nil type)
   ("(\\(\\w\\|[][.]\\)+)[ \t\n]+\\<" nil type)
   ("\\<instanceof[ \t\n]+\\(\\w\\|[][.]\\)+\\>" nil type)
   ;; alternative if types not colored:  
   ;; Uncolor types in field/method decls (must be before field/method decls)
   ;; (color words that are after access keyword and before another word)
   ;;;   ("^[ \t\n]*\\<\\(public\\|protected\\|private\\|static\\|abstract\\|native\\|transient\\|volatile\\)\\>[ \t\n]+\\(\\(\\w\\|[][.]\\)+[ \t\n]+\\<\\)*" nil default)
   ;; field/method decls -- only recognized after an access keyword 
   ;; like public, protected, private, static, etc.
   ;; (color starting at access keyword, until certain punctuation)
   ("^[ \t\n]*\\<\\(public\\|protected\\|private\\|static\\|abstract\\|native\\|transient\\|volatile\\)\\>[^=({;]*" nil declaration)
   ))

;; end

;******************************************************************************
;*                                                                            *
;*  myfunc.el - Various snadder functions                                     *
;*                                                                            *
;******************************************************************************

;*** Path specifications that may be updated regularily ***********************

(defconst home     (concat (getenv "HOME") "/"))
(defconst dev_home (concat (getenv "DEV_HOME") "/"))

(cond (window-system
       (setq hilit-mode-enable-list  '(not text-mode)
	     hilit-background-mode   'light            ;; 'light or 'dark
	     hilit-inhibit-hooks     nil
	     hilit-inhibit-rebinding nil)
       (let ((num-colors 'many))            ;; 'many, or 'few for 16 color LCD
	 ;; customize color mapping, as you wish.
         ;; Coloring strategies you may wish to keep in mind:
         ;;  o Reserve more noticeable color (e.g., reds) just for words
         ;;    that you must scan for from greater distances, e.g.,
         ;;    method definition names, not words within methods.
         ;;  o Use low contrast colors for close-distance highlighting:
         ;;    Preserve horizontal readability of program text by coloring
         ;;    keywords, types, and literals with colors that are similar
         ;;    in intensity to the default text color, not high contrast
         ;;    colors.  Garish, high-contrast coloring distracts the eye
         ;;    into noticing the pattern formed by the colors, e.g., grouping
         ;;    magenta words on adjacent lines, making them more difficult to
         ;;    read horizontally.  You can also reduce this effect by reducing
         ;;    the coloring, e.g., by leaving types the default color.
	 ;; note: on NT italic doesn't work in emacs 19.34.6
	 (cond ((and (eq num-colors 'many) (eq hilit-background-mode 'light))
		;; for Black text on White, FloralWhite, etc. background
		(hilit-translate comment     'firebrick
				 doccomment  'firebrick
				 string      'DarkGoldenrod4
				 literal     'DarkGoldenrod4
				 declaration 'black
				 keyword     'Blue3
				 type        'Purple3
				 default     'Black));; used for uncoloring
	       ((and (eq num-colors 'many) (eq hilit-background-mode 'dark))
		;; for White text on Black background
		(hilit-translate comment     'PaleGreen-italic
				 doccomment  'Green2-italic
				 string      'Khaki2
				 literal     'Khaki2
				 declaration 'LightSalmon1
				 keyword     'LightBlue1
				 type        'Thistle1
				 default     'White));; used for uncoloring
	       ((and (eq num-colors 'few) (eq hilit-background-mode 'light))
		;; few colors (e.g., 16 color LCD),  Black text on White
		(hilit-translate comment     'ForestGreen-italic
				 doccomment  'ForestGreen-italic
				 string      'Goldenrod
				 literal     'Goldenrod
				 declaration 'Red
				 keyword     'Navy
				 type        'Brown
				 default     'Black));; used for uncoloring
	       ((and (eq num-colors 'few) (eq hilit-background-mode 'dark))
		;; few colors (e.g., 16 color LCD), White text on Black
		(hilit-translate comment     'Green-italic
				 doccomment  'DarkGreen-italic
				 string      'Yellow
				 literal     'Yellow
				 declaration 'Magenta
				 keyword     'Cyan
				 type        'Grey
				 default     'White));; used for uncoloring
	       ((eq hilit-background-mode 'mono)
		;; hilit bug: using default doesn't unitalicize, etc.
		(hilit-translate comment     'default-italic
				 doccomment  'default-bold-italic
				 string      'default-bold
				 literal     'default-bold
				 declaration 'default-bold-italic
				 keyword     'default-bold
				 type        'default-italic))
	       )
	 )))


;*** Shell Setup **************************************************************

(setq comint-output-filter-functions '(comint-postoutput-scroll-to-bottom
                                       comint-strip-ctrl-m))


;*** Command history **********************************************************

(defun my-previous-history (n)
  ;; Go up in history if the buffer height is 1, else go up in the buffer.
  ;; Position goes to the end of line if going up in history.
  (interactive "p")
  (previous-history-element n)
  (end-of-line))

(defun my-next-history (n)
  ;; Go up in history if the buffer height is 1, else go up in the buffer.
  ;; Position goes to the end of line if going up in history
  (interactive "p")
  (next-history-element n)
  (end-of-line))

;(define-key function-key-map [C-up]   [?\M-p])
;(define-key function-key-map [C-down] [?\M-n])


;*** Intelligent buffer switching *********************************************

(defvar LIMIT     1)
(defvar time      0)
(defvar mylist  nil)

(defun time-now ()
  (car (cdr (current-time))))

(defun bubble-buffer ()
  (interactive)
  (if (or (> (- (time-now) time) LIMIT) (null mylist))
      (progn (setq mylist (copy-alist (buffer-list)))
	     (delq (get-buffer " *Minibuf-0*") mylist)
	     (delq (get-buffer " *Minibuf-1*") mylist)))
  (bury-buffer (car mylist))
  (setq mylist (cdr mylist))
  (setq newtop (car mylist))
  (switch-to-buffer (car mylist))
  (setq rest (cdr (copy-alist mylist)))
  (while rest
     (bury-buffer (car rest))
     (setq rest (cdr rest)))
  (setq time (time-now)))

;******************************************************************************

(defun my-isearch-yank-word ()
  (interactive)
  (if (= (length isearch-string) 0)
      (progn (forward-word 1)
             (my-backward-word)))
  (isearch-yank-word))


(defun my-query-replace-yank-word ()
  (interactive)
  (forward-word 1)
  (setq end (point))
  (backward-word 1)
  (setq from-string (buffer-substring (point) end))
  (setq prompt (concat "Replace " from-string " with: "))
  (query-replace from-string (read-from-minibuffer prompt)))



;*** Mouse functions **********************************************************

(defun select-buffer (click)
  ;; Select buffer clicked on by mouse
  (interactive "e")
  (mouse-set-point click)
  (cond ((eq major-mode 'Buffer-menu-mode) (Buffer-menu-select))
         (t                                (mouse-select click))))


(defun my-select-word ()
  (interactive)
  (forward-word 1)
  (setq end (point))
  (backward-word 1)
  (copy-region-as-kill (point) end))



;*** Emacs MAIL ***************************************************************

(defun my-mail-refresh ()
  ;; Remove deleted mail and load new ones
  (interactive)
  (rmail-summary-expunge)
  (rmail-summary-get-new-mail))



;*** Improved scrolling *******************************************************

(defun my-scroll-up ()
  (interactive)
  (scroll-up)); 50)
;  (next-line 50))

(defun my-scroll-down ()
  (interactive)
  (scroll-down)) ; 50)
;  (previous-line 50))



;******************************************************************************

(defun yes-or-no-p (prompt)
  ;; Fast ansewring on silly questions
  (y-or-n-p prompt))

(defun my-menu-bar-mode ()
  ;; Toggle the menubar immediately
  (interactive)
  (menu-bar-mode 0)
  (scroll-down-keep-cursor)
  (scroll-up-keep-cursor))

(defun my-norsk (key)
  ;; Prints the binding of the given key. Usually bound to keys with
  ;; some special keybindings.
  (interactive "k")
  (insert key))

(defun my-big-lpr-region (beg end)
  ;; The default emacs lpr-region is compressed.
  ;; This function prints current region in normal size.
  (interactive "r")
;;  (setq lpr-switches (list ""))
  (lpr-region beg end))
;;  (setq lpr-switches (list "-w200")))

(defun my-kill-word ()
  ;; Delete word where cursor is located
  (interactive)
  (forward-char 1)
  (my-backward-word)
  (kill-word 1)
  (delete-char 1))

(defun load-current ()
  (interactive)
  (load-file (buffer-name)))


;*** Improved OCCUR ***********************************************************

(defun my-occur ()
  ;; Run the emacs internal 'occur' function on the word
  ;; cursor is currently at
  (interactive)
  (occur (current-word)))



;*** Compile stuff ************************************************************

(defun my-compile-init ()
  ;; To run when the compilation buffer is initiated.
  (interactive)
  (local-set-key [f5]  'compile-goto-error))    ; [F5]

(defun my-compile ()
  ;; Compile current file. Avoid all silly questions and confirms.
  (interactive)
  (setq filename (substring (buffer-name) 0 (- (length (buffer-name)) 2)))
  (compile (concat "~/bin/compile " filename)))

(defun my-previous-error ()
  ;; Goto the previous error.
  (interactive)
  (next-error -1))



;*** Powerful file and buffer finding *****************************************

(defun set-name ()
  ;; Put the buffer name in the frame header.
  (interactive)
  (modify-frame-parameters (selected-frame) (list (cons 'name (buffer-name)))))

(defun file-name-p (str)
  ;; Return true if str contains only characters normally used in file names.
  (string-match "^[a-zA-Z0-9/._+-]+$" str))

(defun my-switch-to-file ()
  ;; Fetch the file specified in the cursor location.
  ;; If this is a random filename from a random buffer, try to find it.
  ;; If not found immediately, try on several (include-) directories.
  (interactive)
  (setq here (point))

  ; Determine start of file name
  (while (file-name-p (char-to-string (preceding-char)))
    (forward-char -1))
  (setq beg (point))

  ; Determine end of file name
  (while (file-name-p (char-to-string (following-char)))
    (forward-char 1))
  (setq end (point))

  (goto-char here)

  ; Find the file. Check various directories in prioritized order
  (cond
        ; Cursor is not at something similar to a file name
        ((= beg end) (list-buffers))

        ; Check current directory
        ((file-exists-p (buffer-substring beg end))
         (find-file     (buffer-substring beg end)))

        ; Check the official IES baseline
        ((file-exists-p (concat chi      (buffer-substring beg end)))
        (find-file     (concat baseline (buffer-substring beg end))))

        ; Check my home directory
        ((file-exists-p (concat "~/" (buffer-substring beg end)))
         (find-file     (concat "~/" (buffer-substring beg end))))

        ; Not found: List the current buffers instead
        (t  (list-buffers)))

  (set-name))



;*** Next and previous word. Much better than the silly emacs defaults ********

(defun my-forward-word ()
  ;; Move one word forward. Leave the pointer at start of word
  ;; instead of emacs default end of word. Treat _ as part of word
  (interactive)
  (forward-char 1)
  (backward-word 1)
  (forward-word 2)
  (backward-word 1)
  (backward-char 1)
  (cond ((looking-at "_") (forward-char 1) (my-forward-word))
        (t (forward-char 1))))

(defun my-backward-word ()
  ;; Move one word backward. Leave the pointer at start of word
  ;; Treat _ as part of word
  (interactive)
  (backward-word 1)
  (backward-char 1)
  (cond ((looking-at "_") (my-backward-word))
        (t (forward-char 1))))



;******************************************************************************

(defun my-kill-buffer ()
  ;; Kill default buffer without the silly emacs question
  (interactive)
  (kill-buffer (buffer-name))
  (set-name))

(defun my-switch-to-buffer ()
  ;; Switch to buffer without the silly emacs question
  (interactive)
  (switch-to-buffer (other-buffer))
  (set-name))



;******************************************************************************

(defun delete-this-char ()
  ;; Delete current character
  (interactive)
  (delete-char 1))

(defun backspace ()
  ;; Delete previous character
  (interactive)
  (delete-char -1))

(defun zappa-to-char (arg char)
  ;; Kill up to (but not including) ARG'th occurrence of CHAR
  (interactive "p\ncZappa to char: ")
  (kill-region (point)
               (progn (search-forward (char-to-string char) nil nil arg)
                      (backward-char 1)
                      (point))))



;******************************************************************************

(defun scroll-down-keep-cursor ()
  ;; Scroll the text one line down while keeping the cursor
  (interactive)
  (scroll-down 1))

(defun scroll-up-keep-cursor ()
  ;; Scroll the text one line up while keeping the cursor
  (interactive)
  (scroll-up 1))



;******************************************************************************

(defun split-win-vert ()
  ;; Split window vertically
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (my-switch-to-buffer)
  (other-window 1)
  (defconst vertical 1))

(defun split-win-hori ()
  ;; Split window horizontally
  (interactive)
  (split-window-horizontally)
  (defconst vertical 2))

(defun remove-split ()
  ;; Remove split. Keep window where cursor is
  (interactive)
  (delete-other-windows)
  (defconst vertical 0)
  (set-name))

(defun new-enlarge-window ()
  ;; Enlarge window 1 character or line
  (interactive)
  (if (eq vertical 1)
      (enlarge-window 1)
      (enlarge-window-horizontally 1)))

(defun new-shrink-window ()
  ;; Shrink window 1 character or line
  (interactive)
  (if (eq vertical 1)
      (shrink-window 1)
      (shrink-window-horizontally 1)))



;*** Minibuffer stuff *********************************************************

(defun clear-minibuffer()
  ;; Clear the minibuffer
  (interactive)
  (message nil))

(defun my-parse-minibuffer()
  ;; Extension to the complete word facility of the minibuffer
  ;; First check if a directory specification is given
  (interactive)
  (backward-char 3)
  (setq found t)
  (cond
    ; local directories
    ((looking-at ".cd") (setq directory home))

    ; Personal development
    ((looking-at "dev") (setq directory dev_home))
    ((looking-at ".cc") (setq directory (concat dev_home "cc/src/no/geosoft/cc/")))
    ((looking-at ".ls") (setq directory (concat dev_home "LogStudio/src/no/geosoft/logstudio/")))
    ((looking-at "www") (setq directory (concat dev_home "geosoft.no/LogStudio/")))

    (t                  (setq found nil)))
  (cond (found (beginning-of-line)
	       (kill-line)
	       (insert directory))
	(t     (forward-char 3)
	       (minibuffer-complete))))


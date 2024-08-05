;******************************************************************************
;*                                                                            *
;*  mycmode.el                                                                *
;*  ----------                                                                *
;*  This is a collection of very time effective and powerful C-mode commands. *
;*							                      *
;******************************************************************************

;; Force .h files to use c++ mode
(setq auto-mode-alist (append '(("\\.h$" . c++-mode)) auto-mode-alist))

(defvar c-hanging-braces-alist '((brace-list-open)
                                 (substatement-open after)
                                 (block-close . c-snug-do-while)))

(defun my-c-mode-hook ()
  ;; This function is run prior to c-mode startup
  (setq c-auto-newline t)                        ; Automatic nl after semicolon
  (setq c-basic-offset 2)                        ; Basic indentation offset to 2
  (setq tab-width      2)                        ;
  (setq indent-tabs-mode nil)                    ; Convert tabs to spaces
  (c-set-offset 'case-label 2)                   ; 2 also for case statements
  (c-set-offset 'statement-cont 'c-lineup-math)) ; Eq. statements proper align


;******************************************************************************

(defun c-forward ()
  ;; Move forward to next C function
  (interactive)
  (end-of-defun 2)
  (c-backward))

(defun c-backward ()
  ;; Move backward to previous C function
  (interactive)
  (beginning-of-defun)
  (search-backward "(")
  (beginning-of-line))

(defun my-java-println ()
  ;;
  (interactive)
  (insert "System.out.println ();")
  (backward-char 2)
  (c-indent-command))

(defun html-code ()
  (interactive)
  (backward-word 1)
  (insert "<code>")
  (forward-word 1)
  (insert "</code>"))

(defun my-c-brace()
  ;; Insert matching } in correct position when { is typed
  (interactive)
  (insert "{")
  (c-indent-command)
  (insert "\n\n}")
  (c-indent-command)
  (previous-line 1)
  (c-indent-command))

(defun my-c-paranteces ()
  ;; Insert matching ) in correct position when ( is typed
  (interactive)
  (insert "()")
  (backward-char 1))

(defun my-c-brackets ()
  ;; Insert matching ] in correct position when [ is typed
  (interactive)
  (insert "[]")
  (backward-char 1))

(defun my-c-include-define ()
  ;; Insert #include or #define statement whenever a #d or #i is typed 
  (interactive)
  (insert "#")
  (setq char (read-char))
  (cond ((= char ?d) (insert "define "))
	((= char ?i) (insert "include \".h\"")
                     (backward-char 3)
		     (c-color))
	(t (insert char))))

(defun my-c-while ()
  ;; Insert complete while statement
  (interactive)
  (insert "while () {")
  (c-indent-command)
  (insert "\n\n}")
  (c-indent-command)
  (previous-line 1)
  (c-indent-command)  
  (previous-line 1)
  (end-of-line)
  (backward-char 3))

(defun my-c-for ()
  ;; Insert complete for statement
  (interactive)
  (insert "for () {")
  (c-indent-command)
  (insert "\n\n}")
  (c-indent-command)
  (previous-line 1)
  (c-indent-command)
  (previous-line 1)
  (end-of-line)
  (backward-char 3))

(defun my-c-if ()
  ;; Insert complete if statement
  (interactive)
  (insert "if () {")
  (c-indent-command)
  (insert "\n\n}")
  (c-indent-command)
  (previous-line 1)
  (c-indent-command)
  (previous-line 1)
  (end-of-line)
  (backward-char 3))

(defun my-c-else ()
  ;; Insert complete else statement
  (interactive)
  (insert "else {")
  (c-indent-command)
  (insert "\n\n}")
  (c-indent-command)  
  (previous-line 1)
  (c-indent-command))

(defun my-c-do ()
  ;; Insert complete do statement
  (interactive)
  (insert "do {")
  (c-indent-command)
  (insert "\n} while ();")
  (c-indent-command)
  (previous-line 1)
  (newline-and-indent))

(defun my-c-switch ()
  ;; Insert switch statement
  (interactive)
  (insert "switch () {")
  (c-indent-command)
  (insert "\n\n}")
  (c-indent-command)
  (previous-line 1)
  (c-indent-command)  
  (previous-line 1)
  (end-of-line)
  (backward-char 3))

(defun my-c-case ()
  ;; Insert case statement
  (interactive)
  (insert "case 1 : ")
  (c-indent-command)  
  (insert "\n\nbreak;")
  (c-indent-command)
  (previous-line 1)
  (c-indent-command)
  (previous-line 1)
  (end-of-line)
  (backward-char 3)
  (delete-backward-char 1))


(defun middle-of-statement ()
  ;; Return true if cursor is in the middle of a c statement.
  (interactive)
  (setq here (point))
  (c-beginning-of-statement 1)
  (setq beg (point))
  (c-end-of-statement 1)
  (setq end (point))
  (goto-char here)
  (if (and (> here beg)
	   (< here end))
      t
      nil))

(defun my-c-comment ()
  ;; Insert comment brackets on current line
  (interactive)
  (setq middle (middle-of-statement))
  (back-to-indentation)  
  (setq beg (point))
  (end-of-line)
  (cond ((equal beg (point))
	   (c-indent-command)
	   (insert "/*  */")
	   (backward-char 3))
	(middle
	   (insert "/*  */")
	   (backward-char 3))
	(t (goto-char beg) 
	   (c-end-of-statement 1)
           (insert " */")
           (goto-char beg)
           (insert "/* ")))
  (c-color))



(defun my-c-semicolon (arg)
  (interactive "P")
  (c-electric-semi&comma arg)
  (setq char (read-char))
  (cond ((= char ?;)    (beginning-of-line)
	                (kill-line)
	                (previous-line 1)
		        (end-of-line))
	((= char ?\015) (newline-and-indent))
	(t              (insert char))))




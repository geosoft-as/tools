;******************************************************************************
;*                                                                            *
;*  myjavamode.el                                                             *
;*  -------------                                                             *
;*  This is a collection of very time effective and powerful C-mode commands. *
;*							                      *
;******************************************************************************

(defun println ()
  ;; Insert a println statement in java code
  (interactive)
  (insert "System.out.println();")
  (backward-char 2)
  (c-indent-command))


(defun java-main-skeleton ()
  ;; Insert a main method skeleton at current location
  (interactive)
  (insert "public static void main(String[] arguments)")
  (c-indent-command)
  (insert "\n{")
  (c-indent-command)
  (insert "\njavax.swing.JFrame f = new javax.swing.JFrame();")
  (c-indent-command)
  (insert "\nf.setDefaultCloseOperation(javax.swing.JFrame.EXIT_ON_CLOSE);");
  (c-indent-command)
  (insert "\n")
  (c-indent-command)
  (insert "\n")
  (c-indent-command)
  (insert "\n")
  (c-indent-command)
  (insert "\nf.pack();")
  (c-indent-command)
  (insert "\nf.setVisible(true);")
  (c-indent-command)
  (insert "\n}")
  (c-indent-command)
  (previous-line 4)
  (c-indent-command))


(defun java-file-skeleton ()
  ;; Insert java package name at current location
  (interactive)
  (insert "package ")
  (setq here (point))
  (insert default-directory)
  (search-backward "/no/")
  (forward-char 1)
  (kill-region here (point))
  (replace-string "/" ".")
  (c-indent-command)
  (end-of-line)
  (backward-char 1)
  (delete-char 1)
  (insert ";\n\n\n\n")
  (setq class-name (substring (buffer-name) 0 (- (length (buffer-name)) 5)))

  ;; Class outline
  (insert "public final class " class-name)
  (insert "\n{\n\n\n\n}\n")
  (previous-line 2)

  ;; Constructor
  (insert "public " class-name "()")
  (c-indent-command)
  (insert "\n{")
  (c-indent-command)
  (insert "\n}")
  (c-indent-command)
  (previous-line 4)
  (c-indent-command))



(defun java-set-get()
  ;;
  (interactive)
  (setq here (point))
  (end-of-buffer)
  (search-backward "}")
  (setq insertPoint (point))
  (goto-char here)
  (end-of-line)
  (backward-word 1)
  (setq varname (current-word))
  (setq funcname (upcase-initials varname))
  (backward-word 1)
  (setq type (current-word))
  (goto-char insertPoint)

  ;; get function
  (insert "\npublic " type " get" funcname "()")
  (c-indent-command)
  (insert "\n{")
  (c-indent-command)
  (insert "\nreturn " varname ";")
  (c-indent-command)
  (insert "\n}")
  (c-indent-command)
  (insert "\n\n")

  ;; set function
  (insert "public void set" funcname "(" type " " varname ")")
  (c-indent-command)
  (insert "\n{")
  (c-indent-command)
  (insert "\nthis." varname " = " varname ";")
  (c-indent-command)
  (insert "\n")
  (c-indent-command)
  (insert "\n}")
  (goto-char here)
  (next-line 1)
  (next-line 1))


(defun javadoc-method ()
  ;; Insert a Javadoc method comment outline at the cursor position
  (interactive)
  (beginning-of-line)
  (setq start (point))
  (next-line 1)
  (setq startcomment (point))
  (insert "/**")
  (c-indent-command)
  (insert "\n* ")
  (c-indent-command)
  (insert "\n* ")
  (c-indent-command)
  (setq paramdoc (point))
  (insert "\n*/\n")
  (c-indent-command)

  ;; Parameters
  (goto-char start)
  (search-forward "(")
  (setq start-param (point))
  (search-forward ")")
  (setq end-param (point))
  (goto-char start-param)
  (forward-word 2)
  (backward-char 1)
  (while (< (point) end-param)
    (setq now (point))
    (setq param (current-word))
    (goto-char paramdoc)
    (end-of-line)
    (insert "\n* @param " param)
    (setq paramdoc (point))
    (c-indent-command)
    (goto-char now)
    (forward-word 3)
    (backward-char 1))

  ;; Return statement
  (goto-char start)
  (search-forward "(")
  (backward-word 2)
  (setq returnparam (current-word))
  (cond ((not (equal returnparam "void"))
         (goto-char paramdoc)
         (end-of-line)
         (insert "\n* @return ")
         (setq paramdoc (point))
         (c-indent-command)))

  ;; Move entire block above the method name
  (goto-char startcomment)
  (beginning-of-line)
  (setq start (point))
  (search-forward "*/")
  (forward-char 1)
  (kill-region start (point))
  (previous-line 1)
  (yank)

  ;; Indent correctly
  (search-backward "/**")
  (setq start (point))
  (search-forward "*/")
  (setq end (point))
  (goto-char start)
  (while (< (point) end)
    (c-indent-command)
    (forward-line 1))

  ;; Move cursor to beginning of javadoc block
  (goto-char start)
  (next-line 1)
  (end-of-line))



(defun javadoc-class ()
  ;; Insert a Javadoc class comment outline at the cursor position
  (interactive)
  (insert
"/**
 *
 *
 *
 * @author <a href=\"mailto:jacob.dreyer@geosoft.no\">Jacob Dreyer</a>
 */
")
  (previous-line 5)
  (end-of-line))

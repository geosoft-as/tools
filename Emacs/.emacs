;******************************************************************************
;*                                                                            *
;*   .emacs - Emacs 19.35 startup file                                        *
;*                                                                            *
;*   o Verified on Emacs 23.1.1 (MS/Windows)                                  *
;*   o Verified on Emacs xx (Linux)                                           *
;*                                                                            *
;*   GeoSoft (C) 1992 - 2021                                                  *
;*                                                                            *
;******************************************************************************

;*** Default setup ************************************************************

(setq inhibit-startup-message    t)        ; Don't want any silly startup mess.
(setq initial-scratch-message    nil)      ; Don't want any silly startup mess.
(setq make-backup-files          nil)      ; Don't want any silly backup files
(setq auto-save-list-file-name   nil)      ; Don't want any silly .saves files
(setq auto-save-default          nil)      ; Don't want any silly auto saving
(setq search-highlight           t)        ; Highlight search object
(setq query-replace-highlight    t)        ; Highlight query object
(setq mouse-sel-retain-highlight t)        ; Keep mouse highlightening
(setq baud-rate                  100000)   ; Increased cursor movement speed
(setq frame-title-format         "%b")     ; Frame title to current buffer
(setq suggest-key-bindings       nil)      ; Turn off the silly key explanaition
(setq compilation-read-command   nil)      ; Compile without question
(setq default-truncate-lines     t)        ; Don't want any line wrapping

; Git support slows down opening files. Turn it off for now
(setq vc-handled-backends nil)

; REMOVE THESE WHEN VERIFIED THAT THE STUFF WORKS ON LINUX
;(require 'pc-select)
;(pc-selection-mode)

;(setq x-select-enable-clipboard  t)
;(setq interprogram-cut-function 'x-select-text)
;(setq interprogram-paste-function 'get-clipboard)

(scroll-bar-mode     -1)                   ; Turn off the silly scrollbar
(menu-bar-mode       -1)                   ; Turn off the silly menubar
(tool-bar-mode       -1)                   ; Turn off the silly toolbar
(auto-save-mode     nil)

; Mouse selection and interprogram copy-paste.
; Order of the two matters as the first sets the second to nil
(setq interprogram-cut-function 'x-select-text)

;(set-face-background 'modeline "black")  ; Set modeline background color
;(set-face-foreground 'modeline "white")  ; Set modeline foreground color
(set-face-background 'region   "yellow")  ; Set region background color
(set-background-color "lightgray")
(set-foreground-color "black")
(set-cursor-color     "black")

;(setq lpr-command "lpr")                   ; Override the defualt "lp"
;(require 'mldrag)                          ; Modeline mouse dragging

(standard-display-8bit 32 255)             ; Norwegian chars: æ ø å Æ Ø Å

;(add-hook 'after-init-hook    'clear-minibuffer) ; Clear minibuffer
(add-hook 'c-mode-common-hook 'my-c-mode-hook)   ; C-mode extensions
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)

;; Seems like the after-init-hook doesn't work anymore. Use this instead
(defun display-startup-echo-area-message()
  (message ""))

(setenv "ESHELL" "/bin/csh")               ; Use csh rather than tcsh in shell

(setq standard-indent 2)


;*** Load specific functions **************************************************

(load "cc-mode")                                ; The ultimate c-mode!
(load (concat (getenv "DEV_HOME") "/tools/Emacs/hilit19.el"))      ; General coloring function
(load (concat (getenv "DEV_HOME") "/tools/Emacs/java-hilit19.el")) ; Java coloring mode
(load (concat (getenv "DEV_HOME") "/tools/Emacs/mycmode.el"))      ; Additional features in c-mode
(load (concat (getenv "DEV_HOME") "/tools/Emacs/myjavamode.el"))   ; Additional features in java-mode
(load (concat (getenv "DEV_HOME") "/tools/Emacs/mycsharpmode.el")) ; Additional features in csharp-mode
(load (concat (getenv "DEV_HOME") "/tools/Emacs/myfunc.el"))       ; Some snadder functions

(require 'ange-ftp)

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . java-mode)) auto-mode-alist))

; LINUX ONLY!
;(set-default-font "Monospace-10")


;*** Printing *****************************************************************

; (setq ps-printer-name "//draupner/HPLaserJetCM3530MFP")

(setq ps-printer-name t)
(setq ps-lpr-command "C:\\Program Files\\gs\\gs9.15\\bin\\gswin64c.exe")
(setq ps-lpr-switches '("-q" "-dNOPAUSE" "-dBATCH"
                        "-sDEVICE=mswinpr2"
                        "-sPAPERSIZE=a4"))


;(setenv "PRINTER" "PDFCreator")
;(cond ((eq system-type 'windows-nt)
;   (setq ps-printer-name "PDFCreator")
;   (setq ps-printer-name-option "-d")
;   (setq ps-lpr-command "c:/cygwin/bin/lpr.exe")))

(setq ps-print-header nil)
(setq ps-font-size 8)
(setq ps-top-margin 50)
(setq ps-bottom-margin 10)
(setq ps-left-margin 5)
(setq ps-right-margin 0)


;*** Global keybindings *******************************************************

(global-set-key    [delete]            'undo)            ; [Delete]

(global-set-key    [f1]            'remove-split)            ; [F1]
(global-set-key    [C-f1]          'delete-window)           ; [Ctrl]-[F1]
(global-set-key    [f2]            'split-win-vert)          ; [F2]
(global-set-key    [f3]            'compile)                 ; [F3]
(global-set-key    [f4]            'bubble-buffer)           ; [F4]
(global-set-key    [f5]            'copy-region-as-kill)     ; [F5]
(global-set-key    [f16]           'copy-region-as-kill)     ; [Copy]
(global-set-key    [f6]            'kill-region)             ; [F7]
(global-set-key    [f20]           'kill-region)             ; [Cut]
(global-set-key    [f7]            'yank)                    ; [F8]
(global-set-key    [f18]           'yank)                    ; [Paste]
(global-set-key    [f8]            'ignore-key)              ; [F9]
(global-set-key    [f9]            'ignore-key)              ; [F9]
(global-set-key    [f10]           'my-norsk)                ; [F10]      æ
(global-set-key    [S-f10]         'my-norsk)                ; [S]-[F10]  Æ
(global-set-key    [f11]           'my-norsk)                ; [F11]      ø
(global-set-key    [S-f11]         'my-norsk)                ; [S]-[F11]  Ø
(global-set-key    [f12]           'my-norsk)                ; [F12]      å
(global-set-key    [S-f12]         'my-norsk)                ; [S]-[F12]  Å
(global-set-key    [f13]           'ignore-key)              ; [Props]
(global-set-key    [f14]           'undo)                    ; [Undo]
(global-set-key    [kp-subtract]   'undo)                    ; [Undo]
(global-set-key    [f15]           'ignore-key)              ; [Front]
(global-set-key    [f17]           'vc-toggle-read-only)     ; [Open] (MWM)
(global-set-key    [f19]           'find-file)               ; [Find]
(global-set-key    [C-f19]         'find-alternate-file)     ; [Ctrl]-[Find]
(global-set-key    [f21]           'ps-print-region-with-faces) ; [Pause]
(global-set-key    [pause]         'ps-print-region-with-faces) ; [Pause]
(global-set-key    [f22]           'ps-print-region-with-faces) ; [PrSc]
(global-set-key    [print]         'ps-print-region-with-faces) ; [PrSc]
(global-set-key    [C-f22]         'my-big-lpr-region)       ; [Ctrl]-[PrSc]
(global-set-key    [C-print]       'my-big-lpr-region)       ; [Ctrl]-[PrSc]
(global-set-key    [f24]           'scroll-down-keep-cursor) ; [=]
(global-set-key    [kp-divide]     'scroll-down-keep-cursor) ; [/]
(global-set-key    [f25]           'other-window)            ; [/]
(global-set-key    [kp-begin]      'other-window)            ; [5]
(global-set-key    [cancel]        'other-window)            ; [5]
(global-set-key    [f26]           'scroll-up-keep-cursor)   ; [*]
(global-set-key    [kp-multiply]   'scroll-up-keep-cursor)   ; [*]
(global-set-key    [up]            'previous-line)           ; [Up]
(global-set-key    [kp-up]         'previous-line)           ; [Up]
(global-set-key    [C-up]          'backward-paragraph)      ; [Ctrl]-[Up]
(global-set-key    [C-kp-up]       'backward-paragraph)      ; [Ctrl]-[Up]
(global-set-key    [down]          'next-line)               ; [Down]
(global-set-key    [kp-down]       'next-line)               ; [Down]
(global-set-key    [C-down]        'forward-paragraph)       ; [Ctrl]-[Down]
(global-set-key    [C-kp-down]     'forward-paragraph)       ; [Ctrl]-[Down]
(global-set-key    [C-left]        'my-backward-word)        ; [Ctrl]-[Left]
(global-set-key    [C-kp-left]     'my-backward-word)        ; [Ctrl]-[Left]
(global-set-key    [M-left]        'backward-kill-word)      ; [ESC]-[Left]
(global-set-key    [M-kp-left]     'backward-kill-word)      ; [ESC]-[Left]
(global-set-key    [C-right]       'my-forward-word)         ; [Ctrl]-[Right]
(global-set-key    [C-kp-right]    'my-forward-word)         ; [Ctrl]-[Right]
(global-set-key    [M-right]       'kill-word)               ; [ESC]-[Right]
(global-set-key    [M-kp-right]    'kill-word)               ; [ESC]-[Right]
(global-set-key    [f27]           'beginning-of-buffer)     ; [Home]
(global-set-key    [home]          'beginning-of-buffer)     ; [Home]
(global-set-key    [kp-home]       'beginning-of-buffer)     ; [Home]
(global-set-key    [f33]           'end-of-buffer)           ; [End]
(global-set-key    [end]           'end-of-buffer)           ; [End]
(global-set-key    [kp-end]        'end-of-buffer)           ; [End]
(global-set-key    [C-f27]         'back-to-indentation)     ; [Ctrl]-[Home]
(global-set-key    [C-home]        'back-to-indentation)     ; [Ctrl]-[Home]
(global-set-key    [C-kp-home]     'back-to-indentation)     ; [Ctrl]-[Home]
(global-set-key    [f29]           'my-scroll-down)          ; [PgUp]
(global-set-key    [prior]         'my-scroll-down)          ; [PgUp]
(global-set-key    [kp-prior]      'my-scroll-down)          ; [PgUp]
(global-set-key    [f35]           'my-scroll-up)            ; [PgDn]
(global-set-key    [next]          'my-scroll-up)            ; [PgDn]
(global-set-key    [kp-next]       'my-scroll-up)            ; [PgDn]
(global-set-key    [f31]           'recenter)                ; [R11]
(global-set-key    [C-f31]         'transient-mark-mode)     ; [Ctrl]-[R11]
(global-set-key    [C-kp-begin]    'transient-mark-mode)     ; [Ctrl]-[5]
(global-set-key    [C-f33]         'end-of-line)             ; [Ctrl]-[End]
(global-set-key    [C-kp-end]      'end-of-line)             ; [Ctrl]-[End]
(global-set-key    [insert]        'overwrite-mode)          ; [Ins]
(global-set-key    [kp-insert]     'overwrite-mode)          ; [Ins]
(global-set-key    [delete]        'delete-char)             ; [Del]
(global-set-key    [C-delete]      'my-kill-buffer)          ; [Ctrl]-[Del]
(global-set-key    [C-kp-delete]   'my-kill-buffer)          ; [Ctrl]-[Del]
(global-set-key    [help]          'apropos)                 ; [Help]
(global-set-key    [C-help]        'describe-function)       ; [Ctrl]-[Help]
(global-set-key    [kp-enter]      'hippie-expand)           ; [Enter]
(global-set-key    "\C-l"          'goto-line)               ; [Ctrl]-[L]
(global-set-key    "\C-b"          'scroll-bar-mode)         ; [Ctrl]-[B]
(global-set-key    "\M-b"          'my-menu-bar-mode)        ; [ESC]-[B]
(global-set-key    "\C-b"          'my-query-replace-yank-word) ;
(global-set-key    "\C-f"          'make-frame)              ; [Ctrl]-[F]
(global-set-key    "\M-f"          'delete-frame)            ; [ESC]-[F]
(global-set-key    "\C-z"          'zappa-to-char)           ; [Ctrl]-[Z]
(global-set-key    "\C-t"          'my-kill-word)            ; [Ctrl]-[T]
(global-set-key    [down-mouse-1]  'select-buffer)



;*** C keybindings ************************************************************

(define-key c-mode-map    [return]    'newline-and-indent)     ; [Return]
(define-key c-mode-map    [f8]        'my-c-module-comment)    ; [F8]
(define-key c-mode-map    [f9]        'my-c-routine-comment)   ; [F9]
(define-key c-mode-map    [f13]       'hilit-rehighlight-buffer) ; [Props]
(define-key c-mode-map    [kp-add]    'hilit-rehighlight-buffer) ; [+]
(define-key c-mode-map    [C-up]      'c-backward)             ; [Ctrl]-[Up]
(define-key c-mode-map    [C-kp-up]   'c-backward)             ; [Ctrl]-[Up]
(define-key c-mode-map    [C-down]    'c-forward)              ; [Ctrl]-[Down]
(define-key c-mode-map    [C-kp-down] 'c-forward)              ; [Ctrl]-[Down]
(define-key c-mode-map    "\M-w"      'my-c-while)             ; [ESC]-[W]
(define-key c-mode-map    "\M-f"      'my-c-for)               ; [ESC]-[F]
(define-key c-mode-map    "\M-i"      'my-c-if)                ; [ESC]-[I]
(define-key c-mode-map    "\M-e"      'my-c-else)              ; [ESC]-[E]
(define-key c-mode-map    "\M-d"      'my-c-do)                ; [ESC]-[D]
(define-key c-mode-map    "\M-/"      'my-c-comment)           ; [ESC]-[/]
(define-key c-mode-map    "\M-s"      'my-c-switch)            ; [ESC]-[S]
(define-key c-mode-map    "\M-c"      'my-c-case)              ; [ESC]-[C]
(define-key c-mode-map    "#"         'my-c-include-define)    ; [#]
(define-key c-mode-map    "{"         'my-c-brace)             ; [{]

(define-key c++-mode-map    [return]    'newline-and-indent)     ; [Return]
(define-key c++-mode-map    [f8]        'my-c-module-comment)    ; [F8]
(define-key c++-mode-map    [f9]        'my-c-routine-comment)   ; [F9]
(define-key c++-mode-map    [f13]       'hilit-highlight-buffer) ; [Props]
(define-key c++-mode-map    [kp-add]    'hilit-highlight-buffer) ; [+]
(define-key c++-mode-map    [C-up]      'c-backward)             ; [Ctrl]-[Up]
(define-key c++-mode-map    [C-kp-up]   'c-backward)             ; [Ctrl]-[Up]
(define-key c++-mode-map    [C-down]    'c-forward)              ; [Ctrl]-[Down]
(define-key c++-mode-map    [C-kp-down] 'c-forward)              ; [Ctrl]-[Down]
(define-key c++-mode-map    "\M-w"      'my-c-while)             ; [ESC]-[W]
(define-key c++-mode-map    "\M-f"      'my-c-for)               ; [ESC]-[F]
(define-key c++-mode-map    "\M-i"      'my-c-if)                ; [ESC]-[I]
(define-key c++-mode-map    "\M-e"      'my-c-else)              ; [ESC]-[E]
(define-key c++-mode-map    "\M-d"      'my-c-do)                ; [ESC]-[D]
(define-key c++-mode-map    "\M-/"      'my-c-comment)           ; [ESC]-[/]
(define-key c++-mode-map    "\M-s"      'my-c-switch)            ; [ESC]-[S]
(define-key c++-mode-map    "\M-c"      'my-c-case)              ; [ESC]-[C]
(define-key c++-mode-map    "#"         'my-c-include-define)    ; [#]
(define-key c++-mode-map    "{"         'my-c-brace)             ; [{]


;*** Java keybindings *********************************************************

(define-key java-mode-map    [return] 'newline-and-indent)       ; [Return]
(define-key java-mode-map    "\M-p"   'println)
(define-key java-mode-map    "\M-f"   'java-file-skeleton)
(define-key java-mode-map    "\M-s"   'java-set-get)
(define-key java-mode-map    "\M-m"   'java-main-skeleton)
(define-key java-mode-map    "\M-i"   'java-iterator-skeleton)
(define-key java-mode-map    [f8]     'javadoc-class)
(define-key java-mode-map    [f9]     'javadoc-method)
(define-key java-mode-map    [f3]     'compile)
(define-key java-mode-map    [kp-add] 'hilit-highlight-buffer)   ; [+]


;*** CSharp setup *************************************************************

;(define-key csharp-mode-map    [return] 'newline-and-indent)       ; [Return]
;(define-key csharp-mode-map    [f8]     'csharp-class)
;(define-key csharp-mode-map    [f9]     'csharp-method)
;(define-key csharp-mode-map    [kp-add] 'hilit-highlight-buffer)   ; [+]
;(define-key csharp-mode-map    "{"      'my-c-brace)               ; [{]

;*** Search keybindings *******************************************************

(define-key isearch-mode-map "\C-w"   'my-isearch-yank-word) ; [Ctrl]-[w]



;*** Lisp keybindings *********************************************************

(define-key emacs-lisp-mode-map [return] 'newline-and-indent)     ; [Return]
(define-key emacs-lisp-mode-map [f13]    'hilit-highlight-buffer) ; [Props]
(define-key emacs-lisp-mode-map [kp-add] 'hilit-highlight-buffer) ; [+]



;*** Minibuffer keybindings ***************************************************

(setq minibuffer-local-filename-completion-map minibuffer-local-completion-map)
(setq minibuffer-local-must-match-filename-map minibuffer-local-must-match-map)
(define-key minibuffer-local-filename-completion-map " " 'my-parse-minibuffer) ; [Space]



;*** Modeline keybindings *****************************************************

(global-set-key [mode-line down-mouse-1] 'mldrag-drag-mode-line)

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

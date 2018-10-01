;;; w32-grep.el --- Better default for grep etc on w32

(require 'grep)
(eval-when-compile (require 'cl))

;; Fix-me: How can I be sure this is re-read?
(eval-and-compile
(defcustom grep-files-aliases
  ;; Fix-me: The rest of the [] patterns.
  '(("all" .   "* .*")
    ("el" .    "*.el")
    ;;("ch" .    "*.[ch]")
    ;;("c" .     "*.c")
    ("c" .     "*.c *.h")
    ("cc" .    "*.cc *.cxx *.cpp *.C *.CC *.c++")
    ("cchh" .  "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
    ("hh" .    "*.hxx *.hpp *.[Hh] *.HH *.h++")
    ;;("h" .     "*.h")
    ("h" .     "*.h *.c")
    ;;("l" .     "[Cc]hange[Ll]og*")
    ("l" .     "ChangeLog*")
    ;;("m" .     "[Mm]akefile*")
    ("m" .     "Makefile*")
    ("tex" .   "*.tex")
    ("texi" .  "*.texi")
    ("asm" .   "*.[sS]"))
  "*Alist of aliases for the FILES argument to `lgrep' and `rgrep'."
  :type 'alist
  :group 'grep)
)

(defcustom grep-r-command nil
  "The default find command for \\[rgrep].
In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
Customize or call the function `grep-apply-setting'."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :set 'grep-apply-setting
  :group 'grep)

(defcustom grep-r-template nil
  "The default command to run for \\[rgrep].
The following place holders should be present in the string:
 <D> - base directory for find
 <X> - find options to restrict or expand the directory list
 <F> - find options to limit the files matched
 <C> - place to put -i if case insensitive grep
 <R> - the regular expression searched for.
In interactive usage, the actual value of this variable is set up
by `grep-compute-defaults'; to change the default value, use
Customize or call the function `grep-apply-setting'."
  :type '(choice string
		 (const :tag "Not Set" nil))
  :set 'grep-apply-setting
  ;;:version "22.1"
  :group 'grep)

(defun grep-compute-defaults ()
  (unless (or (not grep-use-null-device) (eq grep-use-null-device t))
    (setq grep-use-null-device
          (with-temp-buffer
            (let ((hello-file (expand-file-name "HELLO" data-directory)))
              (not
               (and (if grep-command
                        ;; `grep-command' is already set, so
                        ;; use that for testing.
                        (grep-probe grep-command
                                    `(nil t nil "^English" ,hello-file)
                                    #'call-process-shell-command)
                      ;; otherwise use `grep-program'
                      (grep-probe grep-program
                                  `(nil t nil "-nH" "^English" ,hello-file)))
                    (progn
                      (goto-char (point-min))
                      (looking-at
                       (concat (regexp-quote hello-file)
                               ":[0-9]+:English")))))))))
  (unless (and grep-command grep-find-command
               grep-template grep-find-template)
    (let ((grep-options
           (concat (if grep-use-null-device "-n" "-nH")
                   (if (grep-probe grep-program
                                   `(nil nil nil "-e" "foo" ,null-device)
                                   nil 1)
                       " -e"))))
      (unless grep-command
        (setq grep-command
              (format "%s %s " grep-program grep-options)))
      (unless grep-template
        (setq grep-template
              (format "%s <C> %s <R> <F>" grep-program grep-options)))
      (unless grep-find-use-xargs
        (setq grep-find-use-xargs
              (if grep-find-use-grep-r
                  nil
                (cond
                 ((and
                   (grep-probe find-program
                               `(nil nil nil ,null-device "-print0"))
                   (grep-probe "xargs" `(nil nil nil "-0" "-e" "echo")))
                  'gnu)
                 (t
                  'exec)))))
      (unless grep-r-command
        (setq grep-r-command
              (format "%s -r <C> %s . --include="
                      grep-program grep-options)))
      (unless grep-find-command
        (setq grep-find-command
              (if grep-find-use-grep-r
                  (format "%s -r <C> %s . --include="
                          grep-program grep-options)
                (cond ((eq grep-find-use-xargs 'gnu)
                       (format "%s . -type f -print0 | xargs -0 -e %s"
                               find-program grep-command))
                      ((eq grep-find-use-xargs 'exec)
                       (let ((cmd0 (format "%s . -type f -exec %s"
                                           find-program grep-command)))
                         (cons
                          (format "%s {} %s ;" cmd0 null-device)
                          (1+ (length cmd0)))))
                      (t
                       (format "%s . -type f -print | xargs %s"
                               find-program grep-command))))))
      (unless grep-r-template
        (setq grep-r-template
              (format "%s -r <C> %s <R> . --include=<F>"
                      grep-program grep-options)))
      (unless grep-find-template
        (setq grep-find-template
              (let ((gcmd (format "%s <C> %s <R>"
                                  grep-program grep-options)))
                (cond ((eq grep-find-use-xargs 'gnu)
                       (format "%s . <X> -type f <F> -print0 | xargs -0 -e %s"
                               find-program gcmd))
                      ((eq grep-find-use-xargs 'exec)
                       (format "%s . <X> -type f <F> -exec %s {} %s ;"
                               find-program gcmd null-device))
                      (t
                       (format "%s . <X> -type f <F> -print | xargs %s"
                               find-program gcmd)))))))
;;     (when grep-find-use-grep-r
;;       ;; (message "[%s]\n[%s]\n[%s]\n[%s]\n" grep-template grep-command grep-find-template grep-find-command)
;;       ;;;;;; Default values on w32 using cmd.exe + GnuWin32 utils::
;;       ;;
;;       ;; grep-template = [grep <C> -nH -e <R> <F>]
;;       ;; grep-command  = [grep -nH -e ]
;;       ;; grep-find-template = [find . <X> -type f <F> -print0 | xargs -0 -e grep <C> -nH -e <R>]
;;       ;; grep-find-command  = [find . -type f -print0 | xargs -0 -e grep -nH -e ]
;;       ;;
;;       ;; Need to change those calling the find program:
;;       (setq grep-find-template (replace-regexp-in-string "<C>" "-r <C>" grep-template t t))
;;       (setq grep-find-template (replace-regexp-in-string "<F>" ". --include=<F>" grep-find-template t t))
;;       (setq grep-find-command (replace-regexp-in-string "<C>" "-i" grep-find-template nil nil)) ;; fix-me
;;       (setq grep-find-command (replace-regexp-in-string "<[RF]>" "" grep-find-template nil nil))
;;       ;;(message "New:\n[%s]\n[%s]\n" grep-find-template grep-find-command)
;;       ;; New values:
;;       ;; grep-find-template = [grep -r <C> -nH -e <R> . --include=<F>]
;;       ;; grep-find-command  = [grep -r <C> -nH -e  . --include=]
;;       )
    )
  (unless (or (not grep-highlight-matches) (eq grep-highlight-matches t))
    (setq grep-highlight-matches
          (with-temp-buffer
            (and (grep-probe grep-program '(nil t nil "--help"))
                 (progn
                   (goto-char (point-min))
                   (search-forward "--color" nil t))
                 t)))))

(defun grep-recompute-defaults ()
  (setq grep-command nil
        grep-find-command nil
        grep-r-command nil
        grep-template nil
        grep-find-template nil
        grep-r-template nil
        grep-use-null-device 'auto-detect
        grep-find-use-xargs nil)
  (grep-compute-defaults))

(defcustom grep-find-use-grep-r (memq system-type '(windows-nt))
  "If non-nil then use 'grep -r' instead of 'find | grep'.
On w32 using a port of the GNU find program together with dito
xargs for recursive search is problematic for \(at least) two
reasons:

- The currently known w32 ports of find+xargs have some bug that
  makes it impossible to search for strings containing a space.
- There is a program find.exe that comes with w32 which is by
  default in the path.

Therefor the default on w32 is to 'grep -r' for recursive
searches. Some tests has however indicated this is a little bit
slower than using find+xargs so the latter is used where it can
safely be used."
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (grep-recompute-defaults))
  :type 'boolean
  :group 'grep)

(defun grep-expand-template (template &optional regexp files dir excl for-what)
  "Replace <C>, <D>, <F>, <R>, and <X> in grep TEMPLATE string.
Return the patched string which will be a command to run in the
current shell (see `shell-file-name'). Replacing is governed by
`grep-expand-keywords'.

REGEXP is the regular expression to grep for.
FILES should be a list of shell filename patterns.
DIR should be a directory.
EXCL is currently only used for if FOR-WHAT is 'grep-find

FOR-WHAT may be one of this:

 'grep -- building a command for grep in current directory
 'grep-find -- for find+grep
 'grep-r -- for grep in current directory tree
"
  (let ((command template)
        (cf case-fold-search)
        (case-fold-search nil)
        (files (split-string files))
        (excl ""))
    (unless for-what
      (setq for-what (if dir 'grep-r 'grep)))
    (case for-what
     ('grep
      (setq files (mapconcat (lambda (f) f) files " ")))
     ('grep-r
      (setq files (mapconcat (lambda (f)
                               (shell-quote-argument f)) files " --include=")))
     ('grep-find
      (dolist (c (append "();" nil))
        (let ((cs (char-to-string c)))
          (setq command
                (replace-regexp-in-string
                 cs (shell-quote-argument cs) command t t))))

      (when grep-find-ignored-directories
          (setq excl
                (concat
                 (shell-quote-argument "(")
                 " -path "
                 (mapconcat #'(lambda (dir)
                                (shell-quote-argument
                                 (concat "*/" dir)))
                            grep-find-ignored-directories
                            " -o -path ")
                 " "
                 (shell-quote-argument ")")
                 " -prune -o ")))

      (setq files (concat (shell-quote-argument "(")
                          " -name "
                          (mapconcat #'shell-quote-argument
                                     files
                                     " -o -name ")
                          " "
                          (shell-quote-argument ")"))))
     (t
      (error "Bad value, for-what=%s" for-what)))
    (dolist (kw grep-expand-keywords command)
      (if (string-match (car kw) command)
          (setq command
                (replace-match
                 (or (if (symbolp (cdr kw))
                         (symbol-value (cdr kw))
                       (save-match-data (eval (cdr kw))))
                     "")
                 t t command))))))

(defun rgrep (regexp &optional files dir)
  "Recursively grep for REGEXP in FILES in directory tree rooted at DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-find-command'.

Collect output in a buffer.  While find runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error]
in the grep output buffer, to go to the lines where grep found matches.

This command shares argument histories with `grep', `lgrep'
and/or `grep-find'.

See also `grep-find-use-grep-r'."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
                                   nil nil 'grep-find-history)
             nil))
      ((not grep-find-template)
       (list nil nil
             (read-string
              "grep.el: No `grep-find-template' available. Press RET.")))
      (t (let* ((regexp (grep-read-regexp))
                (files (grep-read-files regexp))
                (dir (read-directory-name "Base directory: "
                                          nil default-directory t)))
           (list regexp files dir))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (if (null files)
        (if (not (string= regexp grep-find-command))
            (compilation-start regexp 'grep-mode))
      (setq dir (file-name-as-directory (expand-file-name dir)))
      (let ((command (grep-expand-template
                      (if grep-find-use-grep-r
                          grep-r-template
                        grep-find-template)
                      regexp
                      files
                      dir
                      nil
                      (if grep-find-use-grep-r
                          'grep-r
                        'grep-find))))
        (when command
          (let ((history (if grep-find-use-grep-r
                             'grep-history
                           'grep-find-history)))
            (if current-prefix-arg
                (setq command
                      (read-from-minibuffer "Confirm: "
                                            command nil nil history))
              (add-to-history history command)))
          (let ((default-directory dir))
            (compilation-start command 'grep-mode)
            (with-current-buffer next-error-last-buffer
              (add-hook 'compilation-finish-functions
                        'grep-r-remove-ignored-directories nil t)))
          ;; Set default-directory if we started rgrep in the *grep* buffer.
          (if (eq next-error-last-buffer (current-buffer))
              (setq default-directory dir)))))))

(defun grep-r-ignore-pattern()
  (when grep-find-ignored-directories
    ;; Fix-me: directory-sep-char is obsolete and is always ?/.
    (concat (char-to-string directory-sep-char)
            "\\(?:"
            (regexp-opt
             (mapcar #'(lambda (dir) (regexp-quote dir))
                     grep-find-ignored-directories))
            "\\)"
            (char-to-string directory-sep-char)
            ".*?:[0-9]+:"
            )))

;; Fix-me: Using compilation-filter-hook is just a little bit better
;; and a lot more trouble ...
(defun grep-r-remove-ignored-directories(buffer msg)
  (when grep-find-ignored-directories
    (with-current-buffer buffer
      (save-match-data
        (goto-char (point-min))
        (let ((inhibit-read-only t)
              (ignore-pattern (grep-r-ignore-pattern))
              (kill-whole-line t))
          (while (re-search-forward ignore-pattern nil t)
            ;; fix-me: go back 2 chars and test some prop to assure we
            ;; are at the right place
            (when (memq 'compilation-line-number
                        (get-char-property (point) 'face))
              (beginning-of-line)
              (kill-line))))))))

(defun lgrep (regexp &optional files dir)
  "Run grep, searching for REGEXP in FILES in directory DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-command'.

Collect output in a buffer.  While grep runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error]
in the grep output buffer, to go to the lines where grep found matches.

This command shares argument histories with `rgrep' and/or `grep'."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-command
                                   nil nil 'grep-history)
             nil))
      ((not grep-template)
       (list nil
             (read-string
              "grep.el: No `grep-template' available. Press RET.")))
      (t (let* ((regexp (grep-read-regexp))
                (files (grep-read-files regexp))
                (dir (read-directory-name "In directory: "
                                          nil default-directory t)))
           (list regexp files dir))))))
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (null files)
          (if (string= command grep-command)
              (setq command nil))
        (setq dir (file-name-as-directory (expand-file-name dir)))
        (setq command (grep-expand-template grep-template
                                            regexp
                                            files
                                            nil
                                            nil
                                            'grep))
        (when command
          (if (equal current-prefix-arg '(4))
              (setq command
                    (read-from-minibuffer "Confirm: "
                                          command nil nil 'grep-history))
            (add-to-history 'grep-history command))))
      (when command
        (let ((default-directory dir))
          ;; Setting process-setup-function makes exit-message-function work
          ;; even when async processes aren't supported.
          (compilation-start (if (and grep-use-null-device null-device)
                                 (concat command " " null-device)
                               command) 'grep-mode))
        (if (eq next-error-last-buffer (current-buffer))
            (setq default-directory dir))))))

;; (grep-recompute-defaults)
;; (setq grep-find-use-grep-r nil)
;; (setq grep-find-use-grep-r t)

(provide 'w32-grep)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w32-grep.el ends here

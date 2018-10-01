;;; w32shell.el --- Helpers for inferior shells on w32

;; Copyright (C) 2005, 2006, 2007 by Lennart Borgman
;;
;; Author: Lennart Borgman
;; Created: Tue Nov 22 01:07:13 2005
;; Version: 0.52
;; Last-Updated: Fri May 18 10:58:41 2007 (7200 +0200)
;; Keywords:
;; Compatibility: Emacs 22
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; On w32 you sometimes need an inferior shell like GNU bash for
;; installations and other purposes. (Note: An "inferior" process in
;; Emacs is a subprocess.) You may also need to have the Emacs bin
;; directory in path at the same time. The main purpose of this
;; package is to make this easier. There are however also some helper
;; functions for manipulating paths on w32.
;;
;; To choose shell customize group the `w32shell'.
;;
;; To run interactive shells you can use `cygwin-shell', `msys-shell'
;; and `cmd-shell'.
;;
;; Notice that you can get the paths to cygwin and MSYS automatically
;; if you use EmacsW32. From the menu bar choose Options - Customize
;; EmacsW32... and then let Emacs search for those paths.
;;
;; Put this in your .emacs:
;;
;;     (require 'w32shell)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;; 2005-12-15 More careful check of shell exec.
;; 2005-12-21 (w32shell-delayed-customize): No dialog box.
;; 2005-12-22 (w32shell-set-shell): Call cygwin-mount-activate later.
;; 2006-12-13 Added support for gnuwin32 progs that comes with EmacsW32
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers
(defun w32shell-in-exec-path (path)
  (let ((dc-exec-path (mapcar (lambda (elt)
                                (downcase elt))
                              exec-path)))
    (member (downcase path) dc-exec-path)))

(defun w32shell-add-exec-path (path &optional append)
  (unless (w32shell-in-exec-path path)
    (add-to-list 'exec-path path append)))

(defun w32shell-remove-exec-path (path)
  (unless (listp path) (setq path (list path)))
  (let ((dcpath (mapcar (lambda (elt)
                          (downcase elt))
                        path)))
    (dolist (dc dcpath)
      (setq exec-path (mapcar (lambda (elt)
                                (unless (equal dc (downcase elt))
                                  elt))
                              exec-path)))
    (setq exec-path (delete nil exec-path))))


(defun w32shell-in-envpath (path)
  (let ((envpath (replace-regexp-in-string "\\\\" "/" (getenv "PATH")))
        (norpath (replace-regexp-in-string "\\\\" "/" path))
        (case-fold-search t))
    (string-match (concat "\\(?:^\\|;\\)" (regexp-quote norpath) "\\($\\|;\\)") envpath)))

(defun w32shell-add-envpath (path &optional append)
  (unless (w32shell-in-envpath path)
    (let ((bslash-path (replace-regexp-in-string "/" "\\\\" path)))
      (if append
          (setenv "PATH" (concat (getenv "PATH") ";" bslash-path))
        (setenv "PATH" (concat bslash-path ";" (getenv "PATH")))))))

(defun w32shell-remove-envpath (path)
  (let ((paths path))
    (unless (listp paths) (setq paths (list paths)))
    (dolist (path path)
      (let ((envpath (replace-regexp-in-string "\\\\" "/" (getenv "PATH")))
            (pos (w32shell-in-envpath path)))
        (while pos
          (let* (
                 (sub1 (if (= 0 pos) "" (substring envpath 0 pos)))
                 (sub2 (substring envpath (+ pos 1 (length path))))
                 (newenvpath (replace-regexp-in-string "/" "\\\\" (concat sub1 sub2))))
            (setenv "PATH" newenvpath))
          (setq envpath (replace-regexp-in-string "\\\\" "/" (getenv "PATH")))
          (setq pos (w32shell-in-envpath path))
          )))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs itself in path
;; FIx-me: is this needed?
;; (defun w32shell-emacs-path ()
;;   (file-name-as-directory +exec-directory+))
;;   (if (string= "/" (substring +exec-directory+ -1))
;;                         (setq emacs-path (substring +exec-directory+ 0 -1))
;;                       +exec-directory+))
(defun w32shell-emacsw32-gnuwin32-bindir ()
  ;;(lwarn '(w32shell-emacsw32-gnuwin32-bindir) :warning "+exec-directory+=%s" +exec-directory+)
  (let* ((top (directory-file-name
               (file-name-directory
                (directory-file-name
                 (file-name-directory
                  (directory-file-name
                   +exec-directory+))))))
         (emacsw32 (directory-file-name
                    (expand-file-name "EmacsW32" top)))
         (gnuwin32 (directory-file-name
                    (expand-file-name "gnuwin32" emacsw32)))
         (bin (expand-file-name  "bin" gnuwin32)))
    ;;(lwarn '(w32shell-emacsw32-gnuwin32-bindir) :warning "top=%s" top)
    ;;(lwarn '(w32shell-emacsw32-gnuwin32-bindir) :warning "emacsw32=%s" emacsw32)
    ;;(lwarn '(w32shell-emacsw32-gnuwin32-bindir) :warning "bin=%s" bin)
    (when (file-directory-p bin)
      (file-name-as-directory bin))))

(defun w32shell-add-emacs (&optional append)
  "Add Emacs itself to the path of inferior shells."
  (interactive)
  (w32shell-add-envpath +exec-directory+)
  (w32shell-add-exec-path +exec-directory+))
(defun w32shell-remove-emacs ()
  "Remove Emacs itself from the path of inferior shells."
  (interactive)
  (w32shell-remove-envpath +exec-directory+)
  (w32shell-remove-exec-path +exec-directory+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Choosing a w32 shell for Emacs

(defun w32shell-delayed-customize (msg symbol)
  (let ((use-dialog-box nil)
        (is-group (get symbol 'custom-group)))
    (when (y-or-n-p (format "%s. Do you want to customize %s now? " msg symbol))
      (if is-group
          (customize-group symbol)
        (customize-option symbol)))))

(defun w32shell-verify-bindir (bin-sym shexe)
  "If BIN-SYM value is a sh bin directory name return it.
Otherwise offer the user at idle time to customize it.

Helper for `w32shell-set-shell'."
  (let ((standard-value (car (get bin-sym 'standard-value)))
        (bindir (symbol-value bin-sym))
        errmsg exefile)
    (if (equal bindir standard-value)
        (setq errmsg (concat "You must set " (symbol-name bin-sym)))
      (if (file-directory-p bindir)
          (progn
            ;;(setq exefile (concat bindir "/" shexe))
            (setq exefile (expand-file-name shexe (file-name-as-directory bindir)))
            (unless (file-executable-p exefile)
              (setq errmsg (concat "Can't find file " exefile))))
        (setq errmsg (concat "Can't find directory " bindir))))
    (if errmsg
        (let ((use-dialog-box nil))
          (lwarn '(w32shell) :warning errmsg)
          (unless (eq major-mode 'custom-mode)
            (with-timeout (6 (progn
                               (lwarn '(w32shell) :warning "Ok, please customize w32shell later!")
                               (message "Time out, continuing")))
              (w32shell-delayed-customize errmsg 'w32shell)))
          nil)
      bindir)))

(defvar w32shell-current-shell-path nil)

(defcustom w32shell-wanted-progs
  '("grep" "find" "xargs" "cmp" "diff" "diff3" "cmp" "patch" "locate")
  "List of programs that are checked for availability.
This list of programs are searched for in your path by
`executable-find' when calling `w32shell-set-shell'.  If any of
them is not found a warning is given."
  :type '(repeat string)
  :group 'w32shell)

;; Fix-me: write a function that checks consistency against this!
(defun w32shell-set-shell (shellname)
  "Set shell to use for inferior shells.
This sets `shell-file-name' and the environment variable SHELL.
It also changes the environment variable PATH and `exec-path'.

Accepted values for SHELLNAME are \"cmd\", \"cygwin\" and
\"msys\".

If SHELLNAME is \"cygwin\" then it calls `cygwin-mount-activate'.

If SHELLNAME is \"cygwin\" or \"msys\" then the corresponding bin
directory is added to path.

This function checks if the programs in `w32shell-wanted-progs'
that may be used from the inferior shells are available.  If they
are not a warning will be given. It also checks if 'find' is the
unix style find or not.

Returns non-nil if success."
  (interactive
   (list
    (let* ( (history '("cmd" "msys" "cygwin"))
            (history-length (length history)) )
      (completing-read "Choose shell: " '("cmd" "msys" "cygwin") nil t "cygwin" 'history))))
  ;;(lwarn '(w32shell) :warning "Calling w32shell-set-shell %s" shellname)
  (let (bin shell)
    (cond (  (equal shellname "cygwin")
             (setq bin (w32shell-verify-bindir 'w32shell-cygwin-bin "bash.exe"))
             (when bin
               (unless (file-directory-p bin) (error "Can't find directory %s" bin))
               (let ((usr-bin (expand-file-name "../usr/bin" bin))
                     (usr-local-bin (expand-file-name "../usr/local/bin" bin)))
                 (setq bin (expand-file-name "../bin" bin))
                 (setq bin (list bin usr-bin usr-local-bin)))
               ;; in cygwin use "cygpath -a -w /bin" and "echo $PATH" to check.
               ;;
               ;; c:/cygwin/bin shows up in cygwin as /usr/bin,looks
               ;; like a cygwin bug.  I have reported it, but cygwin
               ;; maintainers does not seem to think it is important
               ;; (and I can agree).
               (setq shell "bash")
               (setenv "PS1" "Cygwin \\w > "))
             )
          (  (equal shellname "msys")
             (setq bin (w32shell-verify-bindir 'w32shell-msys-bin "sh.exe"))
             (when bin
               (setq shell "sh")
               (setenv "PS1" "MSYS \\w > "))
             )
          (  (equal shellname "cmd")
             (setq bin (w32shell-emacsw32-gnuwin32-bindir))
             ;;(lwarn '(w32shell) :warning "cmd cond, bin=%s" bin)
             (setq shell (expand-file-name "cmdproxy.exe" +exec-directory+))
             )
          (  t
             (error "Unrecognized shell name: %s" shellname)))
    (when (or bin shell)
      ;; (when bin
      ;;   (unless (listp bin) (setq bin (cons bin nil)))
      ;;   (dolist (b bin)
      ;;     (unless (file-directory-p b)
      ;;       (error "Can't find directory %s" b))))
      (when w32shell-current-shell-path
        (w32shell-remove-exec-path w32shell-current-shell-path)
        (w32shell-remove-envpath   w32shell-current-shell-path)
        (setq w32shell-current-shell-path nil))
      (cond ( (equal shellname "cmd")
              (setq process-coding-system-alist nil)
              (setq w32-process-args nil)
              (remove-hook 'comint-output-filter-functions
                           'comint-strip-ctrl-m)
              )
            ( (or (equal shellname "cygwin") (equal shellname "msys"))
              ;;(setq process-coding-system-alist '((shell-file-name . undecided-unix)))
              ;;(setq process-coding-system-alist (list (cons shell-file-name 'undecided-unix)))
              (setq w32-process-args ?\")
              ;; For Java?:
              (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
              )
            )
      (when shell
        ;;(lwarn '(w32shell) :warning "bin=%s, shell-name=%s" bin shellname)
        (when bin
          (unless (listp bin) (setq bin (list bin)))
          (dolist (b bin)
            (w32shell-add-exec-path b)
            (w32shell-add-envpath b))
          ;;(message "exec-path=%s" exec-path)
          ;;(message "evn PATH=%s" (getenv "PATH"))
          )
        (setq w32shell-current-shell-path bin)
        ;; Call cygwin-mount. After an idea by Ismael Valladolid Torres:
        (when (equal shellname "cygwin")
          (when (require 'cygwin-mount nil t)
            (cygwin-mount-activate)))
        (setq shell-file-name shell) ; Single shell
        ;;(setenv "SHELL" shell-file-name) ; Single shell
        ;;(setq explicit-shell-file-name shell-file-name)
        ;; Some sanity checks:
        (w32shell-check-wanted-progs)
        ))
    bin))

(defun w32shell-get-missing-progs ()
  (let ((missing))
    (dolist (prog w32shell-wanted-progs)
      (unless (executable-find prog)
        (add-to-list 'missing prog)))
    missing))

(defun w32shell-find-is-unix-find ()
  (let ((find-prog (executable-find "find"))
        (findstr-prog (executable-find "findstr")))
    (not (string= (file-name-directory find-prog)
                  (file-name-directory findstr-prog)))))

(defun w32shell-check-wanted-progs ()
  "Checks if `w32shell-wanted-progs' are available.
This depends on `w32shell'."
  (interactive)
  (dolist (prog (w32shell-get-missing-progs))
    (lwarn '(w32shell) :warning
           (concat "When using '" shellname "' program '" prog "' can't be found")))
  (unless (w32shell-find-is-unix-find)
    (lwarn '(w32shell) :warning
           (concat "When using '" shellname "' program 'find'"
                   " will be Windows' find, should be unix' find"))))

(defun w32shell-quote-argument (argument)
  "Like `shell-quote-argument' but knows about w32shell."
  (unless (eq system-type 'windows-nt) (error "You can only use this on w32"))
  (let ((system-type (if (string= "cmd" shell-file-name)
                         system-type
                       'gnu/linux)))
      (shell-quote-argument argument)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom

(defgroup w32shell nil
  "Customization group for w32shell"
  :group 'w32)

(defcustom w32shell-cygwin-bin ""
  "Path to Cygwin bin directory.
Note that you can set this automatically from the menus through
Options - Customize EmacsW32 if you are using EmacsW32."
  :type 'directory
  :group 'w32shell)

(defcustom w32shell-msys-bin ""
  "Path to MSYS bin directory.
Note that you can set this automatically from the menus through
Options - Customize EmacsW32 if you are using EmacsW32."
  :type 'directory
  :group 'w32shell)

(defvar w32shell-old nil)
(defcustom w32shell-shell nil
  "Shell to use for `shell' command.
Value should be 'cmd, 'cygwin or 'msys.  If it is cygwin or msys
those utilities bin path are put first in path.

Setting is done with `w32shell-set-shell'."
  ;; Make sure emacsw32 is loaded:
  ;;:set-after '(emacsw32-style-frame-title)
  :type '(choice
          (const :tag "(unset)" nil)
          (const :tag "Windows cmd.exe - uses unix progs from EmacsW32" cmd)
          (const :tag "Cygwin" cygwin)
          (const :tag "MSYS" msys)
          )
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (unless (eq w32shell-old value)
             (setq w32shell-old value)
             (w32shell-set-shell (format "%s" value))
             ))
         t)
  :group 'w32shell)

(defcustom w32shell-add-emacs-to-path t
  "Add Emacs bin directory to path when non-nil."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (unless (w32shell-in-exec-path +exec-directory+)
               (w32shell-add-emacs))
           (w32shell-remove-emacs)))
  :group 'w32shell)

(defvar w32shell-with-shell-internal nil)

(defmacro w32shell-with-shell (use-shell &rest body)
  "Execute the BODY forms with shell temporary set to USE-SHELL."
  (declare (indent 0) (debug t))
  `(let ((shell-file-name)
         (exec-path exec-path)
         (envpath (getenv "PATH")))
     (condition-case err
         (when (w32shell-set-shell ,use-shell)
           ,@body)
       (error (message "%s" (error-message-string err))))
     (setenv "PATH" envpath)))


(defun cygwin-shell ()
  "Run `shell' with Cygwin as the shell.
Does not affect the setting of `w32shell-shell' but otherwise
works as if you had set this to 'cygwin.

See also `msys-shell' and `cmd-shell'."
  "Run `shell' with MSYS as the shell.
Is otherwise similar to `cygwin-shell'."
  (interactive)
  (w32shell-with-shell "cygwin" (shell "*cygwin shell*")))

(defun msys-shell ()
  "Run `shell' with MSYS as the shell.
Is otherwise similar to `cygwin-shell'."
  (interactive)
  (w32shell-with-shell "msys" (shell "*msys shell*")))

(defun cmd-shell ()
  "Run `shell' with Windows Command Prompt as the shell.
File name completion with Tab/Shift-Tab is done in the style that
Windows Command Prompt does it.

Is otherwise similar to `cygwin-shell'."
  (interactive)
  (w32shell-with-shell
    "cmd"
    (progn
      (shell "*cmd shell*")
      ;; fix-me: Temporary, until removed from viper
      ;; (when (and (boundp 'viper-insert-basic-map)
      ;;            (keymapp viper-insert-basic-map))
      ;;   (define-key viper-insert-basic-map
      ;;     (if viper-xemacs-p [(shift tab)] [S-tab]) nil))
      (local-set-key [tab] 'w32shell-dynamic-complete-filename-like-cmd-fw)
      (local-set-key [(shift tab)]
                     'w32shell-dynamic-complete-filename-like-cmd-bw))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun w32shell-dynamic-complete-filename-like-cmd-fw ()
  "Tab style file name completion like cmd.exe.
Tries to do Tab style file name completion like cmd.exe on w32
does it.

See also `w32shell-dynamic-complete-filename-like-cmd-bw'."
  (interactive)
  (w32shell-dynamic-complete-filename-like-cmd t))

(defun w32shell-dynamic-complete-filename-like-cmd-bw ()
  "Shift-Tab style file name completion like cmd.exe.
Tries to do Shift-Tab style file name completion like cmd.exe on
w32 does it.

See also `w32shell-dynamic-complete-filename-like-cmd-fw'."
  (interactive)
  (w32shell-dynamic-complete-filename-like-cmd nil))

(defconst w32shell-dynamic-complete-state nil)

(defcustom w32shell-dynamic-complete-sync-dirs t
  "Synchronize process directory and `default-directory' if non-nil.
If non-nil then `w32shell-dynamic-complete-filename-like-cmd-fw' (and
dito -bw) will send an invisible \"cd\" to the process running
cmd.exe to find out what directory the cmd.exe process
uses. `default-directory' is then set to this directory."
  :type 'boolean
  :group 'w32shell)

(defcustom w32shell-dynamic-complete-only-dirs '("cd" "pushd")
  "Commands for which only directories should be shown.
When doing file name completion the commands in this list will
only get directory names.

This is used in `w32shell-dynamic-complete-filename-like-cmd-fw' (and
dito -bw)."
  :type '(repeat string)
  :group 'w32shell)

(defun w32shell-dynamic-complete-filename-like-cmd (forward)
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (process-mark proc))
         (point (point))
         (cmdstr (buffer-substring-no-properties pmark point))
         (argv (w32shell-get-argv cmdstr))
         (first-arg (car argv))
         (last-arg (car (reverse argv)))
         (only-dirs (member (car first-arg) w32shell-dynamic-complete-only-dirs))
         (prev-cmdstr          (nth 0 w32shell-dynamic-complete-state))
         (prev-completion      (nth 1 w32shell-dynamic-complete-state))
         (prev-begin-filename  (nth 2 w32shell-dynamic-complete-state))
         (in-completion (string= cmdstr prev-cmdstr))
         (begin-filename prev-begin-filename)
         new-completion
         new-full-completion
         completion-dir
         completion-dir-given
         dir-files
         pick-next
         beginning-last
         )
    ;; Initialize
    (setq w32shell-dynamic-complete-state nil)
    (when last-arg
      (setq completion-dir-given (file-name-directory (car last-arg))))
    (if completion-dir-given
        (setq completion-dir-given
              (file-name-as-directory completion-dir-given))
      (setq completion-dir-given ""))
    ;; Not continuing completion set up for completion
    (unless in-completion
      (setq prev-completion nil)
      (if last-arg
          (setq begin-filename
                (concat "^" (file-name-nondirectory (car last-arg))))
        (setq begin-filename nil))
      ;; Sync process directory and default-directory
      (when w32shell-dynamic-complete-sync-dirs
        (let ((old-out-filter (process-filter proc)))
          (condition-case err
              (progn
                (set-process-filter
                 proc
                 (lambda (proc str)
                   (let ((lstr (split-string str "[\r\n]+")))
                     (setq default-directory
                           (file-name-as-directory (nth 1 lstr))))))
                (process-send-string proc "cd\n")
                (accept-process-output proc))
            (error (message "%s" (error-message-string err))))
          (set-process-filter proc old-out-filter))))
    ;; Find completion
    (setq completion-dir (expand-file-name completion-dir-given))
    (setq dir-files (directory-files completion-dir nil begin-filename))
    (unless forward
      (setq dir-files (reverse dir-files)))
    (dolist (f dir-files)
      (when (and (not (member f '("." "..")))
                 (or (not only-dirs)
                     (file-directory-p (expand-file-name f completion-dir))))
        (unless new-completion
          (setq new-completion f))
        (if (string= f prev-completion)
            (setq pick-next t)
          (when pick-next
            (setq pick-next nil)
            (setq new-completion f)))))
    (setq new-full-completion
          (convert-standard-filename
           (concat completion-dir-given new-completion)))
    ;; Replase last argument
    (setq beginning-last (nth 1 last-arg))
    (unless beginning-last
      (setq beginning-last 0))
    (goto-char (+ pmark beginning-last))
    (unless (eolp) (kill-line))
    ;; The code below should probably use shell-quote-argument, but
    ;; because of trouble with this function I am using a more
    ;; w32 specific quoting here at the moment.
    (if (not (memq ?\  (append new-full-completion nil)))
        (insert new-full-completion)
      (insert ?\")
      (insert new-full-completion)
      (insert ?\"))
    ;; Save completion state
    ;;
    ;; return non-nil to show completion has been done!
    (setq w32shell-dynamic-complete-state
          (list
           (buffer-substring-no-properties pmark (point))
           new-completion
           begin-filename))))

(defun w32shell-get-argv (cmdline)
  "Split CMDLINE into args.
The splitting is done using the syntax used on MS Windows.

Return a list where each element is a list in the form

  \(arg arg-begin arg-end)

where ARG is the argument stripped from any \". ARG-BEGIN and
ARG-END are the beginning and end of the argument in cmdline.

If CMDLINE ends with a space or is \"\" a list consisting of
\(\"\" LEN nil) is added. LEN is the length of CMDLINE."
  (let ((lcmd (append cmdline nil))
        (len (length cmdline))
        argv
        state
        arg
        arg-begin
        arg-end
        c
        )
    (while lcmd
      (setq c (car lcmd))
      (setq lcmd (cdr lcmd))
      (cond
       (  (not state)
          (when arg (error "arg not nil"))
          (cond
           ( (= c ?\ ))
           ( (= c ?\")
             (setq arg-begin (- len 1 (length lcmd)))
             (setq state 'state-qarg))
           ( t
             (setq arg-begin (- len 1 (length lcmd)))
             (setq state 'state-arg)
             (setq arg (cons c arg)))))
       (  (eq state 'state-arg)
          (cond
           ( (= c ?\ )
             (setq state nil)
             (setq arg-end (- len 1 (length lcmd)))
             (setq argv (cons
                         (list (concat (nreverse arg))
                               arg-begin
                               arg-end)
                         argv))
             (setq arg nil))
           ( (= c ?\")
             (setq state 'state-arg-q))
           ( t
             (setq arg (cons c arg)))))
       (  (eq state 'state-arg-q)
          (cond
           ( (= c ?\")
             (setq state 'state-arg))
           ( t
             (setq arg (cons c arg)))))
       (  (eq state 'state-qarg)
          (cond
           ( (= c ?\")
             (setq state 'state-qarg-q))
           ( t
             (setq arg (cons c arg)))))
       (  (eq state 'state-qarg-q)
          (cond
           ( (= c ?\ )
             (setq state nil)
             (setq arg-end (- len 1 (length lcmd)))
             (setq argv (cons
                         (list (concat (nreverse arg))
                               arg-begin
                               arg-end)
                         argv))
             (setq arg nil))
           ( (= c ?\")
             (setq arg (cons c arg))
             (setq state 'state-qarg))
           ( t
             (setq arg (cons c arg)))))
       (  t
          (error "unknown state=%s" state))
       ))
    (if arg
        (progn
          (setq arg-end (- len 0 (length lcmd)))
          (setq argv (cons
                      (list
                       (concat (nreverse arg))
                       arg-begin
                       arg-end)
                      argv)))
      (when (or (not c) (= c ?\ ))
        (setq argv (cons (list "" (length cmdstr) nil) argv))))
    (reverse argv)))

;; For testing:
(when nil
  (global-set-key [f9]         'w32shell-dynamic-complete-filename-like-cmd-fw)
  (global-set-key [(shift f9)] 'w32shell-dynamic-complete-filename-like-cmd-bw)
  )

(when nil
  (let* ((cmd "cd \\\"hej\"\\du \"sista\"")
         (argv (w32shell-get-argv cmd)))
    (dolist (a argv)
      (message "%s %s %s (%s)"
               (nth 0 a)
               (nth 1 a)
               (nth 2 a)
               (substring cmd (nth 1 a) (nth 2 a)))))
  )

;;(w32-shell-execute nil (concat (getenv "SystemRoot") "\\explorer.exe") "/n,/select,c:\\test\\temp.htm")
(defun w32shell-explorer-file (file)
  "Open Windows Explorer with file FILE selected."
  (interactive "fFile to focus in Explorer: ")
  (let ((full-file (expand-file-name file)))
    (setq full-file (replace-regexp-in-string "/" "\\" full-file t t))
    (w32-shell-execute nil (concat (getenv "SystemRoot") "\\explorer.exe")
                       (concat "/n,/select," full-file))))

(defun w32shell-explorer-current-file ()
  "Open Windows Explorer with current file selected."
  (interactive)
  (if buffer-file-name
      (w32shell-explorer-file buffer-file-name)
    (message "Buffer has no file name")))

(defun w32shell-explorer-old (dir)
  "Open Windows Explorer in directory DIR.
For some reason with this function Explorer does not get
focus. Use the new version instead."
  (interactive "DStart in directory: ")
  (setq dir (expand-file-name dir))
  (w32-shell-execute nil dir))

(defun w32shell-explorer (dir)
  "Open Windows Explorer in directory DIR."
  (interactive "DStart in directory: ")
  (setq dir (expand-file-name dir))
  ;;(setq dir (directory-file-name dir))
  (message "dir=%s" dir) (sit-for 2)
  (w32-shell-execute
   "explore" ;;nil
   "" ;(concat (getenv "SystemRoot") "\\explorer.exe")
   (concat "/n," dir)
   ))

(defun w32shell-explorer-here ()
  "Open Windows Explorer in current directory."
  (interactive)
  (w32shell-explorer default-directory))

(defun w32shell-cmd (dir)
  "Open a Windows command prompt in directory DIR.
Emacs bin dir is added to path in the started command window."
  (interactive "DStart in directory: ")
  (let ((default-directory (expand-file-name dir))
        (old-emacs-dir (getenv "emacs_dir"))
        (old-path (getenv "PATH")))
    (setenv "emacs_dir"
            (save-match-data
              (replace-regexp-in-string "/" "\\" old-emacs-dir t t)))
    (w32shell-add-envpath +exec-directory+)
    (unwind-protect
        (condition-case err
            (progn
              ;;(call-process "cmd.exe" nil 0 nil "/c" "start" (concat '(?\") "hej4" '(?\")) "cmd.exe")
              ;; Bug in call-process quoting, use this instead this:
              ;;(w32-shell-execute nil "cmd.exe" "/c start \"Command Prompt with Emacs in PATH\"")
              ;; Nope, that will not give the correct path ... - turn off quoting is spawnve instead:
              (let ((w32-quote-process-args nil))
                (call-process "cmd.exe" nil 0 nil "/c" "start"
                              (concat '(?\") "Command Prompt with Emacs in PATH" '(?\")) "cmd.exe")))
          (error (message "%s" (error-message-string err))))
      (setenv "emacs_dir" old-emacs-dir)
      (setenv "PATH" old-path))))

(defun w32shell-cmd-here ()
  "Open a Windows command prompt in current directory.
Emacs bin dir is added to path in the started command window."
  (interactive)
  (w32shell-cmd default-directory))

(provide 'w32shell)

;;; w32shell.el ends here

;;; cmd-mode.el --- Edit of MS Windows cmd and bat files ;; -*- coding: sjis-dos -*-
;;
;; Copyright (C) 2001-2005 by Tadamegu Furukawa. <tfuruka1 at nifty dot com>
;;
;; Author: Tadamegu Furukawa <tfuruka1 at nifty dot com>
;; Maintainer: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2001-08-28
;; Lxast-Updated: Thu May 10 18:43:53 2007 (7200 +0200)
;; Version: 1.42
;; Keywords:
;;
;; Fxeatures that might be required by this library:
;;
;;   `font-lock', `syntax'.
;;
;; Kindly translated to English 2005 by Kenichi Handa after an inquiry
;; by Lennart Borgman.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of GNU Emacs.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; MS-DOS�̃o�b�`�t�@�C���y��Windows NT�̃R�}���h�X�N���v�g��ҏW����
;; �ׂ̂ւȂ��傱���[�h�ł��B%����R�����āA�����Ȃ񂾂��킩��Ȃ��o�b
;; �`�t�@�C����ǂ݈Ղ�����ړI�ō쐬�����̂ŁA��{�I��font-lock�ȊO��
;; �@�\�͖w�ǂ���܂���B������emacs 20.7 "Meadow-1.14 (AWSAKA:62)" ��
;; ���������Ă��܂���(����ȊO�Ŏg�p����鎖��z�肵�Ă��܂���)�B
;;
;; Tiny mode for editing batch files of MS-DOS and command scripts of
;; Windows NT.  The purpose is to improve the readablity of those
;; files that contains many `%' and are difficult to read.  So,
;; basically this provides only a proper font-lock setting.  This
;; program has been tested for Emacs 20.7 "Meadow-1.14 (AWSAKA:62)
;; (the other versions of Emacsen are out of my focus).
;;
;; It has now been tested also on Emacs 21.3 and 22.0.50 on W2k.



;; .emacs�̉������Ɉȉ���ǉ����Ă��������B�g���q��cmd����bat�̃t�@�C
;; �����J���Ǝ�����cmd-mode�ɂȂ�܂��B
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Usage:
;;
;; Please input the following code somewhere in your .emacs.  Then,
;; cmd-mode is automatically activated when you open a file whose
;; extention is "cmd" or "bat".

;;    (autoload 'cmd-mode "cmd-mode" "CMD mode." t)
;;    (setq auto-mode-alist (append '(("\\.\\(cmd\\|bat\\)$" . cmd-mode))
;;                                  auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;;;;;;;;;;;;;;;;; What is this:
;; $Id: cmd-mode.el,v 1.1 2005/07/18 14:38:50 Administrator Exp $
;; $Log: cmd-mode.el,v $
;; Revision 1.1  2005/07/18 14:38:50  Administrator
;; *** empty log message ***
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Revision 1.42  2007/05/09 (Lennart Borgman)
;;
;; - Added comment support
;; - Using compilation buffer for cmd-exe
;; - Added heuristics to go to error from compilation buffer
;;
;; The first two was after a tip from EmacsWiki, but I rewrote that
;; part quite a bit.

;; Revision 1.41  2005/10/18
;;
;; - Minor bugs corrected (Lennart Borgman)

;; Revision 1.4  2005/10/18
;;
;; o Translation to English. (Kenichi Handa)
;; o Changed set-mark to push-mark. (Lennart Borgman)


;; Revision 1.3  2001/09/11 12:39:03  tfuruka1
;;
;; �����j���[�����Ă݂��B
;; o Add menu.
;;
;; ���C�ӂ̃��x���փW�����v����@�\��ǉ��B
;; o Add a facility to jump to any label.


;; Revision 1.2  2001/08/31 13:25:50  tfuruka1
;;
;; ��SET�̊��ϐ��ɃL�[���[�h�����񂪊܂܂�Ă���ƁA�F�t�����ςɂȂ���
;;   ���C��
;; o Fix the problme of incorrect coloring in the case that an
;;   environment varialbe of SET contains a keyword string.
;;
;; ��SET /A �̎��ɉ��Z�q�̈ꕔ�����ϐ��Ƃ��āA�ԈႦ�ĐF�t���������
;;   �C��
;; o FIx the problem of incorrectly coloring a part of operand in SET
;;   /A as an environment variable in

;; Revision 1.1  2001/08/28 13:14:43  tfuruka1
;;
;; o Initial version.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:


;; Added by L Borgman:
(require 'font-lock)

;; (replace-regexp "[\t ]+$" "")

(defconst cmd-mode-revision-number "1.4" "cmd-mode.el version number.")

;;; *** ���j���[�o�[
;;; *** menu bar
(defvar cmd-menu-bar (make-sparse-keymap "cmd-mode-menu")
  ;; "���j���["
  "Menu bar entry.")
(defvar cmd-submenu-jump (make-sparse-keymap "cmd-mode-submenu-jump")
  ;; "�|�b�v�A�b�v���j���[�i�W�����v�j"
  "Popup menu for jump.")

;;; *** hook
(defvar cmd-mode-hook nil
  ;; "cmd-mode���쐬����^�C�~���O�ŌĂ΂�� hook �ł��B"
  "Hook called when `cmd-mode' is entered.")
(defvar cmd-help-mode-hook nil
  ;; "cmd-help-mode���쐬����^�C�~���O�ŌĂ΂�� hook �ł��B"
  "Hook called when `cmd-help-mode' is entered.")

;;; *** �O���R�}���h
;;; *** External command
(defvar cmd-help-command "help"
  ;; "help�R�}���h�̃R�}���h���BWindows NT�n�ȊO�ɂ͑��݂��܂���B"
  "Name of `help' command.  It exists only on Windows NT.")

;;; *** buffer�֌W
;;; *** Buffer related things.
(defvar cmd-temp-buf " *cmd-temp-buf*"
  ;; "��Ɨp�̉B���o�b�t�@"
  "Hidden buffer for work.")
(defvar cmd-help-buf-base-name "*cmd-help "
  ;; "�w���v�o�b�t�@�̃x�[�X��"
  "Base name of help buffer.")

;;; *** regexp
(defvar cmd-full-size-space-etc-pattern "\\(�@+\\)"
  ;; "�����u�S�p�̋󔒁v�̐��K�\��"
  "Regular expression for fullwidth space sequence.")
(defvar cmd-tab-pattern "\\(\t+\\)"
  ;; "TAB�����̐��K�\��"
  "Regular expression for TAB character sequence.")
(defvar cmd-comment-pattern "^[ \t]*\\(@*\\)\\(rem\\)\\([\t ].*$\\|$\\)"
  ;; "�R�����g�s�̐��K�\��"
  "Regular expression for a comment line.")
(defvar cmd-variable-pattern
  (concat "\\("
          "%\\("                     ;�ŏ��̕�����%  the first char is `%'
          "\\*"                      ;%* �̏ꍇ	in case of "%*"
          "\\|"
          "~[^0-9 \n]+[0-9a-z]"      ;~f1 ��		e.g. ~f1
          "\\|"
          "[0-9a-z]\\b"              ;%1 ��		e.g. %1
          "\\|"
          "[^%\n �@-�]+%"           ;�ʏ�̊��ϐ�	normal env. var.
          "\\|"
          "%[0-9a-z]"               ;%% ��		e.g. %$
          "\\)"
          "\\)")
;;   "���ϐ��̓W�J�̐��K�\��

;; �z�肵�Ă���p�^�[���͈ȉ��̒ʂ�ł��B

;; ��            �Ӗ�
;; -----------   -----------------------
;; %*            ���ׂĂ̈������Q�Ƃ���
;; %~fi, %~dp1   �o�b�`�p�����[�^(%n)�y��FOR�ϐ��Q�Ƃ̒u��
;; %������%      �ʏ�̊��ϐ��̎Q��
;; %1, %i        �o�b�`�p�����[�^(%n)�y��FOR�ϐ��Q��
;; %%, %%1, %%i  %���̂��́A�o�b�`�p�����[�^(%n)�y��FOR�ϐ��Q�Ɓi�o�b�`
;;               �t�@�C�����Ŏg�p�����ꍇ�j"
  "Regular expression for expanding an environment variable.

The following patterns are concerned.

example		meaning
-------		-------
%*		refer to all of the arguments
%~f1, %~dp1	replacement for batch parameter (%n) and FOR variable reference
%STRING%	normal reference of an environment variable
%1, %i		batch parameter (%n) and FOR variable reference
%%, %%1, %%i	% itself, batch parameter (%n) and FOR variable reference
		(when used in a batch file)"
)
(defvar cmd-const-pattern
  "\\(^[ \t]*@\\|nul\\|:eof\\|&&\\||\\|\\^\\|&[12]?\\|[,;]\\)"
  ;; "�����t�������L�����̐��K�\��"
  "Regular expression for conditional symbols.")
(defvar cmd-set-pattern
  (concat "\\b"
          "\\(set\\b\\)\\([ \t]+/a\\)*"
          "\\(\\([ \t]+[_A-Za-z-][_A-Za-z0-9-]*\\)*\\)"
          "\\([-+/\\*]\\|\\W\\)*")
  ;; "SET�R�}���h�̐��K�\��"
  "Regular expression for SET command.")
(defvar cmd-label-pattern "^[ \t]*\\(:[:A-Za-z0-9_-]+\\)"
  ;; "���x���̐��K�\��"
  "Regular expression for a label.")
(defvar cmd-redirect-pattern
  (concat "\\("
          "[12]?>>?"                 ;1> 2> 1>> 2>> > >>
          "\\|"
          "<[12]?"                      ;<1 <2 <
          "\\|"
          "2>&1"
          "\\|"
          "1>&2"
          "\\)")
  ;; "���_�C���N�g�̐��K�\��"
  "Regular expression for redirection.")
(defvar cmd-option-pattern
  (concat "\\b"
          (regexp-opt
           '(
             ;; if
             "not" "exist" "defined" "errorlevel" "cmdextversion"
             "equ" "neq" "lss" "leq" "gtr" "geq"
             ;; for
             "eol" "skip" "delims" "tokens" "in" "do") t) "\\b")
  ;; "IF�����̃I�v�V�����̐��K�\��"
  "Regular expression for options to a statement like IF.")
(defvar cmd-command-pattern
  (concat "\\b"
          (regexp-opt '("assoc" "break" "call" "cd" "chdir" "cls"
                        "color" "copy" "date" "del" "dir" "echo"
                        "echo." "endlocal" "erase" "exit" "for"
                        "ftype" "goto" "if" "md" "mkdir" "move" "path"
                        "pause" "popd" "prompt" "pushd" "rd" "ren"
                        "rename" "rmdir" "setlocal" "shift" "start"
                        "time" "title" "type" "ver" "verify" "vol" )
                      t) "\\b")
;;   "�R�}���h�̐��K�\��

;; �R�}���h�͓����R�}���h�݂̂ł��BWindows NT��HELP�R�}���h�ňꗗ�\������
;; ��R�}���h����A�O���R�}���h(*.exe, *.com)�����������̂ł��B�A���ASET
;; �� REM �͑��Œ�`���Ă��܂��̂ŁA�����ł͏����Ă��܂��B�܂��AECHO ��
;; ECHO. ���܂߂Ă��܂�"

  "Regular expression for internal commands.

Only internal commands.  Actually they are commands listed by
HELP command of Windows NT (such external commands as *.ext and
*.com are excluded).  SET and REM are also excluded because they
are defined in the other place.  As for ECHO, ECHO.  is included
too.")

;; font-lock�̐ݒ�
;; Setting for font-lock.
(defvar cmd-font-lock-keywords
  (list
   ;; �R�����g
   ;; command
   (list cmd-comment-pattern
         '(1 font-lock-constant-face)   ;�s����@	@ at bol
         '(2 font-lock-keyword-face)    ;rem
         '(3 font-lock-comment-face)    ;�R�����g����	command character
         )
   ;; SET
   (list cmd-set-pattern
         '(1 font-lock-keyword-face)    ;SET
         '(3 font-lock-type-face)       ;���ϐ���	name of env. var.
         )
   ;; ���x��
   ;; label
   (list cmd-label-pattern
         '(1 (cons font-lock-function-name-face '(underline))))
   ;; ���_�C���N�g�L��
   ;; redirect symbol
   (list cmd-redirect-pattern 1 font-lock-warning-face)
   ;; ���ϐ��̎Q��
   ;; reference of environment variable
   (list cmd-variable-pattern 1 font-lock-variable-name-face)
   ;; �����R�}���h
   ;; internal command
   (list cmd-command-pattern 1 font-lock-keyword-face)
   ;;�����t�������L����
   ;; e.g. conditional symbols
   (list cmd-const-pattern 1 font-lock-constant-face)
   ;; IF����FOR���̃I�v�V������
   ;; e.g. options of IF and FOR
   (list cmd-option-pattern 1 font-lock-builtin-face)
   ;; �S�p�X�y�[�X
   ;; fullwidth space
   (list cmd-full-size-space-etc-pattern '(1 '(underline)))
   ;; TAB����
   ;; TAB character
   (list cmd-tab-pattern '(1 '(highlight)))
   )
;;   "cmd-mode, cmd-help-mode�Ŏg�p����font-lock�̐ݒ�ł��B
;; �ڍׂ�font-lock-defaults���Q�Ƃ��Ă��������B"
  "Setting for font-lock used in `cmd-mode' and `cmd-help-mode'.
See `font-lock-defaults' for detail.")

(defun cmd-mode-version ()
  ;; "cmd-mode��Version�\��"
  "Show the version of `cmd-mode'."
  (interactive)
  (message
   (concat "cmd-mode version " cmd-mode-revision-number)))

(defun cmd-help (arg)
;;   "help�R�}���h�����s���āA���ʂ�\�����܂��B
;; ���̊֐��́AWindows NT�n�ȊO�ł́A���삵�܂���B

;; ��Windows \\(9[58]\\|Me\\)�ł����삷��悤�ɍ�������肾������ł����A
;; �����ɓ��삵�܂���B"
  "Execute help command and show the result.
This functinos works only on Windows NT.

Implementers note: I intended that this work also on Windows
\\(9[58]\\|Me\\), but actually failed.
Argument ARG is the command to describe."
  (interactive
   (list (let* ((command (current-word))
                (input (read-string
                        (format "Help for command%s: " ;; "�R�}���h%s: "
                                (if (string-equal command "")
                                    ""
                                  (format " (%s)" command))))))
                (if (string-equal input "")
                    (if (string-equal command "")
                        "help"
                      command)
                  input))))

  (let* ((case-fold-search t)
        (cmd-help-buffer (format "%s%s*"
                                 cmd-help-buf-base-name (downcase arg)))
        (comspec (getenv "ComSpec"))
        (cmd-help-arg
         (cond
          ((string-match "cmd\\.exe$" comspec)
           (list "\\/c" cmd-help-command
                 (if (string-equal arg "help") "" arg)))
          ((string-match "command\\.com$" comspec)
           (if (string-equal arg "help")
               (error "Insert command" ;; "�R�}���h����͂��Ă�������"
		      )
             (list "\\/c" arg "\\/?")))
          (t
           (error (concat "Work only on WindowsXX ComSpec="
			  ;;"WindowsXX�ȊO�ł͓��삵�܂��� ComSpec="
			  comspec)))))
        )
    (set-buffer (get-buffer-create cmd-help-buffer))
    (setq buffer-read-only nil)
    (erase-buffer)
    (apply (function call-process) comspec nil t nil cmd-help-arg)
    (goto-char (point-min))
    (display-buffer cmd-help-buffer)
    (cmd-help-mode)
    ))

(defun cmd-help-mode-exit ()
  ;; "cmd-help-mode���I�����܂��B"
  "Terminate `cmd-help-mode'."
  (interactive)
  (if (eq major-mode 'cmd-help-mode)
      (kill-buffer (current-buffer)))
  )

(defvar cmd-help-local-map nil)
(defun cmd-help-mode ()
;;   "cmd-help����Ăяo����܂��B

;; ���w���v�R�}���h�����s���A���ʂ�\�����܂��B
;;   \\[cmd-help]

;; ��cmd-help-mode���I�����܂��B
;;   \\[cmd-help-mode-exit]"

  "Mode for `cmd-help'.

o Execute help command and show the result.
  \\[cmd-help]

o Terminate `cmd-help-mode'.
  \\[cmd-help-mode-exit]"
  (interactive)
  (kill-all-local-variables)
  ;;���[�h���̐ݒ�ƁA���[�h���C���̃��[�h���t�B�[���h�̐ݒ�
  ;; Setting the mode name and the mode-name field of the mode-line.
  (setq major-mode 'cmd-help-mode
        mode-name "cmd-help"
        buffer-auto-save-file-name nil  ;�����ۑ����Ȃ�	suppresss auto saving
        buffer-read-only t              ;�ǂݍ��ݐ�p	for read only
        )
  ;; �L�[�}�b�v�̐ݒ�
  ;; Setting keymap.
  (setq cmd-help-local-map (make-keymap))
  ;; �L�[�̊��蓖��
  ;; Assigning keys
  (define-key cmd-help-local-map "\C-c\C-h" 'cmd-help)
  ;;(define-key cmd-help-local-map "\C-m" 'cmd-help)
  (define-key cmd-help-local-map "?" 'cmd-help)
  (define-key cmd-help-local-map "q" 'cmd-help-mode-exit)
  (define-key cmd-help-local-map "Q" 'cmd-help-mode-exit)
  (define-key cmd-help-local-map "n" 'next-line)
  (define-key cmd-help-local-map "p" 'previous-line)

  ;; ���[�J���}�b�v�̎g�p�錾
  ;; Declare to use of the local map
  (use-local-map cmd-help-local-map)

  ;; �t�H���g���b�N�̐ݒ�
  ;; Setting for font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((cmd-font-lock-keywords) t t))
  (font-lock-mode t)

  (run-hooks 'cmd-help-mode-hook)
  )

(defun cmd-right-trim-region (start end)
  ;; "���[�W�����Ŏw�肳�ꂽ�͈͂̍s���̋󔒕������폜���܂��B"
  "Delete space characters at the end of line of the specified region.
Region is between START and END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "[\t ]+$" end t)
      (replace-match ""))))

(defun cmd-find-error ()
  "Try to find the error position in the cmd file.
This is maybe impossible to write, this is however a heuristic
that seems to work at least for me."
  (beginning-of-line)
  (when (looking-at (rx
                     (any "a-z")
                     ":\\"
                     (0+ (not (any ">")))
                     ">"
                     (0+ space)
                     (submatch (0+ nonl))
                     ))
    (let ((error-string (match-string 1))
          error-args
          arg
          error-point
          )
      (message "#%s#" error-string)
      (while (and error-string
                  (< 0 (length error-string)))
        (if (string-match (rx
                           (submatch
                            (or
                             (seq
                              ?\"
                              (0+ (not (any ?\")))
                              ?\")
                             (seq
                              (0+ (not (any " "))))
                             ))
                           (or
                            eos
                            (1+ " ")))
                          error-string)
            (progn
              ;;(lwarn 't :warning "e=#%s#" error-string)
              (setq arg (match-string 1 error-string))
              ;;(lwarn 't :warning "m=%s" arg)
              (setq error-args (cons arg error-args))
              (setq error-string (substring error-string (match-end 0))))
          (setq error-string nil)))
      (setq error-args (reverse error-args))
      ;;(lwarn 't :warning "ea=%s" error-args)
      ;; Try an exact match first, then guess those args surrounded by "" are substitutes:
      (with-current-buffer compilation-source
        (let ((old-point (point))
              (error-line))
          (goto-char (point-min))
          (let ((error-pattern (mapconcat 'regexp-quote error-args " *")))
            ;;(lwarn 't :warning "ep=%s" error-pattern)
            (setq error-point (re-search-forward error-pattern nil t))
            (unless error-point
              (setq error-pattern
                    (mapconcat (lambda (element)
                                 (if (string= (substring element 0 1) "\"")
                                     "[^ ]*"
                                   element))
                               error-args
                               " *"))
              (setq error-point (re-search-forward error-pattern nil t))))
          (if error-point
              (setq error-line (line-number-at-pos error-point))
            (message "Could not find error source line"))
          (goto-char old-point)
          error-line)))))

(defvar cmd-compilation-source nil)
(make-variable-buffer-local 'cmd-compilation-source)

(defun cmd-after-compilation (buffer text)
  ;;(lwarn 't :warning "buffer=%s, text=%s" buffer text)
  (message "buffer=%s, text=%s" buffer text)
  (with-current-buffer buffer
    (let ((error-line)
          (old-point (point)))
      (goto-char (point-min))
      (when (search-forward "exited abnormally" nil t)
        (beginning-of-line -1)
        (setq error-line (cmd-find-error))
        (goto-char old-point)
        (beginning-of-line -1)
        (if error-line
            (insert (format "%s:%s:(Notice that this is a guess of the error position.)\n"
                            (buffer-file-name compilation-source) error-line))
          (insert "(Could not find error position in source file.)\n"))
        (goto-char old-point)))))

(defun cmd-next-error-function (arg reset)
  "`next-error-function' for cmd-mode.
There should be only one error so the arguments are effectively
ignored."
  (message "arg=%s, reset=%s" arg reset)(sit-for 2)
  )

(defun cmd-exec ()
;;   "�o�b�t�@�̓��e���i�K�v�ł���΁j�t�@�C���ɕۑ����A�ۑ������t�@�C��
;; �����s����B���Ȃ�蔲�������Ă��܂��B"

  "Save the buffer (if necessary) and execute it.
corner-cutting work."
  (interactive)
  (save-buffer)
  ;;(shell-command (buffer-file-name))
  (let ((source-buffer (current-buffer))
        (compilation-buffer-name-function
         (function
          (lambda (ign)
            (concat "*" (buffer-file-name) "*")))))
    (compile
     (concat (w32-shell-name) " -c " (buffer-file-name)))
    (with-current-buffer (get-buffer (funcall compilation-buffer-name-function nil))
      (setq compilation-source source-buffer)
      ;;(remove-hook 'compilation-finish-functions 'cmd-after-compilation)
      ;;(add-hook 'compilation-finish-functions 'cmd-after-compilation nil t)
      (add-hook 'compilation-finish-functions 'cmd-after-compilation)
      )
    ))

;; (defun cmd-recenter ()
;;   ;; "�J�����g�ʒu�𒆉��ɔz�u������A�]���ȍs���̃X�y�[�X���폜����"
;;   "Set point to the center, and delete extra spaces at the end of line."
;;   (interactive)
;;   (recenter)
;;   (cmd-right-trim-region (point-min) (point-max)))

(defun cmd-next-label ()
  ;; "���̃��x���֒���"
  "Jump to the next label."
  (interactive)
  (if (re-search-forward cmd-label-pattern nil t)
      (goto-char (match-end 0))))
(defun cmd-prev-label ()
  ;; "�O�̃��x���֒���"
  "Jump to the previous label."
  (interactive)
  (if (re-search-backward cmd-label-pattern nil t)
      (goto-char (match-beginning 0))))

(defun cmd-fill-paragraph ()
;;   "�R�����g�s���̍s�l�߂��s���܂��B���Ȃ�蔲�������Ă��܂�(����Ȏ���
;; ���ėǂ��̂��낤��)�B"

  "Fill comment lines.
Fairly corner-cutting (is it allowed to do this kind of thing?)."
  (interactive)
  (save-excursion
    (let (;;�啶���E����������ʂ��Ȃ�
	  ;;Ignore case
	  (case-fold-search t)
          (cmd-rem-regexp
           "\\([\t ]*@?\\(rem\\|echo\\)\\)\\([ \t].*$\\|$\\)")
          rem-begin
          rem-end
          rem-paragraph
          match-str
          (cmd-fill-column fill-column)
          (current-buffer (buffer-name))
          )
      (beginning-of-line)
      (if (looking-at cmd-rem-regexp)
          (progn
            (setq match-str (buffer-substring
                             (match-beginning 1) (match-end 1))
                  rem-begin (point))
            (message ;; "cmd-fill-paragraph�y%s�z"
	     "cmd-fill-paragraph [%s]" match-str)
            (while (not (bobp))
              (forward-line -1)
              (if (looking-at (concat match-str "\\([\t ]\\|$\\)"))
                  (setq rem-begin (point))
                (goto-char (point-min))))

            (goto-char rem-begin)
            (setq cmd-rem-regexp
                  (concat "\\("
                          "\\(^" match-str "[^\n]*\n\\)+"
                          "\\(^" match-str "[^\n]*\\)?\\|"
                          "^" match-str ".*$\\)"))
            (if (looking-at cmd-rem-regexp)
                (progn
                  (setq rem-end (match-end 0)
                        rem-paragraph (buffer-substring rem-begin rem-end))
                  (set-buffer (get-buffer-create cmd-temp-buf))
                  (erase-buffer)
                  (insert rem-paragraph)
                  (indented-text-mode)

                  (goto-char (point-min))
                  (while (re-search-forward
                          (concat "^" match-str " ?") nil t)
                    (replace-match ""))

                  (setq fill-column (- cmd-fill-column (length match-str)))
                  (goto-char (point-min))
                  (while (not (eobp))
                    (fill-paragraph nil)
                    (forward-paragraph))

                  (goto-char (point-min))
                  (while (not (eobp))
                    (beginning-of-line)
                    (insert (concat match-str
                                    (if (looking-at "$") "" " ")))
                    (beginning-of-line)
                    (if (looking-at "[ \t]*@?echo$")
                        (progn
                          (end-of-line);
                          (insert ".")))
                    (forward-line 1))

                  (goto-char (point-max))
                  (beginning-of-line)
                  (if (looking-at (concat match-str "$"))
                      (replace-match ""))

                  (cmd-right-trim-region (point-min) (point-max))

                  (setq rem-paragraph
                        (buffer-substring (point-min) (point-max)))
                  (kill-buffer cmd-temp-buf)
                  (set-buffer current-buffer)
                  (delete-region rem-begin rem-end)
                  (insert rem-paragraph)
                  (message "done.")
              )
              (progn
                (ding)
                (error ;; "cmd-fill-paragraph�y�o�O�ł��傤�z"
		 "cmd-fill-paragraph [seems like a bug]")))
            )
        (message
	 ;;"cmd-fill-paragraph: �s���� REM ���� ECHO �ł͂���܂���"
	 "cmd-fill-paragraph: The line doesn't start with REM nor ECHO.")
        )
      )
    )
  )

(defun cmd-goto-label ()
;;   "�w�肵�����x���փW�����v���܂��B

;; ���x���̓~�j�o�b�t�@����⊮���͂ł��܂��B�~�j�o�b�t�@�ɂ́A���݂̃J�[
;; �\���̉��̕����񂪃f�t�H���g�\������܂��B(�����̃��x���̉��ꂩ�Ɉ�v
;; ����ꍇ)"

  "Jump to the specified label.

Label can be completed at the mini-buffer.  A word at the cursor
position is shown as default (if it matches one of existing
labels)."
  (interactive)
  (let ((label-alist '())
        (label nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*:\\([:A-Za-z0-9_-]+\\)\\([\t ]\\|$\\)" nil t)
        (setq label-alist (cons (list (buffer-substring (match-beginning 1)
                                                        (match-end 1)))
                                label-alist))))
    (if (not label-alist)
        (error ;; "���x����������܂���"
	       "No label found"))
    (setq label (completing-read "Label:" label-alist nil t
                                 (if (assoc (current-word) label-alist)
                                     (current-word) "")))
    (when (and label (not (string= label "")))
      (push-mark (point))                ;�}�[�N��t����		Set mark
      (goto-char (point-min))
      (re-search-forward (concat "^[\t ]*:" label "\\([ \t]\\|$\\)"))
      )
    )
  )

(defvar cmd-local-map nil)
(defun cmd-mode ()
;;   "MS-DOS�̃o�b�`�t�@�C����Windows NT �̃R�}���h�X�N���v�g�t�@�C�����
;; �W����ׂ̂ւȂ��Ⴑ���[�ǂł��B

;; ���o�b�t�@��ۑ���A�ۑ������t�@�C�������s����
;;   \\[cmd-exec]

;; ���R�}���h�̃w���v��\������B
;;   \\[cmd-help]

;; ���w�肵�����x���փW�����v
;;   \\[cmd-goto-label]

;; �����݂̃J�[�\���ʒu����ʂ̒����ɔz�u���A�s���̗]���ȋ󔒕������폜
;;   ����B
;;   \\[cmd-recenter]

;; ���R�����g�s��(REM or ECHO)�̍s�l�߂��s�Ȃ��B
;;   \\[cmd-fill-paragraph]

;; ���O�̃��x����
;;   \\[cmd-prev-label]

;; �����̃��x����
;;   \\[cmd-next-label]"

  "Tiny mode for editing batch files (MS-DOS) and command scripts (Windows NT).

* Save buffer and execute it.

  \\[cmd-exec]

* Show help of a command.
  \\[cmd-help]

* Jump to the specified label.
  \\[cmd-goto-label]

* Fill comment lines (e.g REM, ECHO).
  \\[cmd-fill-paragraph]

* Jump to the previous label.
  \\[cmd-prev-label]

* Jump to the next label.
  \\[cmd-next-label]"
  (interactive)
  (kill-all-local-variables)
  ;;���[�h���̐ݒ�ƁA���[�h���C���̃��[�h���t�B�[���h�̐ݒ�
  ;; Setting the mode name and the mode-name field of the mode-line.
  (setq major-mode 'cmd-mode
        ;;mode-name "CMD��BAT")
        mode-name "CMD")
  ;; �L�[�}�b�v�̐ݒ�
  ;; Setting keymap.
  (setq cmd-local-map (make-keymap))
  ;; �L�[�̊��蓖��
  ;; Assigning keys
  (define-key cmd-local-map "\C-c\C-c" 'cmd-exec)
  (define-key cmd-local-map "\C-c\C-g" 'cmd-goto-label)
  (define-key cmd-local-map "\C-c\C-h" 'cmd-help)
  ;;(define-key cmd-local-map "\C-l" 'cmd-recenter)
  ;;(define-key cmd-local-map "\eq" 'cmd-fill-paragraph)
  (define-key cmd-local-map "\M-q" 'cmd-fill-paragraph)
  ;;(define-key cmd-local-map "\e\C-a" 'cmd-prev-label)
  (define-key cmd-local-map "\M-\C-a" 'cmd-prev-label)
  ;;(define-key cmd-local-map "\e\C-e" 'cmd-next-label)
  (define-key cmd-local-map "\M-\C-e" 'cmd-next-label)

  ;; ���j���[�o�[�̍쐬
  ;; Create the menu bar.
  (define-key cmd-submenu-jump [sub-goto-label]
    '(;; "�C�ӂ̃��x��..."
      "Specified label" . cmd-goto-label))
  (define-key cmd-submenu-jump [sub-next-label]
    '(;; "���̃��x��"
      "Next label" . cmd-next-label))
  (define-key cmd-submenu-jump [sub-prev-label]
    '(;; "�O�̃��x��"
      "Previous label" . cmd-prev-label))

  (define-key cmd-local-map [menu-bar cmd] (cons mode-name cmd-menu-bar))
  (define-key cmd-menu-bar [help] '("Help" ;; "�w���v..."
				    . cmd-help))
  (define-key cmd-menu-bar [sep-2] '("--"))
  (define-key cmd-menu-bar [exec] '("Execute file" ;; "���s"
                                    . cmd-exec))
  (define-key cmd-menu-bar [sep-1] '("--"))
  (define-key cmd-menu-bar [submenu-jump] (cons "Jump to Label" ;; "�W�����v"
						cmd-submenu-jump))
;;   (define-key cmd-menu-bar [recenter] '("Delete trailing spaces"
;; 					;; "�s���̋󔒕����폜"
;; 					. cmd-recenter))
;;   (define-key cmd-menu-bar [fill] '("Fill comments" ;; "�R�����g�̍s�l��"
;; 				    . cmd-fill-paragraph))

  ;; ���[�J���}�b�v�̎g�p�錾
  ;; Declare to use of the local map
  (use-local-map cmd-local-map)

  ;; font-lock�̐ݒ�
  ;; Setting for font-lock.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((cmd-font-lock-keywords) t t))

  ;; configure comment support
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end)
  (setq imenu-generic-expression '((nil "^:\\(\\sw+\\)" 1))
        parse-sexp-ignore-comments t
        comment-end ""
        comment-start "@rem "
        comment-start-skip "@?[Rr][Ee][Mm] *")
  (font-lock-mode t)

  (setq next-error-function 'cmd-next-error-function)

  (run-hooks 'cmd-mode-hook)
  )

(provide 'cmd-mode)

;;; cmd-mode.el ends here

;;; emacsw32-setup-base.el --- Functions for installation and setup of Emacs
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2004-12-27
;; Version: 1.02
;; Last-Updated: Wed Apr 04 15:02:14 2007 (7200 +0200)
;; Compatibility: Emacs 22
;; Keywords: installation setup

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; To find out more about the GNU General Public License you can visit
;; Free Software Foundation's website http://www.fsf.org/.  Or, write
;; to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;; Commentary:

;; This file is used for basic setup in Emacs. It is used by Emacs MS
;; Windows Setup Helper. It should be called the way it is done in
;; mkBase.cmd.


;;; History:
;;


;;; Code:

(defun emacsw32-dir-from-load-file ()
  (let ((load-dir (file-name-directory (if load-file-name
                                           load-file-name
                                         buffer-file-name))))
    (file-name-as-directory (expand-file-name ".." load-dir))))
(message "dir-from-load=%s" (emacsw32-dir-from-load-file))(sit-for 2)


;;(add-to-list 'load-path (expand-file-name (concat +exec-directory+ "../../EmacsW32/lisp/")))
;;(add-to-list 'load-path (file-name-directory load-file-name))
(add-to-list 'load-path
             (file-name-as-directory
              (expand-file-name "lisp" (emacsw32-dir-from-load-file))))


(require 'setup-helper)
(require 'w32-reg-iface)

(defconst w32-setup-ver
  ;;(let ((iss-file (expand-file-name (concat +exec-directory+ "../../EmacsW32/EmacsW32.iss"))))
  (let ((iss-file (expand-file-name "EmacsW32.iss" (emacsw32-dir-from-load-file))))
    (unless (file-exists-p iss-file)
      (error "Can't find %s" iss-file))
    (with-temp-buffer
      (insert-file-contents iss-file)
      (goto-char (point-min))
      (re-search-forward "^VersionInfoVersion=\\(.*\\)$")
      (match-string 1))))


                                        ;"0.65")

(defconst w32-inno-prog nil)

(defconst emacsw32-inno-iscc
  (let ((open-cmd (car (w32-reg-iface-read-value
                        "HKCR/InnoSetupScriptFile/shell/OpenWithInnoSetup/command/"))))
    (when open-cmd
      (let* ((pos (string-match "Compil32\.exe" open-cmd))
             (path (when pos (substring open-cmd 0 pos)))
             (iscc (concat path "ISCC.exe")))
        (when (eq (aref iscc 0) 34) (setq iscc (substring iscc 1)))
        (unless (file-exists-p iscc) (setq iscc nil))
        iscc))))

(defun emacsw32-emacs-batch (&rest emacs-param)
  "Get OS command string for Emacs batch command."
  (let* (
         (retold (concat "\""
                         (convert-standard-filename
                          (expand-file-name
                           (concat +exec-directory+ "emacs.exe")))
                         "\" -batch -no-site-file -q"))
         (retold2 "%emacs-exe% -batch -Q")
         (this-file (if load-file-name
                        load-file-name
                      (buffer-file-name)))
         (this-dir (file-name-directory this-file))
         (install-root (expand-file-name "../.." this-dir))
         (emacs-bin-dir (expand-file-name "emacs/bin" install-root))
         (emacs-exe (convert-standard-filename
                     (expand-file-name "emacs.exe" emacs-bin-dir)))
         (ret (concat emacs-exe " -batch -Q"))
         (par emacs-param))
    (while par
      (setq ret (concat ret " " (car par)))
      (setq par (cdr par)))
    ret))

;;(defvar emacsw32-set-emacs-exe "@ if A%emacs-exe% == A set emacs-exe=emacs.exe\n")
(defvar emacsw32-set-emacs-exe "")

(defun emacsw32-y-or-n (prompt)
  (concat
   emacsw32-set-emacs-exe
   "@" (emacsw32-emacs-batch
        "-eval"
        (concat " \"(unless (y-or-n-p \\\"" prompt "\\\") (kill-emacs 1))\n"))))

(defun emacsw32-write-mkInstaller-cmd ()
  "Write a command file that can create the distribution EmacsInstaller exe."
  (let* ((outexe (concat "EmacsW32-" w32-setup-ver ".exe"))
         (iscc emacsw32-inno-iscc)
         (inst-iss "%0\\..\\temp-installer.iss")
         (emacs-tree (convert-standard-filename
                      (expand-file-name (concat +exec-directory+ "../"))))
         (emacs-exe (convert-standard-filename
                     (expand-file-name
                      (concat +exec-directory+ "emacs.exe"))))
         (code (concat
                "@rem (Created by Setup Helper at " (current-time-string) ")\n"
                "@echo.\n"
                "@echo   This creates a new distribution"
                " Emacs-" (format "%d" emacs-major-version) "-DISTRIBID-emacsw32-" w32-setup-ver ".exe file.\n"
                "@echo   The .exe file will include YOUR current Emacs tree, that is\n"
                "@echo   everything in \"" emacs-tree "\".\n"
                "@echo.\n"
                "@echo   Your are encourage to only keep the core Emacs dist there.\n"
                "@echo   If you want to include other files you may edit the script file\n"
                "@echo   before your create the installation package.\n"
                "@echo.\n"
                "\n"
                "@if not \"%1\"==\"\" goto mk-iss\n"
                "\n"
                (emacsw32-y-or-n "You will be prompted for DISTRIBID (since you did not give it as a parameter).\\nDo you want to continue? ")
                ;;"@if %errorlevel% GTR 0 goto fin\n"
                "@if %errorlevel% GTR 0 exit /b %errorlevel%\n"
                "\n"
                ":mk-iss\n"
                "@if exist " inst-iss " del " inst-iss "\n"
                emacsw32-set-emacs-exe
                "@" (emacsw32-emacs-batch "-l %0\\..\\lisp\\mkInstaller.el --eval=\"(mkInstaller-create-iss \\\"%1\\\" \\\"%2\\\")\"") "\n"
                ;;"@if %errorlevel% NEQ 0 goto fin\n"
                "@if %errorlevel% GTR 0 exit /b %errorlevel%\n"
                "\n"
                "@if not \"%1\"==\"\" goto run\n"
                "\n"
                "@echo.\n"
                (emacsw32-y-or-n "Do you want to run the Inno script above to create the .exe? ")
                "@if %errorlevel% GTR 0 goto fin\n"
                "\n"
                (emacsw32-y-or-n "Do you want to start Inno Setup first to edit the script? ")
                "@if %errorlevel% GTR 0 goto run\n"
                "\n"
                (if iscc
                    (concat "@start " inst-iss "\n")
                  (concat "@echo Could not find Inno Setup 5 Compiler\n"
                          "@rem " inst-iss "\n"))
                "@goto fin\n"
                "\n"
                ":run\n"
                (if iscc
                    (concat "\"" iscc "\" " inst-iss "\n")
                  (concat "@echo Could not find Inno Setup 5 Compiler\n"
                          "@rem iscc.exe " inst-iss "\n"))
                "\n"
                ":fin\n"
                ))
         ;;(file (expand-file-name (concat +exec-directory+ "../../EmacsW32/mkInstaller.cmd")))
         (file (expand-file-name "mkInstaller.cmd" (emacsw32-dir-from-load-file)))
         )
    (with-temp-file file
      (princ code (current-buffer)))
    (message "Created %s" file)))

(defun emacsw32-write-mkSetupHelper-cmd ()
  "Write a command file that can create the distribution EmacsW32-nn exe."
  (let* ((outexe (concat "EmacsW32-alone-" w32-setup-ver ".exe"))
         (iscc emacsw32-inno-iscc)
         (run-exe (concat "if %ERRORLEVEL% LEQ 1 start %0\\..\\Output\\" outexe "\n"))
         (code (concat
                "@rem This file creates the distribution EmacsW32-nn.exe file.\n"
                "@rem (Created by EmacsW32 at " (current-time-string) ")\n"
                (if iscc
                    (concat
                     (concat "\"" iscc "\" %0\\..\\EmacsW32.iss\n")
                     run-exe)
                  (concat
                   "@echo Could not find Inno Setup 5 Compiler\n"
                   "@rem iscc.exe %0\\..\\EmacsW32.iss\n"
                   "@rem " run-exe))
                ))
         ;;(file (expand-file-name (concat +exec-directory+ "../../EmacsW32/mkSetupHelper.cmd")))
         (file (expand-file-name "mkSetupHelper.cmd" (emacsw32-dir-from-load-file)))
         )
    (with-temp-file file
      (princ code (current-buffer)))
    (message "Created %s" file)))

(defun emacsw32-write-mkzip-cmd ()
  "Write a command file that can be used to create the distribution zip file."
  (let* ((outzip (concat "EmacsW32\\Output\\EmacsW32-alone-" w32-setup-ver ".zip"))
         (code (concat
                "@rem This file creates the distribution zip file.\n"
                "@rem (Created by EmacsW32 at " (current-time-string) ")\n"
                "pushd %0\\..\\..\n"
                "@if exist EmacsW32\\mkZip.lst del EmacsW32\\mkZip.lst\n"
                ;;emacsw32-set-emacs-exe
                ;;"@" (emacsw32-emacs-batch "-l EmacsW32/lisp/mkZipLst.el") "\n"
                "@" "emacs.exe -batch -Q -l EmacsW32/lisp/mkZipLst.el" "\n"
                "@if not exist EmacsW32\\Output mkdir EmacsW32\\Output\n"
                "@if exist " outzip " del " outzip "\n"
                "EmacsW32\\bin\\7za.exe a -tzip " outzip " @EmacsW32\\mkZip.lst\n"
                "popd\n"))
         ;;(file (expand-file-name (concat +exec-directory+ "../../EmacsW32/mkZip.cmd")))
         (file (expand-file-name "mkZip.cmd" (emacsw32-dir-from-load-file)))
         )
    (with-temp-file file
      (princ code (current-buffer)))
    (message "Created %s" file)))

(defun emacsw32-write-mkBase-cmd ()
  "Write a command file for running this script file."
  (let ((code (concat
               "@rem Rewrite the command files and update site-start.el.\n"
               "@rem (Created by Setup Helper at " (current-time-string) ")\n"
               ;;emacsw32-set-emacs-exe
               ;;"@" (emacsw32-emacs-batch "-l lisp/emacsw32-setup-base.el") "\n"
               "@" "emacs.exe -batch -Q -l lisp/emacsw32-setup-base.el" "\n"
               ))
        ;;(file (expand-file-name (concat +exec-directory+ "../../EmacsW32/mkBase.cmd")))
        (file (expand-file-name "mkBase.cmd" (emacsw32-dir-from-load-file)))
        )
    (with-temp-file file
      (princ code (current-buffer)))
    (message "Created %s" file)))

(defun emacsw32-write-emacs-cmd ()
  "Write a command file that can be used for starting Emacs from the command line."
  (let* ((emacsclient-exe (concat "\""
                                  (convert-standard-filename
                                   (expand-file-name "emacsclient.exe" +exec-directory+)
                                   )
                                  "\""))
         (code (concat
                "@rem Put this file (e.cmd) in your PATH.\n"
                "@rem (Created by Setup Helper at " (current-time-string) ")\n"
                "@rem -----------------------------\n"
                "@rem Starts Emacs (through emacsclient) from command line.\n"
                "@rem This can be used in many ways:\n"
                "@rem    With a file as parameter: Start editing this file\n"
                "@rem    With a directory as a parameter: Start dired\n"
                "@rem If the first argument to this file is -e then there are\n"
                "@rem some enhancements:\n"
                "@rem    1) Before executing the -e command Emacs changes dir to current dir.\n"
                "@rem    2) \\ embedded in the -e parameter is converted to /.\n"
                "@rem    3) The emacswindow is shown after eval.\n"
                "@rem Then you can do for example to start ediff in Emacs do\n"
                "@rem    e.cmd -e \"(ediff-files \\\"fil1.txt\\\" \\\"fil 2.txt\\\")\"\n"

                "@setlocal\n"
                "@set args=%*\n"
                "@set emacs_client=" emacsclient-exe "\n"
                "@if not A%1 == A-e goto noE\n"
                "@   set args=%args:\\=/%\n"
                "@   set args=%args:/\"=\\\"%\n"
                "@   set emacs_cd=%CD:\\=/%\n"
                "@   %emacs_client% -e \"(setq default-directory \\\"%emacs_cd%\\\")\"\n"

                ":noE\n"

                ;;"@%emacs_client% -n %*\n"))
                "@%emacs_client% -n %args%\n"
                "@if not A%emacs_cd% == A %emacs_client% -n\n"
                ))
         (file (expand-file-name "e.cmd" (emacsw32-dir-from-load-file)))
         )
    (with-temp-file file
      (princ code (current-buffer)))
    (message "Created %s" file)))

(defun emacsw32-write-ediff-cmd ()
  "Write a command file to start Emacs ediff."
  (let* ((emacsclient-exe (concat "\""
                                  (convert-standard-filename
                                   (expand-file-name "emacsclient.exe" +exec-directory+))
                                  "\""))
         (code (concat
                "@rem Put this file (ediff.cmd) in your PATH.\n"
                "@rem (Created by Setup Helper at " (current-time-string) ")\n"
                "@rem -----------------------------\n"
                "@rem Starts Emacs ediff (through emacsclient) from command line.\n"
                "@rem Takes the two file to compare as parameters.\n"

                "@setlocal\n"
                "@set f1=%1\n"
                "@set f2=%2\n"
                "@set f1=%f1:\\=/%\n"
                "@set f2=%f2:\\=/%\n"
                "@set emacs_cd=%CD:\\=/%\n"
                "@set emacs_client=" emacsclient-exe "\n"
                ;;"@" emacsclient-exe " -swf -e \"(setq default-directory \\\"%emacs_cd%\\\")\"\n"
                "@%emacs_client% -n\n"
                "@%emacs_client% -e \"(setq default-directory \\\"%emacs_cd%\\\")\"\n"
                ;;"@" emacsclient-exe " -sqf  -e \"(ediff-files \\\"%f1%\\\" \\\"%f2%\\\")\"\n"))
                "@%emacs_client% -n  -e \"(ediff-files \\\"%f1%\\\" \\\"%f2%\\\")\"\n"))
         ;;(file (expand-file-name (concat +exec-directory+ "../../EmacsW32/ediff.cmd")))
         (file (expand-file-name "ediff.cmd" (emacsw32-dir-from-load-file)))
         )
    (with-temp-file file
      (princ code (current-buffer)))
    (message "Created %s" file)))

(defun emacsw32-write-emacs-sh ()
  "Write a command file that can be used for starting Emacs from an MSYS sh."
  (let ((emacsclient (convert-standard-filename
                      ;;(expand-file-name (concat +exec-directory+ "../../EmacsW32/bin/gnuclient.exe"))
                      ;;(expand-file-name "bin/emacsclient.exe" (emacsw32-dir-from-load-file))
                      (expand-file-name "emacsclient.exe" +exec-directory+)
                      )))
    (setq emacsclient (replace-regexp-in-string "\\\\" "/" emacsclient))
    (setq emacsclient (replace-regexp-in-string ":" "" emacsclient))
    (setq emacsclient (concat "/" emacsclient))
    (let ((code (concat
                 "#!/bin/sh\n"
                 "# Starts Emacs (through emacsclient) from an MSYS sh shell.\n"
                 "# Put this file in your MSYS sh:s PATH.\n"
                 "# (Created by Setup Helper at " (current-time-string) ")\n"
                 ;;"\"" emacsclient "\" -sqf \"$1\"\n"))
                 "\"" emacsclient "\" -n \"$1\"\n"))
          ;;(file (expand-file-name (concat +exec-directory+ "../../EmacsW32/emacs")))
          (file (expand-file-name "emacs" (emacsw32-dir-from-load-file)))
          )
      (with-temp-file file
        (princ code (current-buffer)))
      (message "Created %s" file))))


(defun emacsw32-write-elc-cmd ()
  "Write a command file that can be used for to byte compile a directory."
  (let ((code (concat
               "@rem Recompiles .el files given. Wildcard allowed.\n"
               "@rem (Created by Setup Helper at " (current-time-string) ")\n"
               "for %%f in (%*) do "
               emacsw32-set-emacs-exe
               (emacsw32-emacs-batch "-L ./ -f batch-byte-compile %%f") "\n"
               ))
        ;;(file (expand-file-name (concat +exec-directory+ "../../EmacsW32/elc.cmd")))
        (file (expand-file-name "elc.cmd" (emacsw32-dir-from-load-file)))
        )
    ;;(save-window-excursion
    (with-temp-file file
      (princ code (current-buffer)))
    (message "Created %s" file)))


(defun emacsw32-setup-comment (more-comments)
  "Return comment to put in files when changing them."
  (let ((cmnt (concat "****** Added by emacsw32-setup-base at " (current-time-string))))
    (when more-comments
      (setq cmnt (concat cmnt "\n" more-comments)))
    cmnt))

(defun emacsw32-site-start-sexps-needed ()
  "Return lisp code that must be in site-start.el"
  (concat

   ;;    "(add-to-list 'load-path (expand-file-name (concat +exec-directory+ "
   ;;    (setup-helper-q "../../my-lisp/")
   ;;    ")))\n"

   ;;    "(add-to-list 'load-path (expand-file-name (concat +exec-directory+ "
   ;;    (setup-helper-q "../../EmacsW32/lisp/")
   ;;    ")))\n"

   "(let ((lisp-dir (expand-file-name (concat +exec-directory+ "
   (setup-helper-q "../../EmacsW32/lisp/") "))))\n"
   "(unless (file-accessible-directory-p lisp-dir)\n"
   "(lwarn " (setup-helper-q "Can't find %s") " lisp-dir)\n"
   "(sit-for 10))\n"
   "(when (file-accessible-directory-p lisp-dir)\n"
   "(message " (setup-helper-q "Adding %s to load-path") " lisp-dir)\n"
   "(add-to-list 'load-path lisp-dir))\n"

   "(require 'emacsw32 nil t)\n"
   "(unless (featurep 'emacsw32)\n"
   "(lwarn '(emacsw32) :error " (setup-helper-q "Could not find emacsw32.el") ")))\n"

   ))

;; (defun emacsw32-default-sexps-needed ()
;;   "Return lisp code that must be in default.el"
;;   (concat

;;    "(progn\n"
;;    "(require 'emacsw32 nil t)\n"
;;    "(unless (featurep 'emacsw32)\n"
;;    ;;"(message " (setup-helper-q "Could not find emacsw32.el") ")\n"
;;    ;;"(sit-for 10))\n"
;;    "(lwarn '(emacsw32) :error " (setup-helper-q "Could not find emacsw32.el") ")))\n"

;;    ))



;; (defun emacsw32-change-default-el ()
;;   "Add needed line to default.el"
;;   (let* ((default-el (or (locate-library "default")
;;                          (setup-helper-default-el-file-name1)))
;;          (default-el-el (concat (file-name-sans-extension default-el) ".el"))
;;          )
;;     ;;(message "default-el=%s" default-el)
;;     (when (setup-helper-add-sexp-if-not-found
;;            default-el
;;            (emacsw32-default-sexps-needed)
;;            (emacsw32-setup-comment "Load emacsw32 if found.") t t)
;;       (message "Changed %s" default-el)(sit-for 5)
;;       ;; This assumes that locate-library returns an expanded file name:
;;       (unless (equal default-el-el default-el)
;;         (message "Will byte compile %s" default-el-el)(sit-for 5)
;;         (byte-compile-file default-el-el)(sit-for 5)))
;;     ))

(defun emacsw32-change-site-start-el ()
  "Add needed line to site-start.el"
  (let* ((site-start (or (locate-library (or site-run-file "site-start"))
                         (setup-helper-site-start-el-file-name1)))
         (site-start-el (concat (file-name-sans-extension site-start) ".el"))
         )
    ;;(message "site-start-el=%s" site-start-el) (sit-for 2)
    ;;(message "site-start   =%s" site-start) (sit-for 2)
    (when (setup-helper-add-sexp-if-not-found
           site-start-el
           (emacsw32-site-start-sexps-needed)
           (emacsw32-setup-comment
            "Add EmacsW32/lisp to load-path if found.") t t)
      (message "Changed %s" site-start-el)(sit-for 5)
      ;; This assumes that locate-library returns an expanded file name:
      (unless (equal site-start-el site-start)
        (message "Will byte compile %s" site-start-el)(sit-for 5)
        (byte-compile-file site-start-el)(sit-for 5)))
    ))

(defun emacsw32-user-update-version-numbers ()
  (let ((emacsw32util-html
         (find-file-noselect
          (expand-file-name "etc/emacsw32util.html"
                            (emacsw32-dir-from-load-file))))
        (emacsw32-el
         (find-file-noselect
          (expand-file-name "lisp/emacsw32.el"
                            (emacsw32-dir-from-load-file))))
        (date (format-time-string "%Y-%m-%d"))
        )
    (with-current-buffer emacsw32util-html
      (goto-char (point-min))
      (re-search-forward "<span id=\"version\">\\([^<]*\\)</span>")
      (goto-char (match-beginning 1))
      (when (< 20 (- (match-end 1) (match-beginning 1)))
        (error "Could not update version for %s" emacsw32util-html))
      (delete-region (match-beginning 1) (match-end 1))
      (insert w32-setup-ver " " date)
      (save-buffer))
    (with-current-buffer emacsw32-el
      (goto-char (point-min))
      (re-search-forward "(let ((auto-updated-version \"\\([^\"]*\\)\"))")
      (goto-char (match-beginning 1))
      (when (< 20 (- (match-end 1) (match-beginning 1)))
        (error "Could not update version for %s" emacsw32util-html))
      (delete-region (match-beginning 1) (match-end 1))
      (insert w32-setup-ver " " date)
      (save-buffer))))

(defun emacsw32-setup-base ()
  "Do the necessary setup for emacsw32 to work."
  (or (file-exists-p (setup-helper-site-lisp1))
      (make-directory-internal (setup-helper-site-lisp1)))
  (emacsw32-change-site-start-el)
  ;;(emacsw32-change-default-el)
  (emacsw32-write-emacs-cmd)
  (emacsw32-write-ediff-cmd)
  (emacsw32-write-emacs-sh)
  (emacsw32-write-elc-cmd)
  (emacsw32-write-mkzip-cmd)
  (emacsw32-write-mkBase-cmd)
  (emacsw32-write-mkSetupHelper-cmd)
  (emacsw32-write-mkInstaller-cmd)
  (emacsw32-user-update-version-numbers)
  ;;   (with-timeout (25 t)
  ;;     (read-from-minibuffer "Done EmacsW32 base setup. Push CR to continue... "))
  ;; Timeout does not work in noninteractive mode on w32 with Emacs 21.3.1
  (if noninteractive
      (progn
        (message "Done EmacsW32 base setup. Sleeping 20 seconds so you may read this ...")
        (sit-for 20))
    (message "Done EmacsW32 base setup."))
  t)

;; I would like to get a traceback+prompt in batch mode but I do not know how to get it.
(if noninteractive
    (condition-case err
        (let ((debug-on-error nil))
          (emacsw32-setup-base))
      (error
       (read-from-minibuffer (format "%s -- Push return: " (error-message-string err)))))
  (let ((debug-on-error t))
    (emacsw32-setup-base)))

(provide 'emacsw32-setup-base)

;;; emacsw32-setup-base.el ends here

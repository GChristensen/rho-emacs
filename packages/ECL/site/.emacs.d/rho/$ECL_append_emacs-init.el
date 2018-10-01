;; package:ECL ; please do not remove or edit these comments
;; Slime setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf slime-lisp-implementations 
     (append slime-lisp-implementations
       `((ecl (,(concat +rho-dir+ "/batch/run-ecl"))
          :coding-system utf-8-unix))))

(defvar *visual-c++-path* nil)
(defvar *windows-sdk-path* nil)

(defun find-visual-c++ ()
  (when (not *visual-c++-path*)
    (alien-bind (vcPath sdkPath)
      'vbs "
           On Error Resume Next
           Dim objShell
           Dim vcPath
           Dim sdkPath

           Function MakeVCRegKey(product, major, minor)
             MakeVCRegKey = \"HKLM\\SOFTWARE\\Microsoft\\\" & product & \"\\\" _
                            & major & \".\" & minor & \"\\Setup\\VC\\ProductDir\"
           End Function

           Set objShell = CreateObject(\"WScript.Shell\")

           ' Search for Visual C++ from Visual Studio
           For i = 15 to 7 Step -1
             vcPath = objShell.RegRead(MakeVCRegKey(\"VisualStudio\", i, 0))
             If vcPath <> \"\" Then
               Exit For
             End If
           Next

           ' Search for Visual Studio 2003
           If vcPath = \"\" Then
             vcPath = objShell.RegRead(MakeVCRegKey(\"VisualStudio\", 7, 1))
           End If  

           ' Search for Visual C++ Express
           If vcPath = \"\" Then
             For i = 15 to 8 Step -1
               vcPath = objShell.RegRead(MakeVCRegKey(\"VCExpress\", i, 0))
               If vcPath <> \"\" Then
                 Exit For
               End If
             Next
           End If
      
           If Right(vcPath, 1) = \"\\\" Then
             vcPath = Left(vcPath, Len(vcPath) - 1)
           End If

           sdkPath = objShell.RegRead(\"HKLM\\SOFTWARE\\Microsoft\\\" _
                     & \"Microsoft SDKs\\Windows\\CurrentInstallFolder\")"
    (if (boundp 'vcPath)
        (progn (setq *visual-c++-path* vcPath)
               (setq *windows-sdk-path* sdkPath))
      (progn (setq *visual-c++-path* nil)
             (setq *windows-sdk-path* nil))))))

(defun set-vc-env ())

(defun set-vc-env ()
  (interactive)
  (progn
	(find-visual-c++)
	(if (or (not *visual-c++-path*) (string= *visual-c++-path* ""))
		(> 1 0) ;(warn "Visual C++ is not found. Visual C++ is required by ECL for byte compilation.")
	  (progn
		(setenv "VC_INSTALL_DIR" *visual-c++-path*)
 	(setenv "WIN_SDK_DIR" *windows-sdk-path*)))))

(defadvice slime (before set-ecl-environment activate)
  (when (eq command 'ecl)
	(set-vc-env)))    

(defun rho-run-ecl ()
  (interactive)
  (run-slime-repl 'ecl))

(easy-menu-add-item nil '("((" "Launch") ["ECL" rho-run-ecl t])

(create-default-get-version 'ecl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Slime setup ;;


;; package:MSYS ; please do not remove or edit these comments
;; MSYS setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'w32shell)

(defconst +msys-home+ (concat +home-dir+ "/.msys"))
(defconst +msys-tmp+ (concat +home-dir+ "/.msys/tmp"))

(if (not (file-exists-p +home-dir+))
  (make-directory +home-dir+))

(if (and (not +anyhome?+) (not (file-exists-p +msys-home+)))
  (make-directory +msys-home+))

(if (and (not +anyhome?+) (not (file-exists-p +msys-tmp+)))
  (make-directory +msys-tmp+))

(defun run-msys-shell ()
 (interactive)
 (let ((w32shell-msys-bin (concat +rho-dir+ "/bin/msys/usr/bin"))
        (w32shell-wanted-progs
         '("grep" "find" "xargs" "cmp" "diff" "diff3" "cmp" "patch"))
        (w32shell-shell 'msys))
    (call-interactively 'msys-shell)))

(defun external-msys-shell ()
  (interactive)
  (start-process "rho-msys" "*rho-msys*" 
                 (concat +rho-dir+ "/rho.exe") "/TARGET:MSYS")
  (let ((kill-buffer-query-functions nil))
    (sleep-for 1)
    (kill-buffer "*rho-msys*")))

(defun external-mingw32-shell ()
  (interactive)
  (start-process "rho-mingw32" "*rho-mingw32*" 
                 (concat +rho-dir+ "/rho.exe") "/TARGET:MINGW32")
  (let ((kill-buffer-query-functions nil))
    (sleep-for 1)
    (kill-buffer "*rho-mingw32*")))

(defun external-mingw64-shell ()
  (interactive)
  (start-process "rho-mingw64" "*rho-mingw64*" 
                 (concat +rho-dir+ "/rho.exe") "/TARGET:MINGW64")
  (let ((kill-buffer-query-functions nil))
    (sleep-for 1)
    (kill-buffer "*rho-mingw64*")))

(defun external-install-mingw32 ()
  (interactive)
  (start-process "rho-install-mingw" "*rho-install-mingw*" 
                 (concat +rho-dir+ "/batch/installmingw32.cmd") "")
  (let ((kill-buffer-query-functions nil))
    (sleep-for 1)
    (kill-buffer "*rho-install-mingw*")))

(defun external-install-mingw64 ()
  (interactive)
  (start-process "rho-install-mingw64" "*rho-install-mingw64*" 
                 (concat +rho-dir+ "/batch/installmingw64.cmd") "")
  (let ((kill-buffer-query-functions nil))
    (sleep-for 1)
    (kill-buffer "*rho-install-mingw64*")))


(defun external-update-mingw ()
  (interactive)
  (start-process "rho-update-mingw" "*rho-update-mingw*" 
                 (concat +rho-dir+ "/batch/updatemingw.cmd") "")
  (let ((kill-buffer-query-functions nil))
    (sleep-for 1)
    (kill-buffer "*rho-update-mingw*")))

(defvar *mingw-menu-items* 
        (list "MSYS" ["MSYS2 (emacs)" run-msys-shell t]
                     ["MSYS2 (external)" external-msys-shell t] 
                     ["MINGW32 (external)" external-mingw32-shell t]
                     ["MINGW64 (external)" external-mingw64-shell t]
                     ["--" 'ignore :visible (lambda ())]
                     ["Update installed packages" external-update-mingw t]))

(if (not (file-exists-p (concat +rho-dir+ "/bin/msys/mingw64/bin/gcc.exe")))
  (setf *mingw-menu-items* (append *mingw-menu-items* '(["Install MinGW (64-bit)" external-install-mingw64 t]))))

(if (not (file-exists-p (concat +rho-dir+ "/bin/msys/mingw32/bin/gcc.exe")))
  (setf *mingw-menu-items* (append *mingw-menu-items* '(["Install MinGW (32-bit)" external-install-mingw32 t]))))

(easy-menu-add-item nil '("((" "Launch") *mingw-menu-items*) 

(when +portable?+
  (let ((fstab (concat +rho-dir+ "/bin/msys/etc/fstab")))
    (when (file-exists-p fstab)
      (with-temp-file fstab
        (insert "none / cygdrive binary,posix=0,noacl,user 0 0\n")
;        (insert-string (concat +rho-dir+ "/bin/mingw /mingw32\n"))
        (insert (concat +msys-tmp+ " /tmp\n"))
        (insert (concat +home-dir-unix-path+ "/.msys /home\n"))))))

(create-default-get-version 'MSYS)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MSYS setup ;;


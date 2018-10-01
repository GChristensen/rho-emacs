;;; emacsw32.el --- MS Windows style feeling
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2005-08-03
;; Version: 0.87
;; Last-Updated: 2008-01-21T20:21:32+0100 Mon
;; Keywords: convenience emulations w32
;; Features that might be required by this library:
;;
;;   `cl', `compile', `electric', `emacsw32-eol', `find-func',
;;   `grep', `hfyview', `menuacc', `noprint', `nxhtml-loader',
;;   `rebind', `swbuff', `swbuff-y', `timer', `tmm', `w32-grep',
;;   `w32-integ', `w32-meta', `w32shell'.
;;
;; This file does not do very much by itself, it serves more as a hub
;; for other modules. It uses Emacs customization to load the other
;; modules and it provides a bit of glue sometimes (between cua and
;; viper).  It also provides some keyboard keys (in
;; `emacsw32-mode') and adds some menu entries.
;;
;; Since this module is aimed mostly at users running MS Windows basic
;; documentation is provided in html format in the file
;; EmacsW32Util.html (which is the documentation for EmacsW32 where
;; this file is included).

;; To use this module put it in Emacs load-path and write at the end
;; of your .emacs (or in default.el):
;;
;;    (require 'emacsw32)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of Emacs
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; To find out more about the GNU General Public License you can visit
;; Free Software Foundation's website http://www.fsf.org/.  Or, write
;; to the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

(let* ((this-dir (file-name-directory
                  (if load-file-name load-file-name buffer-file-name)))
       (util-dir (file-name-as-directory
                      (expand-file-name "../nxhtml/util"
                                    this-dir)))
       )
  (unless (file-directory-p util-dir)
    (error "Emacsw32 load error: Can't find %s" util-dir))
  (add-to-list 'load-path this-dir)
  ;; Add the util dir here in case nxhtml is not used. This is
  ;; probably better than addint the autoloads which could be
  ;; confusing if autostart.el was not run.
  (add-to-list 'load-path util-dir))

(require 'emacsw32-eol nil t)
(require 'nxhtml-loader nil t)
(require 'noprint nil t) ;; For removing default print entries
(require 'w32-grep nil t)


(defun emacsw32-get-version ()
  (let ((auto-updated-version "1.58"))
    auto-updated-version))

(defun emacsw32-version ()
  (interactive)
  (message "EmacsW32 Version %s" (emacsw32-get-version)))

;;_____________________________________________________
;;
;; Custom
;;_____________________________________________________

(defgroup emacsw32 nil
  "Selected options for users with experience of MS Windows.
Those options are collected here for your convenience."
  :group 'convenience
  :group 'environment)

(defgroup emacsw32-install nil
  "Installation related options"
  :tag "W32 Installation"
  :group 'emacsw32)

;; Add to EmacsW32 group:
(custom-add-to-group 'emacsw32 'cua-mode        'custom-variable)
(custom-add-to-group 'emacsw32 'recentf-mode    'custom-variable)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Specific to MS Window
(when (eq system-type 'windows-nt)

  (defun use-this-emacs ()
    (interactive)
    (let ((usethis-prog (expand-file-name "usethis.exe" +exec-directory+)))
      (assert (file-exists-p usethis-prog))
      (call-process usethis-prog)))

  (defun emacsw32-is-patched ()
    "Return t if this is the patched version, otherwise nil.
This function only checks if it is the patched version of
Emacs+EmacsW32 that is used. It knows nothing about other
patches."
    (fboundp 'w32-wh-keyboard-ll))

  (require 'w32shell nil t)
  (require 'w32-integ nil t)
  (when (emacsw32-is-patched)
    (require 'menuacc)
    (require 'w32-meta))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; These are the keys for the w32-send-sys-command
  ;; (stored here for your convenience)
  ;;SC_SIZE 0xF000
  ;;SC_MOVE 0xF010
  ;;SC_MINIMIZE 0xF020
  ;;SC_MAXIMIZE 0xF030
  ;;SC_NEXTWINDOW 0xF040
  ;;SC_PREVWINDOW 0xF050
  ;;SC_CLOSE 0xF060 (61536)
  ;;SC_VSCROLL 0xF070
  ;;SC_HSCROLL 0xF080
  ;;SC_MOUSEMENU 0xF090
  ;;SC_KEYMENU 0xF100
  ;;SC_ARRANGE 0xF110
  ;;SC_RESTORE 0xF120
  ;;SC_TASKLIST 0xF130
  ;;SC_SCREENSAVE 0xF140
  ;;SC_HOTKEY 0xF150
  ;;SC_DEFAULT 0xF160
  ;;SC_MONITORPOWER 0xF170
  ;;SC_CONTEXTHELP 0xF180
  ;;SC_SEPARATOR 0xF00F


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Frame w32 handling

  (defsubst emacsw32-send-sys-command (command &optional frame)
    (when window-system
      (w32-send-sys-command command frame)))

  (defun emacsw32-restore-frame ()
    "Restore a minimized frame"
    (interactive)
    (emacsw32-send-sys-command #xf120))

  (defun emacsw32-maximize-frame ()
    "Maximize the current frame"
    (interactive)
    (emacsw32-send-sys-command #xf030))

  (defun emacsw32-make-frame-maximized (frame)
    (when (and emacsw32-max-frames
               window-system
               (let ((minibuffer (frame-parameter frame 'minibuffer))
                     (unsplittable (frame-parameter frame 'unsplittable)))
                 ;;(message "minibuffer=%s, unsplittable=%s" minibuffer unsplittable)
                 (and (window-live-p minibuffer)
                      (not unsplittable))))
      (if (fboundp 'w32-showwindow)
          (let ((SW_MAXIMIZE 3))
            (w32-showwindow frame SW_MAXIMIZE))
        (select-frame frame)
        (emacsw32-maximize-frame))))

  (defun emacsw32-window-setup-maximize ()
    ;;(run-with-idle-timer 2 nil 'emacsw32-maximize-frame)
    (emacsw32-maximize-frame))

  (defcustom emacsw32-max-frames nil
    "Non-nil means maximize new frames."
    :tag "Maximize new frames"
    :set (lambda (symbol value)
           (set-default symbol value)
           (unless noninteractive
             (if value
                 (add-hook 'after-make-frame-functions
                           'emacsw32-make-frame-maximized)
               (remove-hook 'after-make-frame-functions
                            'emacsw32-make-frame-maximized))
             (require 'find-func)
             (unless nil ;(assoc (find-library-name "default") load-history)
               ;; Run with the last hook and idle timer to avoid some
               ;; strange problems at startup
               (when value
                 (add-hook 'window-setup-hook
                           'emacsw32-window-setup-maximize t)))))
    :group 'emacsw32
    :type  'boolean)

  ) ;;when windows-nt


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A windowish frame title
(defvar emacsw32-old-frame-title-format nil)

(defcustom emacsw32-style-frame-title nil
  "If non-nil use a frame title consisting of buffer name followed by Emacs.
Also save the current `frame-title-format' to
`emacsw32-old-frame-title-format'.  When nil restore this value.

Notice: Frame in Emacs terminology is what normally is what a user
would call a window in MS Windows."
  :tag "W32 style window/frame title"
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (progn
               (setq emacsw32-old-frame-title-format frame-title-format)
               (setq frame-title-format "%b - Emacs"))
           (when emacsw32-old-frame-title-format
             (unless (eq frame-title-format emacsw32-old-frame-title-format)
               (setq frame-title-format emacsw32-old-frame-title-format)))))
  :group 'emacsw32)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; The rest is for the emacsw32 mode:

;; (defun emacsw32-kb-vec (char ctrl shft meta)
;;   (unless (< 31 char)
;;     (error "Char should be greater than 31 to avoid confusion"))
;;   (let ((l (list char)))
;;     (when ctrl (setq l (cons 'control l)))
;;     (when shft (setq l (cons 'shift l)))
;;     (when meta (setq l (cons 'meta l)))
;;     (vector l)))

(defvar emacsw32-mode-map (make-sparse-keymap))




(defun emacsw32-add-menus ()
  (when (featurep 'w32shell)
    (let ((k (make-sparse-keymap)))
      (define-key k [explorer-file] '("Explorer with Current &File" . w32shell-explorer-current-file))
      (define-key k [explorer] '("Explorer Here" . w32shell-explorer-here))
      (define-key k [cmd] '("Command Prompt Here" . w32shell-cmd-here))
      (define-key k [divider] '("--"))
      (define-key k [msys-shell]
        (list 'menu-item "MSYS Shell" 'msys-shell
              :help-echo "Run MSYS in a shell buffer"))
      (define-key k [cygwin-shell]
        (list 'menu-item "Cygwin Shell" 'cygwin-shell
              :help-echo "Run Cygwin in a shell buffer"))
      (define-key k [cmd-shell]
        (list 'menu-item "Cmd Shell" 'cmd-shell
              :help-echo "Run Windows Command Prompt in a shell buffer"))
      (define-key-after menu-bar-tools-menu [someshell] (list 'menu-item "W32 Shells" k) 'shell-on-region)))
  (define-key menu-bar-help-menu [emacsw32-sep1] '("--"))
  (define-key menu-bar-help-menu [emacsw32-help]
    (cons "EmacsW32 Introduction" 'emacsw32-show-doc))
;  (define-key-after menu-bar-help-menu [emacsw32-sources]
;    (cons "EmacsW32 Sources" 'emacs-source-info))
  (define-key-after menu-bar-options-menu [emacsw32-customize]
    (cons (concat "Customize EmacsW32 (" (emacsw32-get-version) ") ...")
          'emacsw32-show-custstart)))

(defun emacsw32-show-custstart ()
  "Show start page for emacsw32 customization."
  (interactive)
  (require 'emacsw32-custom)
  (emacsw32-custom-start))


(defconst emacsw32-doc-file
  (convert-standard-filename
   (expand-file-name (concat
                      +exec-directory+
                      "../../EmacsW32/etc/EmacsW32Util.html"))))

(defun emacsw32-show-doc ()
  "Ask Explorer to show emacsw32.el HTML documentation."
  (interactive)
  (if (file-exists-p emacsw32-doc-file)
      (browse-url-of-file emacsw32-doc-file)
    (message "Can't find file %s" emacsw32-doc-file)))


(defun emacsw32-r-short-file-name (begin end)
  "Replace long w32 file name in region with short dito."
  (interactive "rLong w32 file name: ")
  (let* ((long (buffer-substring-no-properties begin end))
         (short (w32-short-file-name long))
         (prompt (format "Replace %s => %s ?  " long short)))
    (if mark-active
        (if short
            (if (y-or-n-p prompt)
                (progn
                  (delete-region begin end)
                  (insert short))
              (message "Canceled"))
          (message "No short w32 file name for '%s'" long))
      (message "Region is not active"))))


;; (customize-option-other-window 'browse-url-browser-function)
(defun browse-url-no-open-windows-browser (url &optional new-window)
  "Same as `browse-url-default-windows-browser' nearly.
You may want to customize `browse-url-browser-function' to this
function if files do not open in your web browser. It is known
that some pc:s have this problem, reason is so far unknown.

The cure seem to be to call `w32-shell-execute' with a nil
argument here instead of \"open\" which the default function
function `browse-url-default-windows-browser' does.

This should be safe unless your computer is strangely configured,
since you are normaly opening .html files with this function."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (eq system-type 'ms-dos)
      (if dos-windows-version
	  (shell-command (concat "start " (shell-quote-argument url)))
	(error "Browsing URLs is not supported on this system"))
    (message "calling (w32-shell-execute nil %S)" url)
    (w32-shell-execute nil url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Execute here

;; Todo: Changing menus here does not adhere to Emacs standard but I
;; think these small changes are ok and useful.
(emacsw32-add-menus)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Ready:
(provide 'emacsw32)

;;; emacsw32.el ends here

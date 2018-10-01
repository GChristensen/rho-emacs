;;; auto-server.el --- Start emacs server from emacsclient
;;
;; Copyright (C) 2006 Free Software Foundation, Inc.
;;
;; Author:  Lennart Borgman <lennart DOT borgman DOT 073 AT student DOT lu DOT se>
;; Created: Thu Nov 16 01:38:51 2006
;; Version: 0.5
;; Last-Updated: Fri Nov 17 01:49:55 2006 (3600 +0100)
;; Keywords:
;; Compatibility:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is NOT YET part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file is used to start emacs server from emacsclient. It is not
;; supposed to be used in any other way.
;;
;; It behaves in a non-standard way for emacs lisp file because it
;; executes some lisp code during load. This code is however only
;; executed if the environment variable EMACSCLIENT_STARTING_SERVER
;; has the value "yes".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'server)

(when (string= "yes" (getenv "EMACSCLIENT_STARTING_SERVER"))
  (let ((server-file (getenv "EMACS_SERVER_FILE")))
    (when server-file
      (setq server-auth-dir (file-name-directory server-file)
            server-name (file-name-nondirectory server-file))))
  (server-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-server.el ends here

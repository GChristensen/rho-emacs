;;; emacsql-sqlite.el --- EmacSQL back-end for SQLite  -*- lexical-binding:t -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/emacsql

;; Package-Version: 3.1.1-git
;; Package-Requires: ((emacs "25.1") (emacsql "3.1.1"))
;; SPDX-License-Identifier: Unlicense

;;; Commentary:

;; This package provides the original EmacSQL back-end for SQLite,
;; which uses a custom binary for communicating with a SQLite database.

;; During package installation an attempt is made to compile the binary.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'eieio)
(require 'emacsql)
(require 'emacsql-sqlite-common)

(emacsql-register-reserved emacsql-sqlite-reserved)

;;; SQLite connection

(defvar emacsql-sqlite-data-root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory where EmacSQL is installed.")

(defvar emacsql-sqlite-executable-path
  (if (memq system-type '(windows-nt cygwin ms-dos))
      "sqlite/emacsql-sqlite.exe"
    "sqlite/emacsql-sqlite")
  "Relative path to emacsql executable.")

(defvar emacsql-sqlite-executable
  (expand-file-name emacsql-sqlite-executable-path
                    (if (or (file-writable-p emacsql-sqlite-data-root)
                            (file-exists-p (expand-file-name
                                            emacsql-sqlite-executable-path
                                            emacsql-sqlite-data-root)))
                        emacsql-sqlite-data-root
                      (expand-file-name
                       (concat "emacsql/" emacsql-version)
                       user-emacs-directory)))
  "Path to the EmacSQL backend (this is not the sqlite3 shell).")

(defvar emacsql-sqlite-c-compilers '("cc" "gcc" "clang")
  "List of names to try when searching for a C compiler.

Each is queried using `executable-find', so full paths are
allowed. Only the first compiler which is successfully found will
used.")

(defclass emacsql-sqlite-connection (emacsql-connection emacsql-protocol-mixin)
  ((file :initarg :file
         :type (or null string)
         :documentation "Database file name.")
   (types :allocation :class
          :reader emacsql-types
          :initform '((integer "INTEGER")
                      (float "REAL")
                      (object "TEXT")
                      (nil nil))))
  (:documentation "A connection to a SQLite database."))

(cl-defmethod initialize-instance :after
  ((connection emacsql-sqlite-connection) &rest _rest)
  (emacsql-sqlite-ensure-binary)
  (let* ((process-connection-type nil)  ; use a pipe
         (coding-system-for-write 'utf-8-auto)
         (coding-system-for-read 'utf-8-auto)
         (file (slot-value connection 'file))
         (buffer (generate-new-buffer " *emacsql-sqlite*"))
         (fullfile (if file (expand-file-name file) ":memory:"))
         (process (start-process
                   "emacsql-sqlite" buffer emacsql-sqlite-executable fullfile)))
    (setf (slot-value connection 'process) process)
    (setf (process-sentinel process)
          (lambda (proc _) (kill-buffer (process-buffer proc))))
    (emacsql-wait connection)
    (emacsql connection [:pragma (= busy-timeout $s1)]
             (/ (* emacsql-global-timeout 1000) 2))
    (emacsql-register connection)))

(cl-defun emacsql-sqlite (file &key debug)
  "Open a connected to database stored in FILE.
If FILE is nil use an in-memory database.

:debug LOG -- When non-nil, log all SQLite commands to a log
buffer. This is for debugging purposes."
  (let ((connection (make-instance 'emacsql-sqlite-connection :file file)))
    (when debug
      (emacsql-enable-debugging connection))
    connection))

(cl-defmethod emacsql-close ((connection emacsql-sqlite-connection))
  "Gracefully exits the SQLite subprocess."
  (let ((process (emacsql-process connection)))
    (when (process-live-p process)
      (process-send-eof process))))

(cl-defmethod emacsql-send-message ((connection emacsql-sqlite-connection) message)
  (let ((process (emacsql-process connection)))
    (process-send-string process (format "%d " (string-bytes message)))
    (process-send-string process message)
    (process-send-string process "\n")))

(cl-defmethod emacsql-handle ((_ emacsql-sqlite-connection) errcode errmsg)
  "Get condition for ERRCODE and ERRMSG provided from SQLite."
  (pcase-let ((`(,_ ,_ ,signal ,errstr)
               (assq errcode emacsql-sqlite-error-codes)))
    (signal (or signal 'emacsql-error)
            (list errmsg errcode nil errstr))))

;;; SQLite compilation

(defun emacsql-sqlite-compile-switches ()
  "Return the compilation switches from the Makefile under sqlite/."
  (let ((makefile (expand-file-name "sqlite/Makefile" emacsql-sqlite-data-root))
        (case-fold-search nil))
    (with-temp-buffer
      (insert-file-contents makefile)
      (goto-char (point-min))
      (cl-loop while (re-search-forward "-D[A-Z0-9_=]+" nil :no-error)
               collect (match-string 0)))))

(defun emacsql-sqlite-compile (&optional o-level async)
  "Compile the SQLite back-end for EmacSQL, returning non-nil on success.
If called with non-nil ASYNC the return value is meaningless."
  (let* ((cc (cl-loop for option in emacsql-sqlite-c-compilers
                      for path = (executable-find option)
                      if path return it))
         (src (expand-file-name "sqlite" emacsql-sqlite-data-root))
         (files (mapcar (lambda (f) (expand-file-name f src))
                        '("sqlite3.c" "emacsql.c")))
         (cflags (list (format "-I%s" src) (format "-O%d" (or o-level 2))))
         (ldlibs (cl-case system-type
                   (windows-nt (list))
                   (berkeley-unix (list "-lm"))
                   (otherwise (list "-lm" "-ldl"))))
         (options (emacsql-sqlite-compile-switches))
         (output (list "-o" emacsql-sqlite-executable))
         (arguments (nconc cflags options files ldlibs output)))
    (cond ((not cc)
           (prog1 nil
             (message "Could not find C compiler, skipping SQLite build")))
          (t (message "Compiling EmacSQL SQLite binary ...")
             (mkdir (file-name-directory emacsql-sqlite-executable) t)
             (let ((log (get-buffer-create byte-compile-log-buffer)))
               (with-current-buffer log
                 (let ((inhibit-read-only t))
                   (insert (mapconcat #'identity (cons cc arguments) " ") "\n")
                   (eql 0 (apply #'call-process cc nil (if async 0 t) t
                                 arguments)))))))))

;;; Ensure the SQLite binary is available

(defun emacsql-sqlite-ensure-binary ()
  "Ensure the EmacSQL SQLite binary is available, signaling an error if not."
  (unless (file-exists-p emacsql-sqlite-executable)
    ;; try compiling at the last minute
    (unless (ignore-errors (emacsql-sqlite-compile 2))
      (error "No EmacSQL SQLite binary available, aborting"))))

(provide 'emacsql-sqlite)

;;; emacsql-sqlite.el ends here

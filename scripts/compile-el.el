;;; compile-el.el --- byte-compile all .el files under packages/  -*- lexical-binding: t; -*-

;; Usage: emacs -Q --batch -l scripts/compile-el.el
;; Run from the repository root.

(require 'bytecomp)

(defconst rho--skip-regexp
  (concat "\\(?:/\\.dir-locals\\.el\\|-autoloads\\.el\\|-pkg\\.el"
          "\\|/\\$[^/]*_append_emacs-init\\.el\\|/tests\\.el\\)\\'")
  "Files that are not meant to be byte-compiled.")

(let* ((root (expand-file-name "packages"))
       (files (seq-remove (lambda (f) (string-match-p rho--skip-regexp f))
                          (directory-files-recursively root "\\.el\\'")))
       (ok 0)
       (failed nil))
  ;; Let cross-file `require's resolve.
  (dolist (f files)
    (add-to-list 'load-path (file-name-directory f)))
  (dolist (f files)
    (condition-case err
        (if (byte-compile-file f)
            (setq ok (1+ ok))
          (push f failed))
      (error
       (message "ERROR compiling %s: %s" f (error-message-string err))
       (push f failed))))
  (message "\nCompiled %d of %d files, %d failed." ok (length files) (length failed))
  (dolist (f (reverse failed))
    (message "  FAILED: %s" (file-relative-name f root)))
  (kill-emacs (if failed 1 0)))

;;; usb-setup.el --- Setup for using Emacs from an usb stick
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-04-10 Fri
;; Version: 0.5
;; Last-Updated: 2009-05-02 Sat
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   Cannot open load file: usb-setup.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file is for using Emacs+EmacsW32 from an usb stick or
;; similar. The idea is from
;;
;;  http://at-aka.blogspot.com/2006/06/portable-emacs-22050-on-usb.html
;;
;; It allows you to have your own .emacs and site-lisp directory with you.
;; To use it do:
;;
;;  - Make a directory U:/home/ on your usb stick (here U: is just the
;;    drive letter you happen to have for your usb stick).
;;
;;  - Put your .emacs there.
;;
;;  - Add (require 'usb-setup) to the end of site-start.el on the usb
;;    stick.
;;
;; You can optionally have an extra site-lisp dir under U:/home/.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(defvar usb-drive-letter (substring data-directory 0 3))
(defvar usb-home-dir (concat usb-drive-letter "home/"))
(defvar usb-site-lisp-dir (expand-file-name "site-lisp" usb-home-dir))
(let ((dir (delete nil (mapcar (lambda (f)
                                 (unless (string-match "\\.elc?\\'" f) f))
                               (cddr (directory-files usb-site-lisp-dir t))))))
  ;; .emacs etc location on usb stick
  (setenv "HOME" usb-home-dir)

  ;; Additional site-lisp dir on usb stick
  (setq load-path (cons usb-site-lisp-dir load-path))
  (setq load-path (append dir load-path)))

(provide 'usb-setup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; usb-setup.el ends here

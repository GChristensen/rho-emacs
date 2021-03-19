;; package:ConvKit ; please do not remove or edit these comments
;; Basic Convenience Kit setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ELPA configuration
(defconst rho--elpa-dir (concat rho--emacs-home "/elpa"))
                     
(if (not (file-exists-p rho--elpa-dir))
  (make-directory rho--elpa-dir))

(add-to-list 'load-path (concat rho--root-dir "/lisp/base/bookmarks"))
(require 'bookmark+)

;; better buffer names distinction
(require 'uniquify)

;; ido mode
(require 'ido)

;; desktop mode
(when (not rho--precomp?)
  (desktop-save-mode 1))

;; C-like mode indentation setup
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(dolist (hook '(lisp-mode-hook
                c-mode-common-hook
                java-mode-hook
                clojure-mode-hook
                scheme-mode-hook
                python-mode-hook
                nisis-mode-hook
         ))
  (add-hook hook (lambda ()
                   (interactive)
                   (linum-mode t)
                   (show-paren-mode 1)
                 ))) 

(custom-set-variables
 '(default-tab-width 4)
 '(indent-tabs-mode nil)
 '(column-number-mode t)
 '(fill-column 79)
 '(c-default-style "linux")
 '(c-basic-offset 4)
 '(scroll-step 1)
 '(scroll-conservatively 100)
 '(scroll-preserve-screen-position t)
 '(uniquify-buffer-name-style 'post-forward)
 '(uniquify-separator ":")
 '(global-visual-line-mode t)
 '(iswitchb-mode t)
 '(fido-mode t)
 '(tabbar-use-images nil)
 '(ido-mode t)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-show-dot-for-dired t)
 '(confirm-nonexistent-file-or-buffer nil)
 '(save-place-mode t) 
 )

(custom-set-faces
 `(default ((t (:family ,(if rho--portable? "Courier New" "Hack") :foundry "outline" :slant normal :weight normal :height 100 :width normal)))))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(fset 'yes-or-no-p 'y-or-n-p)

(easy-menu-add-item  nil '("((")
  ["Bookmarks" bookmark-bmenu-list t] "Color Themes")

;; remove grep output header ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-grep-header ()
  (save-excursion
    (with-current-buffer grep-last-buffer
      (goto-line 5)
      (narrow-to-region (point) (point-max)))))

(defvar delete-grep-header-advice
  (ad-make-advice
   'delete-grep-header nil t
   '(advice lambda () (delete-grep-header))))

(defun add-delete-grep-header-advice (function)
  (ad-add-advice function delete-grep-header-advice 'after 'first)
  (ad-activate function))

(mapc 'add-delete-grep-header-advice
      '(grep lgrep grep-find rgrep zrgrep))


;; org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-adapt-indentation t)

(require 'org-bullets)
(setq org-bullets-bullet-list '("◉" "○" "■" "□" "▲"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic Convenience Kit setup ;;

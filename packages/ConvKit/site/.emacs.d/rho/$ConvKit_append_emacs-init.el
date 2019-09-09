;; package:ConvKit ; please do not remove or edit these comments
;; Basic Convenience Kit setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; misk settings

(add-to-list 'load-path (concat +rho-dir+ "/site/.emacs.d/basic/bookmarks"))

(fset 'yes-or-no-p 'y-or-n-p)

;; better buffer names distinction
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; ido mode
(require 'ido)
(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-show-dot-for-dired t
      confirm-nonexistent-file-or-buffer nil)

;; desktop mode
(when (not +precomp+)
  (desktop-save-mode 1))  

;; winsav
;(require 'winsav)

;; hightlight long lines
;(require 'highlight-80+)

(require 'column-marker)

;; highlight symbol
(require 'highlight-symbol)

(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [(shift f3)] 'highlight-symbol-next)
(global-set-key [(meta f3)] 'highlight-symbol-prev)

;; tabs module
(require 'tabbar)
(tabbar-mode t)

;; ELPA configuration
(defconst +elpa-dir+ (concat +emacs-home+ "/elpa"))
                     
(if (not (file-exists-p +elpa-dir+))
  (make-directory +elpa-dir+))

(create-default-get-version 'convkit)

;; C-like mode indentation setup
(require 'cc-mode)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(ido-mode t)

(save-place-mode t) 

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(dolist (hook '(lisp-mode-hook
                c-mode-common-hook
                java-mode-hook
                clojure-mode-hook
                scheme-mode-hook
                python-mode-hook
                scala-mode-hook
                haskell-mode-hook
                groovy-mode-hook
                nisis-mode-hook
         ))
  (add-hook hook (lambda ()
                   (interactive)
                   (linum-mode t)
                   (column-marker-2 80)
                   (highlight-symbol-mode t)
                   (show-paren-mode 1)
;                   (yas-global-mode t)
                 ))) 

(setq-default indent-tabs-mode nil)

(require 'bookmark+)

(easy-menu-add-item  nil '("((")
  ["Bookmarks" bookmark-bmenu-list t] "Set Home Directory")

(custom-set-variables
 ;'(yas/prompt-functions '(yas/ido-prompt yas/x-prompt yas/completing-prompt yas/no-prompt))
 '(custom-enabled-themes (quote (arjen)))
 '(default-tab-width 4)
 '(indent-tabs-mode nil)
 '(column-number-mode t)
 '(fill-column 79)
 '(c-default-style "linux")
 '(c-basic-offset 4)
 '(scroll-step 1)
 '(scroll-conservatively 100)
 '(scroll-preserve-screen-position t)
 '(global-visual-line-mode t)
 '(iswitchb-mode t)
 '(tabbar-use-images nil))

(custom-set-faces
 `(default ((t (:family ,(if +portable?+ "Courier New" "Hack") :foundry "outline" :slant normal :weight normal :height 100 :width normal)))))

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


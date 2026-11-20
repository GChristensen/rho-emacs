;; package:ConvKit ; please do not remove or edit these comments
;; Basic Convenience Kit setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key minibuffer-local-completion-map (kbd "SPC") 'self-insert-command)

;; bookmark+ is loaded on first use of the bookmark library
(add-to-list 'load-path (concat rho--root-dir "/lisp/base/bookmarks"))
(with-eval-after-load 'bookmark
  (require 'bookmark+))

;; better buffer names distinction
(require 'uniquify)

;; ido mode
(require 'ido)

;; desktop mode
(when (not rho--precomp?)
  (desktop-save-mode 1))

;; C-like mode indentation setup
(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent))

(show-paren-mode 1)

(dolist (hook '(lisp-mode-hook
                c-mode-common-hook
                java-mode-hook
                clojure-mode-hook
                scheme-mode-hook
                python-mode-hook))
  (add-hook hook #'display-line-numbers-mode))

(setq-default tab-width 4)

(custom-set-variables
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
 `(default ((t (:family ,(if (and (not rho--portable?)
                                  (find-font (font-spec :name "Hack")))
                             "Hack"
                           "Courier New")
                :foundry "outline" :slant normal :weight normal :height 100 :width normal)))))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(fset 'yes-or-no-p 'y-or-n-p)

(easy-menu-add-item  nil '("((")
  ["Bookmarks" bookmark-bmenu-list t] "Color Themes")

;; remove grep output header ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rho--delete-grep-header (&rest _)
  (let ((buffer (bound-and-true-p grep-last-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (forward-line 4)
          (narrow-to-region (point) (point-max)))))))

(dolist (function '(grep lgrep grep-find rgrep zrgrep))
  (advice-add function :after #'rho--delete-grep-header))


;; org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-adapt-indentation t)

;; org-modern and its compat dependency are bundled
(add-to-list 'load-path (concat rho--root-dir "/lisp/base/compat"))
(add-to-list 'load-path (concat rho--root-dir "/lisp/base/org-modern"))
(setq org-modern-star '("◉" "○" "■" "□" "▲"))
(with-eval-after-load 'org
  (require 'org-modern)
  (global-org-modern-mode 1))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic Convenience Kit setup ;;

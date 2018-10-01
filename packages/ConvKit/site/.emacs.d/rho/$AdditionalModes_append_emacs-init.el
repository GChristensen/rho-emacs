;; package:AdditionalModes ; please do not remove or edit these comments
;; Additional modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq speedbar-mode-specific-contents-flag t)
(setq speedbar-use-images nil)
(setq speedbar-show-unknown-files t)

(defconst +yas-dir+ (concat +emacs-home+ "/snippets"))

(if (not (file-exists-p +yas-dir+))
  (make-directory +yas-dir+))

(when (and (not +anyhome?+) (display-graphic-p))
  (yas-global-mode t))

(easy-menu-add-item  nil '("((")
  ["Bookmarks" bookmark-bmenu-list t] "Set Home Directory")

(easy-menu-add-item  nil '("((")
  ["Speedbar" sr-speedbar-open t] "Set Home Directory")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional modes ;;


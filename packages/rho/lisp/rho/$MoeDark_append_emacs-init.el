;; package:MoeDark ; please do not remove or edit these comments
;; Additional modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (concat rho--root-dir "/lisp/base/powerline"))
(require 'powerline)

(add-to-list 'custom-theme-load-path (concat custom-theme-directory "/moe-theme/"))
(add-to-list 'load-path (concat custom-theme-directory "/moe-theme/"))
(require 'moe-theme)

(setq moe-theme-highlight-buffer-id t)

(moe-theme-set-color 'purple)

(moe-dark)
(powerline-moe-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional modes ;;


;; package:MoeLight ; please do not remove or edit these comments
;; Additional modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (concat rho--root-dir "/lisp/base/powerline"))
(require 'powerline)

(require 'moe-theme)

(setq moe-theme-highlight-buffer-id t)

(moe-theme-set-color 'purple)

(moe-light)
(powerline-moe-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional modes ;;


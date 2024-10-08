;; package:MoeDark ; please do not remove or edit these comments
;; Additional modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (concat rho--root-dir "/lisp/base/powerline"))
(require 'powerline)

(require 'moe-theme)

(setq moe-theme-highlight-buffer-id t)

(setq moe-theme-modeline-color 'purple)
(setq moe-theme-highlight-buffer-id t)

(moe-dark)
(powerline-moe-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Additional modes ;;


;; package:Clojure ; please do not remove or edit these comments
;; Environment setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PATH" (concat +home-dir+ "/.lein;" (getenv "PATH")))

(setenv "JAVA_OPTS" (concat "-Duser.home=" +home-dir+ ""))
(setenv "LEIN_JVM_OPTS" (concat "-Duser.home=" +home-dir+ ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Environment setup ;;


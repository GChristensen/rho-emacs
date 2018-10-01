;; package:groovy; please do not remove or edit these comments
;; Environment setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PATH" (concat +rho-dir+ "/bin/groovy/bin;/bin/grails/bin;" (getenv "PATH")))

(setenv "GROOVY_HOME" (concat +rho-dir+ "/bin/groovy"))
(setenv "GRAILS_HOME" (concat +rho-dir+ "/bin/grails"))

(setenv "JAVA_OPTS" (concat "-Duser.home=" +home-dir+ ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Environment setup ;;


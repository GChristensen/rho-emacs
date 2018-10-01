@echo off

@rem package:Clojure

setlocal enabledelayedexpansion

@set CP=%RHO_DIR%\bin\clojure\clojure.jar


@for /R "%HOME%\clojure\lib" %%a in (*.jar) do (
 set CP=!CP!;%%a
)

@endlocal & @set lispx.clojure.classpath=%CP%
@set lispx.clojure.args="-Duser.home=%HOME%" clojure.main

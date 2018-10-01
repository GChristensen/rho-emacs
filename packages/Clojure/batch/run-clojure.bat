@echo off
setLocal EnableDelayedExpansion

set CLASSPATH=%RHO_DIR%/bin/clojure/clojure.jar"
for /R "%HOME%\clojure\lib" %%a in (*.jar) do (
  set CLASSPATH=!CLASSPATH!;%%a
)
set CLASSPATH=!CLASSPATH!"

java -Duser.home="%HOME%" -classpath "%CLASSPATH%" clojure.main --repl
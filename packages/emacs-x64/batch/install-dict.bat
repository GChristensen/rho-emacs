copy /Y *.* "%RHO_DIR%\bin\utils\aspell\lib\aspell-0.60"
del "%RHO_DIR%\bin\utils\aspell\lib\aspell-0.60\%1.alias"
echo add %2.multi>>"%RHO_DIR%\bin\utils\aspell\lib\aspell-0.60\%1.alias"
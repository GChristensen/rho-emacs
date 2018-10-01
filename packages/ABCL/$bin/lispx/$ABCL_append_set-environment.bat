@rem package:ABCL

@set lispx.abcl.classpath=%RHO_DIR%\bin\abcl\abcl.jar
@set lispx.abcl.load=%RHO_DIR%\site\abcl-init.lisp
@set lispx.abcl.args="-Duser.home=%HOME%" org.armedbear.lisp.Main --noinform


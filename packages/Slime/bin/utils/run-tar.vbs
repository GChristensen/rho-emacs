
Set objShell = CreateObject("WScript.Shell")
Set objFSO = CreateObject("Scripting.FileSystemObject")

commandLine = "bsdtar -C """ & WScript.Arguments(1) & """ -xzvf """ & WScript.Arguments(0) & """"

Set objProcess = objShell.Exec(commandLine)

Set objFile = objFSO.CreateTextFile(WScript.Arguments(2), True)
Set objStdErr = objProcess.StdErr

Do Until objStdErr.AtEndOfStream
  objFile.WriteLine objStdErr.ReadLine()  
Loop
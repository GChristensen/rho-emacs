@echo off
rem Installs all fonts from this directory using FontReg.exe (/copy copies from the current directory).
rem Self-elevates because FontReg requires administrator rights.

net session >nul 2>&1
if errorlevel 1 (
    echo Requesting administrator rights...
    powershell -NoProfile -Command "Start-Process -FilePath '%~f0' -Verb RunAs"
    exit /b
)

cd /d "%~dp0"

set "FONTREG=%~dp0..\..\rho\utils\FontReg.exe"
if not exist "%FONTREG%" set "FONTREG=%~dp0..\..\..\utils\FontReg.exe"
if not exist "%FONTREG%" (
    echo FontReg.exe not found.
    pause
    exit /b 1
)

"%FONTREG%" /copy
echo FontReg exit code: %errorlevel%
pause

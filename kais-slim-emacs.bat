@echo off
if exist "%USERPROFILE%\.emacs" (
    echo [W] Found a .emacs file, please be aware that it might interfere with this configuration.
)
set DIR=%~dp0
C:\Users\ahk2hi\bin\emacs-30.1\bin\emacs.exe --init-directory=%DIR% --load=%DIR%init.el %*

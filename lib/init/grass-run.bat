@echo off
rem Batch file to launch GRASS commands

rem Change console title to name of module
title GRASS: %1

rem Force command line startup mode
set GRASS_UI_TERM=1

rem Run command
"%*"

title GRASS: %1 Done.
if %errorlevel% == 1 goto error

echo.
echo %1 complete.
pause
:end
exit %errorlevel%

:error
echo -----
echo ERROR: %1 exited abnormally.
echo -----
pause
goto end

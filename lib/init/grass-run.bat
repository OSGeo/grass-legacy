@echo off
rem Batch file to launch GRASS commands

rem Change console title to name of module
title GRASS: %1

rem Force command line startup mode
set GRASS_UI_TERM=1

rem Run command
"%*"

if %errorlevel% == 1 goto error

:end
title GRASS: %1 Done.
pause
exit %errorlevel%

:error
echo -----
echo ERROR: %1 exited abnormally.
echo -----
goto end

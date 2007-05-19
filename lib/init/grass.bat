@echo off
rem #########################################################################
rem #
rem #		GRASS Initialization
rem #
rem #########################################################################

set WINGISBASE=GISBASE_VALUE
"%WINGISBASE%\etc\init.bat" %*

rem *******Environment variables***********
rem Uncomment and set the following values if they differ from the indicated default

rem Directory where your .grassrc6 file will be stored
rem HOME=%USERPROFILE%

rem Name of the wish (Tk) executable
rem GRASS_WISH=wish.exe

rem Path to the shell command
rem GRASS_SH=c:\msys\1.0\bin\sh.exe

rem Path to your web browser
rem GRASS_HTML_BROWSER=%SYSTEMDRIVE%/PROGRA~1/INTERN~1/IEXPLORE.EX

rem Path to the proj files (notably the epsg projection list)
rem GRASS_PROJSHARE=/c/grass/share/proj

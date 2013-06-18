@echo off
rem #########################################################################
rem
rem  MODULE:   	GRASS Initialization
rem  AUTHOR(S):	Paul Kelly
rem
rem  PURPOSE:  	The source file for this batch script is lib/init/init.bat.
rem             It sets up some environment variables, default GISRC file
rem             if necessary, etc. prior to starting GRASS proper.
rem             It is intended to be a native Windows replacement for Init.sh,
rem             but does not (yet) contain all the same functionality.
rem
rem             In particular also, GUI mode prints nothing to the terminal
rem             and does not expect or provide an interactive terminal
rem             running in addition to the GUI display.
rem 
rem  COPYRIGHT: (C) 2006-2013 by the GRASS Development Team
rem
rem             This program is free software under the GNU General Public
rem   	    	License (>=v2). Read the file COPYING that comes with GRASS
rem   	    	for details.
rem
rem #########################################################################

set SAVEPATH=%PATH%
rem
rem The .bat files in the \bin directory are what execute the scripts
rem  on Windows. The \scripts dir is added to the PATH automatically by
rem  the wxGUI; for Command Line mode and GIS.m, shell scripts called from
rem  other shell scripts need to have %GISBASE%\scripts in the PATH.
rem

if exist "%APPDATA%\GRASS6\env.bat" (
   	call %APPDATA%\GRASS6\env.bat
)
if "%GRASS_ADDON_PATH%"=="" set GRASS_ADDON_PATH=%APPDATA%\GRASS6\addons
PATH=%GISBASE%\bin;%GISBASE%\lib;%GRASS_ADDON_PATH%;%PATH%

set GIS_LOCK=1
set GRASS_VERSION=GRASS_VERSION_NUMBER
if "%HOME%"=="" set HOME=%USERPROFILE%
set WINGISRC=%APPDATA%\GRASS6\grassrc6
rem Create missing folder
if not exist "%APPDATA%\GRASS6" (
	md "%APPDATA%\GRASS6"
)
rem Make sure %GISRC% is set so g.dirseps will work
rem (not actually used)
set GISRC=junk

rem Generate GISBASE by converting dirsep characters from \ to /
FOR /F "usebackq delims==" %%i IN (`g.dirseps -g "%GISBASE%"`) DO @set GISBASE=%%i

if not "%LANG%"=="" goto langset
FOR /F "usebackq delims==" %%i IN (`"%GISBASE%\etc\winlocale"`) DO @set LANG=%%i
:langset

if "%GRASS_WISH%"=="" set GRASS_WISH=wish.exe
if "%GRASS_SH%"=="" set GRASS_SH=c:\msys\1.0\bin\sh.exe

rem Should do something with "assoc .html" and ftype here but would require
rem a new g.manual.bat too so leaving it like this for now...
if "%GRASS_HTML_BROWSER%"=="" set GRASS_HTML_BROWSER=explorer

if "%GRASS_PROJSHARE%"=="" set GRASS_PROJSHARE=CONFIG_PROJSHARE

rem Add python scripts to the PATHEXT variable
set PATHEXT=%PATHEXT%;.PY

if "%1" == "-version" goto displaylicence
if "%1" == "-v" goto displaylicence

if "%1" == "-text" goto settextmode
:aftertextcheck

if "%1" == "-tcltk" goto setguimode
if "%1" == "-wxpython" goto setwxmode
if "%1" == "-wx" goto setwxmode
if "%1" == "-gui" goto setguimode

:afterguicheck

if exist "%WINGISRC%" (
   set HAVE_GISRC=true
   goto aftercreategisrc
)

set HAVE_GISRC=false
rem Create an initial GISRC file based on current directory
"%GISBASE%\etc\echo" "GISDBASE: %USERPROFILE%" | g.dirseps -g > "%WINGISRC%"
"%GISBASE%\etc\echo" "LOCATION_NAME: <UNKNOWN>" >> "%WINGISRC%"
"%GISBASE%\etc\echo" "MAPSET: <UNKNOWN>" >> "%WINGISRC%"

:aftercreategisrc

rem Now set the real GISRC
FOR /F "usebackq delims==" %%i IN (`g.dirseps -g "%WINGISRC%"`) DO @set GISRC=%%i

rem Fetch the language setting from user's prefs
FOR /F "usebackq delims==" %%i IN (`g.gisenv "get=LANG"`) DO @set LANG=%%i

rem Set GRASS_GUI

if "%GRASS_GUI%" == "" (
  FOR /F "usebackq delims==" %%i IN (`g.gisenv "get=GRASS_GUI"`) DO @set GRASS_GUI=%%i
) else (
  g.gisenv "set=GRASS_GUI=%GRASS_GUI%"
)

rem Set wxGUI as default if not specified elsewhere
if "%GRASS_GUI%"=="" set GRASS_GUI=wxpython

rem Clean out old .tmp files from the mapset
if "%HAVE_GISRC%"=="true" (
  "%GISBASE%\etc\clean_temp" > NUL:
)
set HAVE_GISRC=

rem why doesn't it like if () else ()?
if not "%PYTHONPATH%" == "" set PYTHONPATH=%PYTHONPATH%;%GISBASE%\etc\python
if "%PYTHONPATH%" == "" set PYTHONPATH=%GISBASE%\etc\python


if "%GRASS_GUI%"=="wxpython" goto wxpython

PATH=%PATH%;%GISBASE%\scripts
if "%GRASS_GUI%"=="text" goto text


rem Tcl/Tk GUI setup
if not "%GRASS_WISH%"=="" (
	"%GRASS_WISH%" "%GISBASE%\etc\gis_set.tcl"
) else (
	"%GISBASE%\etc\gis_set.tcl"
)

rem This doesn't seem to work; don't understand return codes from gis_set.tcl PK
rem if return ok, gis.m start:
if %errorlevel% == 2 goto exitinit

rem Does line 42 above mean that GRASS_WISH will always be set?
if not "%GRASS_WISH%"=="" (
	start /b "GRASS Tcl/Tk" "%GRASS_WISH%" "%GISBASE%\etc\gm\gm.tcl"
) else (
	start /b "GRASS Tcl/Tk" "%GISBASE%\etc\gm\gm.tcl"
)

rem Will redirecting output to NUL hide legitamite error messages, harming debugging?
"%GISBASE%\etc\clean_temp" > NUL:

goto exitinit

:wxpython

set PYTHONPATH=%PYTHONPATH%;%GISBASE%\etc\wxpython

if "%GRASS_PYTHON%"=="" set GRASS_PYTHON=python

"%GRASS_PYTHON%" "%GISBASE%/etc/wxpython/gis_set.py"
if %errorlevel% == 2 goto exitinit
"%GRASS_PYTHON%" "%GISBASE%/etc/wxpython/wxgui.py"

goto exitinit

:text

"%GISBASE%\etc\set_data"

if %errorlevel% == 1 goto exitinit

rem Get LOCATION_NAME to use in prompt
FOR /F "usebackq delims==" %%i IN (`g.gisenv "get=LOCATION_NAME"`) DO @set LOCATION_NAME=%%i

type "%GISBASE%\etc\welcome"

"%GISBASE%\etc\echo" ""
"%GISBASE%\etc\echo" "GRASS homepage:                          http://grass.osgeo.org/"
"%GISBASE%\etc\echo" "This version running thru:               Windows Command Shell (cmd.exe)"
"%GISBASE%\etc\echo" "When ready to quit enter:                exit"
"%GISBASE%\etc\echo" "Help is available with the command:      g.manual -i"
"%GISBASE%\etc\echo" "See the licence terms with:              g.version -c"
rem                  "Start the GUI with:                      g.gui wxpython"
"%GISBASE%\etc\echo" ""

prompt GRASS %GRASS_VERSION% $C%LOCATION_NAME%$F$G 

cmd.exe

prompt
goto exitinit

:displaylicence

type "%GISBASE%\etc\license"
goto exitinit

:settextmode

set GRASS_GUI=text
shift

goto aftertextcheck

:setguimode
set GRASS_GUI=tcltk
shift

goto afterguicheck

:setwxmode
set GRASS_GUI=wxpython
shift

goto afterguicheck

:exitinit

rem Clean out old .tmp files from the mapset
"%GISBASE%\etc\clean_temp" > NUL:

set PATH=%SAVEPATH%
set SAVEPATH=
exit /b

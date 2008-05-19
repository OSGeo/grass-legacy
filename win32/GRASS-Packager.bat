@echo off

rem --------------------------------------------------------------------------------------------------------------------------
rem Set the script variables
rem --------------------------------------------------------------------------------------------------------------------------

set GRASS_RELEASE_PACKAGE_DIR=.\GRASS-Release-Package
set GRASS_6_DEV_PACKAGE_DIR=.\GRASS-6-Dev-Package
set GRASS_7_DEV_PACKAGE_DIR=.\GRASS-7-Dev-Package

set GRASS_RELEASE_INSTALL_FOLDER=c:\msys\local\grass-6.3.0
set GRASS_6_DEV_INSTALL_FOLDER=c:\msys\local\grass-6-svn
set GRASS_7_DEV_INSTALL_FOLDER=c:\msys\local\grass-7-svn

@echo -----------------------------------------------------------------------------------------------------------------------
@echo Self Contained GRASS Automated Packager
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Edited by: Marco Pasetti
@echo Last Update: 17 May 2008
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Select the GRASS version to pack:
@echo.
@echo 1. Current GRASS Release Version
@echo.
@echo 2. Current GRASS-6 Development Version
@echo.
@echo 3. Current GRASS-7 Development Version
@echo.

set /p SELECTION=Enter your selection (1/2/3):

if %SELECTION%==1 (
set PACKAGE_DIR=%GRASS_RELEASE_PACKAGE_DIR%
set GRASS_PREFIX=%GRASS_RELEASE_INSTALL_FOLDER%
)

if %SELECTION%==2 (
set PACKAGE_DIR=%GRASS_6_DEV_PACKAGE_DIR%
set GRASS_PREFIX=%GRASS_6_DEV_INSTALL_FOLDER%
)

if %SELECTION%==3 (
set PACKAGE_DIR=%GRASS_7_DEV_PACKAGE_DIR%
set GRASS_PREFIX=%GRASS_7_DEV_INSTALL_FOLDER%
)

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Remove the previous Selected Package and create a new PACKAGE_DIR
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

pause

if exist %PACKAGE_DIR% rmdir /S/Q %PACKAGE_DIR%
mkdir %PACKAGE_DIR%

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy %GRASS_PREFIX% content to PACKAGE_DIR
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

xcopy %GRASS_PREFIX% %PACKAGE_DIR% /S/V/F

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy Extralibs to PACKAGE_DIR\extralib
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

mkdir %PACKAGE_DIR%\extralib

copy c:\msys\local\bin\*.dll %PACKAGE_DIR%\extralib
copy c:\msys\local\sqlitel\bin\*.dll %PACKAGE_DIR%\extralib
copy c:\msys\local\pgsql\lib\libpq.dll %PACKAGE_DIR%\extralib

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy Extrabins to PACKAGE_DIR\extrabin
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

mkdir %PACKAGE_DIR%\extrabin
copy c:\msys\local\bin\*.exe %PACKAGE_DIR%\extrabin
copy c:\msys\local\sqlite\bin\*.exe %PACKAGE_DIR%\extrabin

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy SQLite content to PACKAGE_DIR\sqlite
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

xcopy c:\msys\local\sqlite %PACKAGE_DIR%\sqlite /S/V/F/I

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy shared PROJ.4 files to PACKAGE_DIR\proj
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

xcopy c:\msys\local\share\proj %PACKAGE_DIR%\proj /S/V/F/I

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy Tcl/Tk content to PACKAGE_DIR\tcl-tk
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

mkdir %PACKAGE_DIR%\tcl-tk
mkdir %PACKAGE_DIR%\tcl-tk\include
mkdir %PACKAGE_DIR%\tcl-tk\lib
mkdir %PACKAGE_DIR%\tcl-tk\lib\tcl8.5
mkdir %PACKAGE_DIR%\tcl-tk\lib\tk8.5

xcopy c:\msys\local\tcl-tk\bin %PACKAGE_DIR%\tcl-tk\bin /S/V/F/I

copy c:\msys\local\tcl-tk\include\*.h %PACKAGE_DIR%\tcl-tk\include

copy c:\msys\local\tcl-tk\lib\tcl8.5\*.tcl %PACKAGE_DIR%\tcl-tk\lib\tcl8.5
copy c:\msys\local\tcl-tk\lib\tcl8.5\tclIndex %PACKAGE_DIR%\tcl-tk\lib\tcl8.5

copy c:\msys\local\tcl-tk\lib\tk8.5\*.tcl %PACKAGE_DIR%\tcl-tk\lib\tk8.5
copy c:\msys\local\tcl-tk\lib\tk8.5\tclIndex %PACKAGE_DIR%\tcl-tk\lib\tk8.5

xcopy c:\msys\local\tcl-tk\lib\tk8.5\ttk %PACKAGE_DIR%\tcl-tk\lib\tk8.5\ttk /S/V/F/I

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy MSYS files to PACKAGE_DIR\msys
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

mkdir %PACKAGE_DIR%\msys

copy c:\msys\* %PACKAGE_DIR%\msys

xcopy c:\msys\bin %PACKAGE_DIR%\msys\bin /S/V/F/I
xcopy c:\msys\doc %PACKAGE_DIR%\msys\doc /S/V/F/I
xcopy c:\msys\etc %PACKAGE_DIR%\msys\etc /S/V/F/I
xcopy c:\msys\info %PACKAGE_DIR%\msys\info /S/V/F/I
xcopy c:\msys\lib %PACKAGE_DIR%\msys\lib /S/V/F/I
xcopy c:\msys\man %PACKAGE_DIR%\msys\man /S/V/F/I

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Packaging Completed
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.
pause
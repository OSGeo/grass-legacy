@echo off

rem ----------------------------------------------------------------------------------------------------------
rem Set the script variables
rem ----------------------------------------------------------------------------------------------------------

set TRUNK_INSTALL_FOLDER=c:\msys\local\grass-trunk
set TEST_PACKAGE_DIR=.\GRASS-Trunk-Package

set RELEASE_INSTALL_FOLDER=c:\msys\local\grass-6.3.0
set RELEASE_PACKAGE_DIR=.\GRASS-Release-Package

@echo -----------------------------------------------------------------------------------------------------------------------
@echo Self Contained GRASS Automated Packager
@echo -----------------------------------------------------------------------------------------------------------------------
@echo GRASS Version: Current SVN Trunk/Release Tarball
@echo.
@echo Edited by: Marco Pasetti
@echo Last Update: 21 April 2008
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Select if you want to create a Test Package from the Current SVN Trunk Build
@echo or a Release Package from the Current GRASS Release Tarball Build
@echo.
@echo 1. Current GRASS SVN Trunk Build
@echo.
@echo 2. Current GRASS Release Tarball Build
@echo.

set /p UPDATE_TYPE=Enter your selection (1/2):

if %UPDATE_TYPE%==1 (
set PACKAGE_DIR=%TEST_PACKAGE_DIR%
set GRASS_PREFIX=%TRUNK_INSTALL_FOLDER%
)

if %UPDATE_TYPE%==2 (
set PACKAGE_DIR=%RELEASE_PACKAGE_DIR%
set GRASS_PREFIX=%RELEASE_INSTALL_FOLDER%
)

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Create PACKAGE_DIR
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

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
copy c:\msys\local\pgsql\lib\libpq.dll %PACKAGE_DIR%\extralib

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy Extrabins to PACKAGE_DIR\extrabin
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

mkdir %PACKAGE_DIR%\extrabin
copy c:\msys\local\bin\*.exe %PACKAGE_DIR%\extrabin

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy SQLite content to PACKAGE_DIR\sqlite
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

mkdir %PACKAGE_DIR%\sqlite
xcopy c:\msys\local\sqlite %PACKAGE_DIR%\sqlite /S/V/F

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy shared PROJ.4 files to PACKAGE_DIR\proj
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

mkdir %PACKAGE_DIR%\proj
xcopy c:\msys\local\share\proj %PACKAGE_DIR%\proj /S/V/F

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy Tcl/Tk content to PACKAGE_DIR\tcl-tk
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

mkdir %PACKAGE_DIR%\tcl-tk
xcopy c:\msys\local\tcl-tk %PACKAGE_DIR%\tcl-tk /S/V/F

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Copy MSYS files to PACKAGE_DIR\msys
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.

mkdir %PACKAGE_DIR%\msys
mkdir %PACKAGE_DIR%\msys\bin
mkdir %PACKAGE_DIR%\msys\doc
mkdir %PACKAGE_DIR%\msys\etc

copy c:\msys\* %PACKAGE_DIR%\msys
xcopy c:\msys\bin %PACKAGE_DIR%\msys\bin /S/V/F
xcopy c:\msys\doc %PACKAGE_DIR%\msys\doc /S/V/F
xcopy c:\msys\etc %PACKAGE_DIR%\msys\etc /S/V/F

@echo.
@echo -----------------------------------------------------------------------------------------------------------------------
@echo Packaging Completed
@echo -----------------------------------------------------------------------------------------------------------------------
@echo.
pause
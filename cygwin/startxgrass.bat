@echo off
REM Startscript for GRASS 5 with cygwin and XFree86
REM Please adapt to your installation!
REM You can not avoid to pop up a command window on invocation of
REM this batch file. Sorry.
REM Do not close this window, as this terminates your GRASS session!
REM 
REM You may create a shortcut on the windows desktop to this batch file
REM and choose "start minimized" or such from the context menue of the
REM shortcut. 
REM 
REM Andreas Lange, $Date$
REM adapted from the file StartGrass of John Huddleston
REM with additions from Malcolm Blue
REM with even more additions and customizations from Andreas Lange
REM
REM ----- Window manager selection
REM  
REM Available window managers with XFree86 for cygwin:
REM  - twm ..................... twm
REM  - vtwm .................... vtwm
REM  - xfwm/xfce ............... xfwm
REM  - blackbox ................ blackbox
REM  - mwm (from lesstiff) ..... mwm
REM You are on your own to install and configure the
REM window manager of your choice! Default window manager
REM is twm, as its from the standard distribution. 
REM
SET WM=twm
REM
REM ----- Path to your CYGWIN ROOT
REM 
set CYGWIN=\cygwin
REM
REM ----- set your resolution here (-28 -38):
REM 
REM color depth: x8, x16, x24, x32
REM for 1200x1600
REM set RES=-screen 0 1172x1562x16
REM for 1280x1024
set RES=-screen 0 1180x986x24
REM for 1152x864
REM set RES=-screen 0 1124x826x16
REM for 1024x768
REM set RES=-screen 0 996x730x16
REM for 800x600
REM set RES=-screen 0 772x562x16
REM 
REM ----- localized settings
REM 
set XLANG=de
REM set XLANG=fi =fr =no =en
set LOCAL=
REM set LOCAL=-xkbmap iso8859-2
REM 
REM ----- Paths adapted for XFree86
REM 
set BIN=.;%CYGWIN%\bin
set LIB=%CYGWIN%\usr\X11R6\bin;%CYGWIN%\usr\X11R6\lib
REM
REM 
REM -------------------------------------------------------------------------
REM ----- nothing to change below!
REM -------------------------------------------------------------------------
REM
REM ----- This is your local DISPLAY
set DISPLAY=127.0.0.1:0.0
REM 
REM ----- Cleanup after last run.
REM
if not exist %CYGWIN%\tmp\.X11-unix\X0 goto CLEANUP-FINISH
attrib -s %CYGWIN%\tmp\.X11-unix\X0
del %CYGWIN%\tmp\.X11-unix\X0
:CLEANUP-FINISH
rmdir %CYGWIN%\tmp\.X11-unix
REM 
REM ----- export PATH as we need it
REM 
set PATH=%PATH%;%BIN%;%CYGWIN%\usr\local\bin;%LIB%
REM echo %PATH%
REM 
REM Check if the OS is Windows NT/2000, otherwise do not use /B switch!
REM 
if %OS% == Windows_NT goto BSWITCH
REM 
REM ----- Win9x/ME part
REM -----
REM
REM ----- start the XWin server
start XWin %RES% %LOCAL%
REM 
REM ----- start window manager
if "%WM%"=="twm" goto TWM
if "%WM%"=="vtwm" goto VTWM
if "%WM%"=="xfwm" goto XFWM
if "%WM%"=="mwm" goto MWM
if "%WM%"=="blackbox" goto BLACKBOX
:TWM
start twm
goto DONEWM
:VTWM
start vtwm
goto DONEWM
:XFWM
start xfwm
goto DONEWM
:BLACKBOX
start xterm -sl 1000 -sb -rightbar -ms red -fg yellow -bg black -e bash --login -i -c blackbox&
goto DONEWM
:MWM
start mwm
goto DONEWM
REM ---- hopefully the window manager started. 
:DONEWM
REM 
REM ----- start xterm (not related to grass)
REM start xterm -sl 1000 -sb -rightbar -ms red -fg yellow -bg black -e bash --login -i
REM 
REM ----- start grass shell session inside cygwin/xterm
start xterm -sl 1000 -sb -rightbar -ms red -fg black -bg white -e bash --login -i -c grass5 -tcltk
REM 
REM ----- setxkbmap does not work on cygwin/XFree86
REM start setxkbmap -model pc102 -layout de -geometry microsoft 
REM start xmodmap /etc/X11/Xmodmap.127.0.0.1
start xmodmap %XLANG%
REM  
REM 
goto END
REM 
:BSWITCH
REM ----- NT/W2K part
REM -----
REM  
REM ----- start the XWin server
start /B XWin %RES% %LOCAL%
REM 
REM ----- start window manager
if "%WM%"=="twm" goto TWMB
if "%WM%"=="vtwm" goto VTWMB
if "%WM%"=="xfwm" goto XFWMB
if "%WM%"=="mwm" goto MWMB
if "%WM%"=="blackbox" goto BLACKBOXB
:TWMB
start /B twm
goto DONEWMB
:VTWMB
start /B vtwm
goto DONEWMB
:XFWMB
start /B xfwm
goto DONEWMB
:BLACKBOXB
start /B xterm -sl 1000 -sb -rightbar -ms red -fg yellow -bg black -e bash --login -i -c blackbox&
goto DONEWMB
:MWMB
start /B mwm
goto DONEWMB
REM
REM ----- hopefully the window manager started
:DONEWMB
REM  
REM ----- start xterm (not related to grass)
REM start /B xterm -sl 1000 -sb -rightbar -ms red -fg yellow -bg black -e bash --login -i
REM 
REM bash -c "/usr/bin/nohup /usr/bin/bash -c ""/usr/bin/bash -c 'twm.exe' &""&" &
REM 
REM ----- start grass shell session inside cygwin/xterm
start /B xterm -sl 1000 -sb -rightbar -ms red -fg black -bg white -e bash --login -i -c grass5 -tcltk
REM 
REM ----- setxkbmap does not work on cygwin/XFree86
REM start /B setxkbmap -model pc102 -layout de -geometry microsoft 
start /B xmodmap /etc/X11/Xmodmap.127.0.0.1
REM start /B xmodmap %XLANG%
REM 
REM ----- this is the end, my friend.
:END
REM
xsetroot -solid aquamarine4
REM ----- enable this for debugging
REM pause
REM 

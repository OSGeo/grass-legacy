@echo off
REM Startscript for GRASS5 with cygwin and StarNet XWin-32
REM Please adapt to your installation!
REM You can not avoid to pop up an windows cmd window on invocation of
REM the xterm, as the xterm.exe is an console application. Sorry.
REM Do not close this window, as this terminates your GRASS session!
REM 
REM Andreas Lange, $Date$
REM
REM
REM ----- Path to your CYGWIN ROOT
set CYGWIN=c:\cygwin
REM
REM ----- Path to your StarNet XWin-32 Server install directory
REM       Do not remove the "", as bash will bail out else. 
set STARNET="C:\PROGRAMME\StarNet\X-Win32 5.1\"
set STARLIB="C:\PROGRAMME\StarNet\X-Win32 5.1\Lib"
REM
REM -------------------------------------------------------------------------
REM ----- nothing to change below!
REM -------------------------------------------------------------------------
REM
REM ----- This is your local DISPLAY
set display=127.0.0.1:0.0
REM 
REM ----- export PATH as need it
set path=%PATH%;%CYGWIN%\usr\X11R6\bin;%CYGWIN%\usr\X11R6\lib;%CYGWIN%\usr\local\bin;%STARNET%;%STARLIB%
REM 
REM ----- start the XWin32 server
start /B XWin32 -screen 0 1024x768x16 -whitepixel 255 -blackpixel 0
REM 
REM ----- start xterm (not related to grass)
REM start /B xterm -sl 1000 -sb -rightbar -ms red -fg yellow -bg black -e bash
REM 
REM ----- start grass shell session inside cygwin/xterm
start /B xterm -sl 1000 -sb -rightbar -ms red -fg green -bg white -e "/usr/local/bin/grass5"
REM 
REM ----- do not start window manager if you choose several windows,
REM       as this uses the XWin internal window manager.
REM start /B twm
REM
REM ----- enable this for debugging
REM pause
REM 


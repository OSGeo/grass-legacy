@echo off
REM Startscript for GRASS5 with cygwin and StarNet XWin-32
REM Please adapt to your installation!
REM You can not avoid to pop up an windows cmd window on invocation of
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
REM
REM
REM ----- Path to your CYGWIN ROOT
set CYGWIN=C:\cygwin
REM
REM ----- set your resolution and color depth here:
set RES=1024x768x16
REM
REM ----- Path to your StarNet XWin-32 Server install directory
REM       Do not remove the "", as bash will bail out else. 
set STARNET="C:\Program Files\StarNet\X-Win32\"
set STARLIB="C:\Program Files\StarNet\X-Win32\Lib"
REM
REM -------------------------------------------------------------------------
REM ----- nothing to change below!
REM -------------------------------------------------------------------------
REM
REM ----- This is your local DISPLAY
set display=127.0.0.1:0.0
REM 
REM ----- export PATH as we need it
set Path=%PATH%;%CYGWIN%\bin;%CYGWIN%\usr\X11R6\bin;%CYGWIN%\usr\X11R6\lib;%CYGWIN%\usr\local\bin;%STARNET%;%STARLIB%
REM 
REM ----- start the XWin32 server
start /B XWin32 -screen 0 %RES% -whitepixel 255 -blackpixel 0
REM 
REM ----- start xterm (not related to grass)
start /B xterm -sl 1000 -sb -rightbar -ms red -fg yellow -bg black -e bash --login -i
REM 
REM ----- start grass shell session inside cygwin/xterm
REM start /B xterm -sl 1000 -sb -rightbar -ms red -fg green -bg black -e "grass5"
REM 
REM ----- do not start window manager if you choose several windows,
REM       as this uses the XWin internal window manager.
REM start /B twm
REM
REM ----- enable this for debugging
REM pause
REM 

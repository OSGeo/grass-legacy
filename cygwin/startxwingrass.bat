@echo off
REM Startscript for GRASS 5 with cygwin and StarNet XWin-32
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
REM
REM ----- Path to your CYGWIN ROOT
REM 
set CYGWIN=C:\cygwin
REM
REM ----- set your resolution here (-28 -38):
REM 
REM color depth: x8, x16, x24, x32
REM for 1200x1600
REM set RES=-screen 0 1200x1600x16
REM for 1280x1024
REM set RES=-screen 0 1280x1024x24
REM for 1152x864
REM set RES=-screen 0 1152x864x24
REM for 1024x768
set RES=-screen 0 1024x768x24
REM for 800x600
REM set RES=-screen 0 800x600x16
REM 
REM
REM ----- Path to your StarNet XWin-32 Server install directory
REM       Do not remove the "", as bash will bail out else. 
REM 
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
REM Check if the OS is Windows NT/2000, otherwise do not use /B switch!
REM 
if %OS% == Windows_NT goto BSWITCH
REM 
REM ----- Win9x/ME part
REM -----
REM
REM ----- start the XWin32 server
start XWin32 %RES% -whitepixel 255 -blackpixel 0
REM 
REM ----- start xterm (not related to grass)
REM start xterm -sl 1000 -sb -rightbar -ms red -fg yellow -bg black -e bash --login -i
REM 
REM ----- start grass shell session inside cygwin/xterm
start xterm -sl 1000 -sb -rightbar -ms red -fg black -bg white -e bash --login -i -c grass5 -tcltk
REM 
REM ----- do not start window manager if you choose several windows,
REM       as this uses the XWin internal window manager.
REM start twm
REM
goto END
REM 
:BSWITCH
REM ----- NT/W2K part
REM -----
REM  
REM ----- start the XWin32 server
start /B XWin32 %RES% -whitepixel 255 -blackpixel 0
REM 
REM ----- start xterm (not related to grass)
REM start /B xterm -sl 1000 -sb -rightbar -ms red -fg yellow -bg black -e bash --login -i
REM 
REM ----- start grass shell session inside cygwin/xterm
start /B xterm -sl 1000 -sb -rightbar -ms red -fg black -bg white -e bash --login -i -c grass5 -tcltk
REM 
REM ----- do not start window manager if you choose several windows,
REM       as this uses the XWin internal window manager.
REM start /B twm
REM
REM ----- this is the end, my friend.
:END
REM
REM ----- enable this for debugging
REM pause
REM 


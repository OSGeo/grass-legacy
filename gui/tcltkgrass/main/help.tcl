#
# Program : help.tcl
# 

help {Help} {-width 80} {-justify left} \
{
                                TclTkGRASS 4.0
               An open graphical user interface for GRASS GIS

--------------------------------------------------------------------------------

INTRODUCTION

  TclTkGRASS is an open graphical user interface for GRASS which can be
  used on top of GRASS. The main modules are accessible the menu structure,
  all other modules can be used either on command line or interactively
  through the shell window. As TclTkGRASS is platform independent, it can
  be used directly on all flavors of Unix.

 
HOW TO USE TclTkGRASS?

  The usage is quite simple: Use the left mouse button to select menu/function.

  This interface allows to access the standard GRASS modules with mouse.
  Please note: Before you use map display commands, you have to open 
  a GRASS monitor (the graphical output screen) as usual. Choose from 
  the menu
       "Display"  
           -> "Monitors"
               -> "Start"
                    -> "x0" (example)

  Like this you can open several monitors, close them or, if several 
  monitors are open, select one of these currently active monitors.

  The "Config" menu allows to set your preferences for fonts and monitor
  settings. They can be saved for future sessions.

  The menus should be self-explaining. Tutorials and descriptions you 
  find at:

  "GRASS GIS Documentation Project":
        http://grass.itc.it/gdp

  "GRASS GIS official site" from GRASS Development Team:
        http://grass.itc.it

  Beside the module windows you can also enter commands directly into 
  the shell window (as usual).


SPECIAL FEATURES

      If you click on a separator line (-----) at the top of each main menu,
      the menu will become a separated window. That is useful, if you frequently
      need the same menu.
      Generally you can leave all module windows on the screen (useful for 
      frequently used windows like "d.erase" etc.).
	 
	 Each module generates commands in the text box at the bottom. These commands
	 can be copied and pasted to the shell window (using the middle mouse button) 
	 or pasted into a command box in the Display Manager.


HOW CAN I CREATE/ADD MY OWN WINDOWS?

   Please read the file 
   
       tcltkgrass/docs/Programming_intro.txt 
   
   for details. You will see, that window programming is quite easy. 
   But you have to be careful with syntax (spaces, braces etc.).
}

#
# Program : help.tcl
# 

help {Help} {-width 80} {-justify left} \
{
                                TclTkGRASS 3.1
               An open graphical user interface for GRASS GIS

--------------------------------------------------------------------------------

INTRODUCTION

  TclTkGRASS is an open graphical user interface for GRASS which can be
  used on top of GRASS. The main modules are accessible the menu structure,
  all other modules can be used either on command line or interactively
  through the shell window. As TclTkGRASS is platform independent, it can
  be used directly on all flavors of Unix.

 
HOW TO USE TclTkGRASS?

  The usage is quite simple: Use the mouse buttons... 

               left button: select menu/function
               right button: contextual menu for a specific menu 
                             (keep it pressed)

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
        http://www.geog.uni-hannover.de/grass/gdp/

  "GRASS GIS Europe pages":
        http://www.geog.uni-hannover.de/grass/

  "GRASS GIS official site" from GRASS Development Team:
        http://www.baylor.edu/~grass/

  Beside the module windows you can also enter commands directly into 
  the shell window (as usual).


SPECIAL FEATURES

  Some specialities and new features are there:
   1) A popup menu is posted every time the mouse right button is pressed 
      inside a module window (except on the "Run" button). It can perform 
      4 actions:

         1.a resize the window to its natural size                    
         1.b reinitialize (remove user entries from input lines etc.)
         1.c raise the window (put it on top of the stacking order)   
         1.d lower the window (put it on bottom of the stacking order)
         1.e quit the window                                          

    The stacking order changed through 1.c or 1.d is memorized with 
           "Config" 
            -> "Save config"
    or 
           "Quit" (when leaving TclTkGRASS)
            -> "Save config before quitting?" 
    in main menu.
    The "quit" function (1.e) is selected on popup, so the window is closed
    if you simply press and release the right button of the mouse (except 
    if you do this near the top edge of the screen).

   2) You get the command help if you press the mouse right button on 
      the "Run" button. More help is available from the main menu through
           "?"
            -> "Manual pages"

      Using the select button, you can choose the specific module´s manpage.

   3) If you click on a separator line (-----) at the top of each main menu,
      the menu will become a separated window. That is useful, if you frequently
      need the same menu.
      Generally you can leave all module windows on the screen (useful for 
      frequently used windows like "d.erase" etc.).

   4) The window´s positions and the windows, which shall be open also in the 
      next session, can be stored when leaving TclTkGRASS:
           "Quit"
              -> "Save config before quitting?" (Yes/No/Cancel)
           
      The GRASS monitors are closed when leaving TclTkGRASS.
      When (re-)starting TclTkGRASS again, the last saved windows
      configuration is used.


HOW CAN I CREATE/ADD MY OWN WINDOWS?

   Please read the file 
       tcltkgrass/docs/Programming_intro.txt 
   for details. You will see, that window programming is quite easy. 
   But you have to be careful with syntax (spaces, braces etc.).
}

TclTkGRASS 4.0

19 March 2004
Michael Barton (Arizona State University)

This is a complete rewrite of the GRASS 5 TclTK menu hierarchy and contents. 
It is designed for GRASS 5.3 but should work fine with GRASS 5.0.3 also (except in cases where 5.3 has new commands missing from 5.0.3). It adds 200 commands and scripts to the menu system--representing nearly all the functioning, normally compiled commands and scripts for GRASS 5.3. Omitted are commands and scripts that: 1) don't seem to work, 2) duplicate the function of other items, 3) are used mainly in scripting/programming, 4) have inscrutable functions (and possibly don't work), and 5) don't seem useful (only a few).

The additions to the menu system are documented in menu_additions.txt; the commands and scripts omitted are documented in not_in_menu.txt. 

Several new scripts were added to support new menu functions. These are
located in the add2scripts directory and documented in new_scripts.txt

These updates were all installed in to the CVS source repository May 4, 2004,
which means GRASS distributions made from source code obtained after that
date (version 5.3-cvs or later) will have this update included.  If you have
an older distribution of GRASS 5.3, you can use the instructions below and
the files available at http://www.public.asu.edu/~cmbarton/grass.htm to update
your GRASS.  This is a temporary solution which will soon no longer be 
necessary.

TO INSTALL into an existing BINARY distribution of GRASS:
1. Quit GRASS

2. Backup your current $GISBASE/tcltkgrass directory (just to be safe) by renaming it $GISBASE/oldtcltkgrass.

3. Copy the /tcltkgrass directory provided here to your $GISBASE (GRASS53 or GRASS5) directory. (It is possible that you may need to adjust permissions for the tcltk scrips in the $GISBASE/tcltkgrass/script directory to make sure that users have execute permissions--hopefully not necessary).

4. Copy the scripts in the directory /add2scripts to your $GISBASE/scripts directory. (It is possible that you may need to adjust permissions for the scripts to make sure users have execute permissions--hopefully not necessary).

5. Restart GRASS and tcltkgrass.

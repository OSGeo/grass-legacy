/*
**  Written by Dave Gerdes  6/1989
**  US Army Construction Engineering Research Lab
*/

/* Modified for X 10/1992
** Terry Baker
**  US Army Construction Engineering Research Lab
*/

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include "digit.h"
#include "gis.h"
#include "wind.h"
#include "Browser.h"



/* 
** Backdrops:    
**  ask_backdrop()  ask for name of backdrop map
**  display_backdrop()  display chosen backdrop map
*/  


void
no_backdrop ()
{
    N_backdrop = "None";
    N_backdrop_mapset = NULL;
    Disp_backdrop = 0;
}


void
ask_backdrop (w)
    Widget w;
{
    char *mapset;
    char *get_browser_str();
    char back_file[200];
    static char back_mapset[200];
    static char back_name[200];
    char *s;
    
    if ((s =  get_browser_string (w)) != NULL)
        strcpy ( back_name, s); 
    mapset = G_find_file2( "cell", back_name, "");
    if (mapset == NULL)
    {
	no_backdrop();
    }
    else
    {
        strcpy (back_mapset, mapset);
        N_backdrop = back_name;
        N_backdrop_mapset = back_mapset;
       	G__file_name (back_file, "cell", back_name, mapset) ;

        if (!Terse_On)
	    Disp_backdrop = mouse_yes_no (
	    "Do you want to automatically redraw backdrop on re-window? "); 
        window_rout (U_north, U_south, U_east, U_west);
	if (Disp_backdrop)
	{
            clear_window ();
            replot (CM);
	}
    }

}

display_backdrop ()
{
/*	return drawcell (); */
/*
    XgdObject *obj;
    Pixmap pix;


    pix = XCreatePixmap (dpy, DefaultRootWindow (dpy), Wdth, Hght, 
                           DefaultDepthOfScreen (XtScreen (canvas)));
    obj = XgdCreateObject(XGD_GEOFRAME);
    XgdDrawRaster (obj, N_backdrop, N_backdrop_mapset, pix);
    XCopyArea (XtDisplay (canvas), pix, XtWindow (canvas), gc,
               0, 0, screen_right, screen_bot, 0, 0);
	       */
}


void
show_backdrop(w)
    Widget w;
{
    if (strcmp(N_backdrop, "None"))
        display_backdrop ();
    else
    {
	make_rbrowse(w);
    }
}


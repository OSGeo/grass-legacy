/*
**  Written by Dave Gerdes  6/1989
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "gis.h"
#include "wind.h"

struct Cell_head cell_window;

/* 
** Backdrops:    
**  ask_backdrop()  ask for name of backdrop map
**  display_backdrop()  display chosen backdrop map
*/  

static char back_file[200];
static char back_mapset[200];
static char back_name[200];
ask_backdrop ()
{
    char *mapset;

    /*
    init_cell_window ();
    */

    mapset = G_ask_old ("", back_name, "cell", "cell");
    if (mapset == NULL)
    {
	N_backdrop = "None";
	N_backdrop_mapset = NULL;
	disable_backdrop ();
	Disp_backdrop = 0;
	return (0);
    }
    strcpy (back_mapset, mapset);
    N_backdrop = back_name;
    N_backdrop_mapset = back_mapset;
    G__file_name (back_file, "cell", back_name, mapset) ;
    enable_backdrop ();

    if (!Terse_On)
	Disp_backdrop = G_yes (
	    "Do you want to automatically redraw backdrop on re-window? ", 1);
    return (1);
}

display_backdrop ()
{
	return drawcell ();

	/*
	flush_keyboard ();
	set_keyboard ();
	plot_backdrop (fp, &Backdrop);
	unset_keyboard ();
	return (0);
	*/

}


disable_backdrop ()
{
    disenable_backdrop (OFF);
}

enable_backdrop ()
{
    disenable_backdrop (ON);
}

disenable_backdrop (onoff)
    int onoff;
{
    register int i;

    for (i = 0 ; M_window.item[i].text != NULL ; i++)
	if (M_window.item[i].command == MWC_BACKDROP)
	{
	    M_window.item[i].enabled = onoff;
	    break;
	}
}

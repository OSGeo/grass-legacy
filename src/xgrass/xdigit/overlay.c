/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
#include <Xm/Xm.h>
#include "digit.h"
#include "Browser.h"
#include "gis.h"
#include "wind.h"

/* 
** Overlays:    
**  ask_overlay()  ask for name of overlay map
**  display_overlay()  display chosen overlay map
*/  

static char over_name[100];
static char over_mapset[100];
static int have_overlay;

void
no_overlay ()
{
    N_overlay = "None";
    Overlay.digit_file = NULL;
    have_overlay = 0;
}
void
ask_overlay (w)
    Widget w;
{
    char buf[1024];
    char *mapset;
    char *tmp;
    int display_overlay();
    char *get_browser_string();

    if ((tmp = get_browser_string(w)) != NULL)
        strcpy ( over_name, tmp);
	mapset = G_find_file2( "dig", over_name, "");

    if (mapset == NULL)
    {
        no_overlay ();
    }
    else
    {
        have_overlay = 1;
        strcpy (over_mapset, mapset);

        N_overlay = over_name;
        if (Overlay.digit_file != NULL)
	    free_name_info (&Overlay);
        G__file_name (buf, "dig", over_name, mapset) ;
        Overlay.digit_file = G_store (buf);
        Overlay.name = over_name;
        Overlay.mapset = mapset;

        if (!Terse_On)
            if(Disp_overlay = mouse_yes_no (
		"Do you want to automatically redraw overlay on re-window? "))
	    {
		display_overlay();
	    }
	    else
		remove_from_list (MWC_OVERLAY);
    }
}

/*
**  returns 0  or -1 if interupted by ESC 
*/
display_overlay ()
{
    int ret = 0;

    if (have_overlay)
    {
	Vect_set_open_level (1);
	if (1 != Vect_open_old (&Overlay, over_name, over_mapset))
	{
	    BEEP;
	    write_info (1, "Could Not open Overlay file for read\n");
	    return (1);
	}

	ret = 0;
	if (0 > plot_overlay (&Overlay))
	    ret = -1;	/* Cancel pressed */

	Vect_close (&Overlay);
    }
    else
	ret = 1;
    return ret;
}

/*
**  returns 0  or -1 if interupted by Cancel 
*/
plot_overlay (overlay)
	struct Map_info *overlay;
{
	int status ;
	int ret = 0;
	char buf[8];


	XFlush (dpy);


	Vect_set_constraint_region (overlay, U_north, U_south, U_east, U_west);
	Vect_rewind (overlay);

	write_info (1, "Plotting Overlay Map");
	TimeOutCursor (1);

	while (1)
	{
	    if (Check_for_interrupt())
		{
			ret = -1;
			goto end;
		}
   /* use Gpoints, since this should never happen in side another action */
		status = Vect_read_next_line (overlay, &Gpoints) ;
		if (status == -1)
		{
		    ret = status ;
		    goto end;
		}
		if (status == -2)
		{
		    ret = (0) ;
		    goto end;
		}

	        nplot_points(LINE, &Gpoints, CLR_OVERLAY, CLR_OVERLAY, 
			CLR_OVERLAY,0);
	 }
end:
	TimeOutCursor (0);
	 return (ret);
}


free_name_info (Map)
    struct Map_info *Map;
{
    if (Map->name)
	free (Map->name);
    if (Map->mapset)
	free (Map->mapset);
    if (Map->digit_file)
	free (Map->digit_file);
}

void
show_overlay (w)
    Widget w;
{
    if (strcmp(N_overlay, "None"))
    {
	Save_Disp_settings();
        Zero_Disp_settings();
	Disp_overlay = 1;
	replot (CM);
	Restore_Disp_settings ();
    }
    else
    {
	make_vbrowse(w);
    }
}

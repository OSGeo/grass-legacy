/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/* modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/


#include "digit.h"
#include "gis.h"
#include "wind.h"
#include "popup.h"

/* 
** Overlays:    
**  ask_overlay()  ask for name of overlay map
**  display_overlay()  display chosen overlay map
*/  

static char over_name[100];
static char over_mapset[100];
ask_overlay ()
{
    char buf[1024];
    char *mapset;

    sprintf(buf,"Terminal input required");
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    message[1] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "mapset", 1) ;
   
    G_clear_screen();
    mapset = G_ask_old ("", over_name, "dig", "vector");
    if (mapset == NULL)
    {
	N_overlay = "None";
	N_overlay_mapset = NULL;
	Overlay.digit_file = NULL;
/*	disable_overlay ();*/
	Disp_overlay = 0;
        G_clear_screen();
        erase_popup("mapset");
	return (0);
    }
    strcpy (over_mapset, mapset);

    N_overlay = over_name;
    N_overlay_mapset = mapset;
    if (Overlay.digit_file != NULL)
	free_name_info (&Overlay);
    G__file_name (buf, "dig", over_name, mapset) ;
    Overlay.digit_file = G_store (buf);
    Overlay.name = over_name;
    Overlay.mapset = mapset;
    Disp_overlay = 1;
/*  enable_overlay ();*/

    if (!Terse_On)
	Disp_overlay = G_yes (
	    "Do you want to automatically redraw overlay on re-window? ", 1);
    
    G_clear_screen();
    erase_popup("mapset");
    return (1);
}

/*
**  returns 0  or -1 if interupted by ESC 
*/
display_overlay ()
{
    int ret = 0;
    char buf[50];

    if (Disp_overlay)
    {
	Vect_set_open_level (1);
	if (1 != Vect_open_old (&Overlay, N_overlay, N_overlay_mapset))
	{
	    BEEP;
            sprintf(buf,"Could Not open Overlay file for read\n");
            message[0] = (char *) malloc (strlen (buf) + 1);
            sprintf(message[0],"%s", buf);
	    message[1] = '\0';

            Dchoose(MEN.name) ;
            popup_messg( "warning", 1) ;

	    sleep (3);
	    erase_popup("warning");
	    return (1);
	}

	ret = 0;
	flush_keyboard ();
	set_keyboard ();

	if (0 > plot_overlay (&Overlay))
	    ret = -1;	/* ESC pressed */
	unset_keyboard ();

	Vect_close (&Overlay);
    }
    else
	ret = 1;
    return ret;
}

/*
**  returns 0  or -1 if interupted by ESC
*/
plot_overlay (overlay)
	struct Map_info *overlay;
{
	int status ;
	int ret = 0;
	char buf[50];

	Vect_set_constraint_region (overlay, U_north, U_south, U_east, U_west);
	Vect_rewind (overlay);

        sprintf(buf,"Displaying Overlay Data");
        message[0] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[0],"%s", buf);
        sprintf(buf,"...Press < ESC > key to stop redraw .");
        message[1] = (char *) malloc (strlen (buf) + 1);
        sprintf(message[1],"%s", buf);
        message[2] = '\0';
	
        Dchoose(MEN.name) ;
        popup_messg( "disp_over", 1) ;

        set_keyboard ();		/* setup for kbhit () */
	while (1)
	{
	         if (key_hit (buf))
	         {
	             if (*buf == ESC)
	             {
	             /* ret = 2; */
	                goto end;
	             }
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
         unset_keyboard ();
	 R_flush ();
         erase_popup("disp_over");
	 return (ret);
}
/*
disable_overlay ()
{
    disenable_overlay (OFF);
}

enable_overlay ()
{
    disenable_overlay (ON);
}

disenable_overlay (onoff)
    int onoff;
{
    register int i;

    for (i = 0 ; M_window.item[i].text != NULL ; i++)
	if (M_window.item[i].command == MWC_OVERLAY)
	{
	    M_window.item[i].enabled = onoff;
	    break;
	}
}
*/
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


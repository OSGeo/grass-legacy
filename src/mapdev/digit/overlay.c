/* %W% %G% */
/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "gis.h"
#include "wind.h"

/* 
** Overlays:    
**  ask_overlay()  ask for name of overlay map
**  display_overlay()  display chosen overlay map
*/  

ask_overlay ()
{
    static char over_name[200];
    char buf[1024];
    char *mapset;

    mapset = G_ask_old ("", over_name, "dig", "vector");
    if (mapset == NULL)
    {
	N_overlay = "None";
	Overlay.digit_file = NULL;
	disable_overlay ();
	return (0);
    }
    N_overlay = over_name;
    if (Overlay.digit_file != NULL)
	free (Overlay.digit_file);
    G__file_name (buf, "dig", over_name, mapset) ;
    Overlay.digit_file = G_store (buf);
    enable_overlay ();

    if (!Terse_On)
	Disp_overlay = G_yes (
	    "Do you want to automatically redraw overlay on re-window? ", 0);
    return (1);
}

display_overlay ()
{
    FILE *fp;

    if (Overlay.digit_file != NULL)
    {
	if ((fp = fopen (Overlay.digit_file, "r")) == NULL)
	{
	    _Clear_info ();
	    BEEP;
	    Write_info (2, "Could Not open Overlay file for read\n");
	    sleep (3);
	    return (-1);
	}
	/*
	dig_init (fp);
	*/
	dig_read_head_binary (fp, Overlay.head);

	flush_keyboard ();
	set_keyboard ();
	plot_overlay (fp, &Overlay);
	unset_keyboard ();

	/*
	dig_fini (fp);
	*/
	fclose (fp);
    }
    else
	return (-1);
    return (0);
}

plot_overlay (fp, overlay)
	FILE *fp ;
	struct Map_info *overlay;
{
	int status ;
	int ret;
	char buf[8];

	dig_init_box( U_north, U_south, U_east, U_west ) ;

	_Clear_info ();
	Write_info (2, "Plotting Overlay Map");
	Write_info (3, "                                  ...Press <ESC> key to stop");
	while (1)
	{
		if (key_hit (buf))
		{
		    if (*buf == ESC)
		    {
			ret = 2;
			goto end;
		    }
		}
   /* use Gpoints, since this should never happen in side another action */
		status = dig_read_line_struct_in_box( fp, &Gpoints) ;
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
	 R_flush ();
	 return (ret);
}

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

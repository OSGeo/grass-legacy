/*
** $Id$
**  Written by Dave Gerdes  4/1988
**  US Army Construction Engineering Research Lab
*/

#include "gis.h"
#include "digit.h"
#include "raster.h"
#include "graphics.h"
#include "wind.h"
#include "dig_curses.h"
#include "display_line.h"
#include "display_area.h"
#include "keyboard.h"
#include "Map_proto.h"
#include "local_proto.h"

/*
**  Window menu 
**
**  driver routine for Window menu
**
**  also includes a mess of routines for displaying various things on 
**  video monitor.  This needs to be organized.
**
**  Written by Dave Gerdes 4 1988
*/
int Window (void)
{
    int command;		/* command user enters */
    int ret;			/* get return values from subrs */
    int Pass;			/* Holds value to return to caller */
    int chr;

    Pass = 0;
    Set_Global (MG_DIGIT, OFF);
    Set_Global (MG_EDIT, OFF);
    Set_Global (MG_LABEL, OFF);
    Set_Global (MG_CUSTOM, OFF);
    Set_Global (MG_TOOL, OFF);
    Set_Global (MG_WINDOW, OFF);
    Set_Global (MG_QUIT, OFF);

    Save_Disp_settings ();
    while(1) 
    {
	_Clear_info ();
	update_global_menu ();
	Write_generic_win(&M_window);

	if ((command = get_menu_command (&M_window, &chr)) > 0)
	{
	    switch(command) {
		case MWC_WIND:
		    zoom_window (0, NULL);
		    break;
		case MWC_PREV:
		    break;
		case MWC_CENT:
		    Zero_Disp_settings();
		    Disp_markers = 1;
		    replot (CMap);
		    Restore_Disp_settings ();
		    break;
		case MWC_LINES:
		    Zero_Disp_settings();
		    Disp_lines = 1;
		    replot (CMap);
		    Restore_Disp_settings ();
		    break;
		case MWC_SITES:
		    Zero_Disp_settings();
		    Disp_sites = 1;
		    replot (CMap);
		    Restore_Disp_settings ();
		    break;
		case MWC_NODES:
		    Zero_Disp_settings();
		    Disp_nodes = 1;
		    replot (CMap);
		    Restore_Disp_settings ();
		    break;
		case MWC_THRESH:
		    Zero_Disp_settings();
		    Disp_thresh = 1;
		    replot (CMap);
		    Restore_Disp_settings ();
		    break;
		case MWC_ISLES:
		    display_islands (CMap);
		    break;
		case MWC_LABELS:
		    Zero_Disp_settings();
		    Disp_labels = 1;
		    replot (CMap);
		    Restore_Disp_settings ();
		    break;
		case MWC_LLABELS:
		    Zero_Disp_settings();
		    Disp_llabels = 1;
		    replot (CMap);
		    Restore_Disp_settings ();
		    break;
		case MWC_SLABELS:
		    Zero_Disp_settings();
		    Disp_slabels = 1;
		    replot (CMap);
		    Restore_Disp_settings ();
		    break;
		case MWC_LLINES:
		    Zero_Disp_settings();
		    Disp_llines = 1;
		    replot (CMap);
		    Restore_Disp_settings ();
		    break;
		case MWC_ULINES:
		    Zero_Disp_settings();
		    Disp_ulines = 1;
		    replot (CMap);
		    Restore_Disp_settings ();
		    break;
		case MWC_CLEAR:
		    R_standard_color (dcolors[CLR_ERASE]);
		    erase_window();
		    outline_window();
		    break;
		case MWC_WHERE:
		    where_am_i (CMap);
		    break;
		case MWC_SCALE:
		    add_scale ();
		    V_flush ();
		    break;
		case MWC_BACKDROP:
		    display_backdrop ();
		    break;
		case MWC_OVERLAY:
		    Zero_Disp_settings();
		    Disp_overlay = 1;
		    replot (CMap);
		    Restore_Disp_settings ();
		    break;
		case MWC_ULAREAS:
		    display_unlabeled_areas (CMap);
		    break;
		case MWC_QUIT:
		    Pass = 0;
		    goto WIND_END;
		    break;
		default:
		    break;
	    }
	}
	else
	{
	    if ((ret = global_menu (chr, &M_window)) > 0)
	    {
		Pass = ret;	/* should never get here for window () */
		break;  /* return and execute new command */
	    }
	    if (ret < 0)
		BEEP;
	}
    }
WIND_END:
    Set_Global (MG_DIGIT, ON);
    Set_Global (MG_EDIT, ON);
    Set_Global (MG_LABEL, ON);
    Set_Global (MG_CUSTOM, ON);
    Set_Global (MG_TOOL, ON);
    Set_Global (MG_WINDOW, ON);
    Set_Global (MG_QUIT, ON);

    Restore_Disp_settings ();
    return (Pass);
}

int display_cents (struct Map_info *map)
{
    register int i;
    int ret = 0;
    char buf[100];

    set_keyboard ();
    R_standard_color (dcolors[CLR_AMARK]);
    for (i = 1 ; i <= map->n_areas ; i++)
    {
	if (key_hit (buf))
	{
	    if (*buf == ESC)
	    {
		ret = -1;
		break;
	    }
	}
	if (AREA_LABELED (&(map->Area[i])))
	{
	    _Blot (&(map->Att[map->Area[i].att].x), &(map->Att[map->Area[i].att].y)); 
	}
    }
    unset_keyboard ();
    V_flush ();
    return (ret);
}

int display_alabels (struct Map_info *map)
{
    register int i;
    register P_AREA *Area;
    int ret = 0;
    char buf[100];

    R_standard_color (dcolors[CLR_ALABEL]);
    set_keyboard ();
    for (i = 1 ; i <= map->n_areas ; i++)
    {
	if (key_hit (buf))
        {
            if (*buf == ESC)
            {
                ret = -1;
                break;
            }
        }
	Area = &(map->Area[i]);
	if (AREA_LABELED (Area))
	{
	    sprintf (buf, "%d", map->Att[Area->att].cat);
	    Adot (&(map->Att[Area->att].x), &(map->Att[Area->att].y), buf); 
	}
    }
    unset_keyboard ();
    V_flush ();
    return (ret);
}

int highlight_llabel (struct Map_info *map, int i)
{
    register P_LINE *Line;
    char buf[100];

    R_standard_color (dcolors[CLR_HIGHLIGHT]);
	Line = &(map->Line[i]);
	if (LINE_ALIVE (Line) && Line->att && map->Att[Line->att].cat)
	{
	    sprintf (buf, "%d", map->Att[Line->att].cat);
	    Adot (&(map->Att[Line->att].x), &(map->Att[Line->att].y), buf); 
	}
    V_flush ();
    return 0;
}


int display_llabel (struct Map_info *map, int i)
{
    register P_LINE *Line;
    char buf[100];

    R_standard_color (dcolors[CLR_LLABEL]);
	Line = &(map->Line[i]);
	if (LINE_ALIVE (Line) && Line->att && map->Att[Line->att].cat)
	{
	    sprintf (buf, "%d", map->Att[Line->att].cat);
	    Adot (&(map->Att[Line->att].x), &(map->Att[Line->att].y), buf); 
	}
    V_flush ();
    return 0;
}

int display_llabels (struct Map_info *map)
{
    register int i;
    register P_LINE *Line;
    char buf[100];

    R_standard_color (dcolors[CLR_LLABEL]);
    for (i = 1 ; i <= map->n_lines ; i++)
    {
	Line = &(map->Line[i]);
	if (LINE_ALIVE (Line) && Line->att && map->Att[Line->att].cat)
	{
	    sprintf (buf, "%d", map->Att[Line->att].cat);
	    Adot (&(map->Att[Line->att].x), &(map->Att[Line->att].y), buf); 
	}
    }
    V_flush ();
    return 0;
}

/* highlight all lines of category */
int display_llines (struct Map_info *map)
{
    register int i;
    int cat;
    char input[1024];

    Clear_info ();
    cat = ask_cat ();
    if (cat <= 0)
	return (-1);

    for (i = 1 ; i <= map->n_lines ; i++)
    {
	if (LINE_ALIVE (&(map->Line[i])) && map->Line[i].att && 
		map->Att[map->Line[i].att].cat == cat)
	{
	    if (0 > V1_read_line (map, &Gpoints, map->Line[i].offset))
		return (-1);
	    _highlight_line (map->Line[i].type, &Gpoints, i, map);
	}
    }
    V_flush ();
    return 0;
}

/* highlight all areas of category */
int display_lareas (struct Map_info *map)
{
    register int i;
    int cat;
    char input[1024];
    P_AREA *Area;

    Clear_info ();
    cat = ask_cat ();
    if (cat <= 0)
	return (-1);

    for (i = 1 ; i <= map->n_areas ; i++)
    {
	Area = &(map->Area[i]);
	if (AREA_LABELED (Area) && map->Att[Area->att].cat == cat)
	{
/*		This could get messy 
	    if (Auto_Window && area_outside_window (Area))
		expand_window (Area->N, Area->S, Area->E, Area->W, 1);
*/
	    _highlight_area (Area, map);
	}
    }
    V_flush ();
    return 0;
}

int display_unlabeled_areas (struct Map_info *map)
{
    register int i;
    P_AREA *Area;
    char buf[100];


    Write_info (3, "                                  ...Press <ESC> key to stop redraw");
    set_keyboard ();
    for (i = 1 ; i <= map->n_areas ; i++)
    {
        if (key_hit (buf))
        {
            if (*buf == ESC)
            {
                break;
            }
        }
 
	Area = &(map->Area[i]);
	if (AREA_ALIVE (Area) && !AREA_LABELED (Area))
	    _highlight_area (Area, map);
    }
    unset_keyboard ();
    V_flush ();
    return 0;
}

int display_islands (struct Map_info *map)
{
    register int i;
    P_LINE *Line;
    char buf[60];

    set_keyboard ();   
    Write_info (3, "                                  ...Press <ESC> key to stop redraw");
    for (i = 1 ; i <= map->n_lines ; i++)
    {
        if (key_hit (buf))
        {
            if (*buf == ESC)
            {
                break;
            }
        }
 
	Line = &(map->Line[i]);
	if (!LINE_ALIVE (Line))
	    continue;
	if (Line->right < 0 || Line->left < 0)
	{
	    if (0 > V1_read_line (map, &Gpoints, Line->offset))
		break;
	    _highlight_line (Line->type, &Gpoints, i, map);
	}
    }
    unset_keyboard ();   
    V_flush ();
    return 0;
}

/* expand current window to include new boundries */
/* window only expands, does not shrink */
int expand_window (double N, double S, double E, double W, int plus)
{
    double diff;

    R_standard_color (dcolors[CLR_ERASE]);
    erase_window();

    if (N < U_north)
	N = U_north;
    if (S > U_south)
	S = U_south;
    if (E < U_east)
	E = U_east;
    if (W > U_west)
	W = U_west;
    /*
    window_conversions (N, S, E, W);
    */
    if (plus)
    {
	diff = (N - S) * .05;
	N += diff;
	S -= diff;
	diff = (E - W) * .05;
	E += diff;
	W -= diff;
    }
    window_rout (N, S, E, W);
    outline_window();

    replot(CMap);
    return 0;
}

/* change current window to new boundries */
int move_window (double N, double S, double E, double W, int plus)
{
    double diff;

    R_standard_color (dcolors[CLR_ERASE]);
    erase_window();

    /*
    if (N < U_north)
	N = U_north;
    if (S > U_south)
	S = U_south;
    if (E < U_east)
	E = U_east;
    if (W > U_west)
	W = U_west;
    */
    /*
    window_conversions (N, S, E, W);
    */
    if (plus)
    {
	diff = (N - S) * .05;
	N += diff;
	S -= diff;
	diff = (E - W) * .05;
	E += diff;
	W -= diff;
    }
    window_rout (N, S, E, W);
    outline_window();

    replot(CMap);
    return 0;
}

/* 
** area_outside_window    return 1 if  any part of Area extends outside the
** Screen window.  0  if it is completely within 
*/
int area_outside_window (P_AREA *Area)
{
/*DEBUG fprintf (stderr, "Checking (%lf,%lf,%lf,%lf) - \n	(%lf,%lf,%lf,%lf), Area->N, Area->S, Area->E, Area->W, U_north, U_south, U_east, U_west); */

    if (Area->N > U_north)
	return (1);
    if (Area->S < U_south)
	return (1);
    if (Area->E > U_east)
	return (1);
    if (Area->W < U_west)
	return (1);
    return (0);
}

int line_outside_window (P_LINE *Line)
{
    if (Line->N > U_north)
	return (1);
    if (Line->S < U_south)
	return (1);
    if (Line->E > U_east)
	return (1);
    if (Line->W < U_west)
	return (1);
    return (0);
}

int clear_window (void)
{
    erase_window ();
    outline_window ();

    return 0;
}

int 
erase_window (void)
{
    static int dummy;
    static int screenx [4];
    static int screeny [4];

    if (dummy == 0)
    {
	screenx[0] = screen_left;
	screeny[0] = screen_top;
	screenx[1] = screen_right;
	screeny[1] = screen_top;
	screenx[2] = screen_right;
	screeny[2] = screen_bot;
	screenx[3] = screen_left;
	screeny[3] = screen_bot;
	dummy = 1;
    }

    R_standard_color (dcolors[CLR_ERASE]);
    R_polygon_abs (screenx, screeny, 4);
    V_flush ();
/*	this replaces:
    D_erase_window ();
*/

    return 0;
}

static char S_Disp_overlay;
static char S_Disp_backdrop;
static char S_Disp_lines;
static char S_Disp_points;
static char S_Disp_nodes;
static char S_Disp_labels;
static char S_Disp_outline;
static char S_Disp_markers;
static char S_Disp_llines;
static char S_Disp_llabels;
static char S_Disp_ulines;
static char S_Disp_sites;
static char S_Disp_slabels;

int 
Save_Disp_settings (void)
{
    S_Disp_overlay = Disp_overlay;
    S_Disp_backdrop = Disp_backdrop;
    S_Disp_lines = Disp_lines;
    S_Disp_points = Disp_points;
    S_Disp_nodes = Disp_nodes;
    S_Disp_labels = Disp_labels;
    S_Disp_outline = Disp_outline;
    S_Disp_markers = Disp_markers;
    S_Disp_llines = Disp_llines;
    S_Disp_llabels = Disp_llabels;
    S_Disp_ulines = Disp_ulines;
    S_Disp_sites = Disp_sites;
    S_Disp_slabels = Disp_slabels;

    return 0;
}

int 
Restore_Disp_settings (void)
{
    Disp_overlay = S_Disp_overlay;
    Disp_backdrop = S_Disp_backdrop;
    Disp_lines = S_Disp_lines;
    Disp_points = S_Disp_points;
    Disp_nodes = S_Disp_nodes;
    Disp_labels = S_Disp_labels;
    Disp_outline = S_Disp_outline;
    Disp_markers = S_Disp_markers;
    Disp_llines = S_Disp_llines;
    Disp_llabels = S_Disp_llabels;
    Disp_ulines = S_Disp_ulines;
    Disp_sites = S_Disp_sites;
    Disp_slabels = S_Disp_slabels;

    return 0;
}

int 
Zero_Disp_settings (void)
{
    Disp_overlay = 0;
    Disp_backdrop = 0;
    Disp_lines = 0;
    Disp_points = 0;
    Disp_nodes = 0;
    Disp_labels = 0;
    Disp_outline = 0;
    Disp_markers = 0;
    Disp_llines = 0;
    Disp_llabels = 0;
    Disp_ulines = 0;
    Disp_sites = 0;
    Disp_slabels = 0;

    return 0;
}


/* Old Zoom
zoom_window ()
{
    if (do_graphics())
    {
	if (Window_Device == MOUSE)
	    set_window_w_mouse ();
	else
	    set_window_w();
	clear_window ();
	replot(CMap);
    }

    return 0;
}
*/

int 
zoom_window (unsigned char type, struct line_pnts *Xpoints)
{
    int button, button1 ;
    int	screen_x, screen_y ;

    screen_x = screen_y = 1;
    if (do_graphics())
    {
	if (Window_Device == MOUSE)
	{
	    while (1)
	    {
		Clear_info ();
		_Clear_base ();
		_Write_base (12, "Buttons:") ;
		_Write_base (13, "   Left:   Select new window  ") ;
#ifdef ANOTHER_BUTTON
		_Write_base (14, "   Middle: Abort/Quit ") ;
		 Write_base (15, "   Right:  Zoom/Pan MENU") ;
#else
		_Write_base (14, "   Middle: Zoom/Pan MENU") ;
		 Write_base (15, "   Right:  Abort/Quit ") ;
#endif

		R_get_location_with_pointer ( &screen_x, &screen_y, &button) ;
		flush_keyboard (); /*ADDED*/
		Clear_info ();

		switch (button)
		{
		    case LEFTB:
			set_window_w_mouse ();
			clear_window ();
			replot(CMap); 
			if(Xpoints)
				highlight_line (type, Xpoints, 0, NULL);
			break ;
		    case MIDDLEB:
		    {
			int zoom_pan = 1;
			while (zoom_pan)
			{
			    Clear_info ();
			    _Clear_base ();
			    _Write_base (12, "Buttons:") ;
			    _Write_base (13, "   Left:   Zoom MENU") ;
#ifdef ANOTHER_BUTTON
			    _Write_base (14, "   Middle: Abort/Quit ") ;
			     Write_base (15, "   Right:  Pan  MENU") ;
#else
			    _Write_base (14, "   Middle: Pan  MENU") ;
			     Write_base (15, "   Right:  Abort/Quit") ;
#endif

			    R_get_location_with_pointer ( &screen_x, &screen_y, &button1) ;
			    flush_keyboard (); /*ADDED*/
			    Clear_info ();
			    switch(button1)
			    {
				case LEFTB:
				    scal_window (type, Xpoints);
				    break;
				case MIDDLEB:
				    slid_window (type, Xpoints);
				    break;
				case RIGHTB:
				    zoom_pan = 0;
				    break;
				default:
				    return(1);
				    break;
			    }
			}
		    }
			break;
		    case RIGHTB:
			return(0);
			break;
		    default:
			return(1) ;
			break ;
		} /* end switch */
		
	    } /* end while */
	} 
	else
	{
	    set_window_w();
	    clear_window ();
	    replot(CMap);
	}

leave:

	return 0;
    }
    return 1;
}

int slid_window (unsigned char type, struct line_pnts *Xpoints)
{
    slid_window_w_mouse (type, Xpoints);

    return 0;
}

int scal_window (unsigned char type, struct line_pnts *Xpoints)
{
    scal_window_w_mouse (type, Xpoints);

    return 0;
}

/*
**  Written by Dave Gerdes  4/1988
**  US Army Construction Engineering Research Lab
*/
/*  Last modified by R.L. Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/

#include "digit.h"
#include "graphics.h"
#include "wind.h"
#include "gis.h"
#include "popup.h"

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
int
Window ()
{
    int command;		/* command user enters */
    int ret;			/* get return values from subrs */
    int Pass;			/* Holds value to return to caller */
    int chr;
    int window=1, background_color, text_color, div_color;
    int menu_top, menu_left;
    int help_for, help_ret, help_cnt;
    static char *sav_opts[100];
    char buff[60];

    Save_Disp_settings ();
    menu_left = Next_l + 1;
    menu_top = Next_t;

    while(window) 
    {
    background_color = D_translate_color(BC_MAIN) ;
    text_color       = D_translate_color(TC_MAIN) ;
    div_color        = D_translate_color(DC_MAIN) ;

    Pass = 0;

    options[0] = "  Window Menu\0";
    options[1] = "";
    options[2] = "Show area markers";
    options[3] = "Show area labels";
    options[4] = "Show lines";
    options[5] = "Show labeled lines";
    options[6] = "Show line labels";
    options[7] = "Show sites";
    options[8] = "Show site labels";
    options[9] = "Show nodes";
    options[10] = "Define new window";
    options[11] = "Clear window";
    options[12] = "Display scale";
    options[13] = "Where am I";
    options[14] = "";
    options[15] = "Replot Screen",
    options[16] = "Help";
    options[17] = "Return to Main menu";
    options[18] = '\0' ;

	G_clear_screen ();
            ret = popup_menu(
                    background_color,
                    text_color,
                    div_color,
                    menu_top,
                    menu_left,
                    MEN_SIZE,
		    "window",
		    _wind
                    ) ;
    if (_wind) _wind = 0;     /* don't save menu panel more than once */

	    switch (ret) {
		case 2:
		    Zero_Disp_settings();
		    Disp_markers = 1;
                    Dchoose(DIG.name) ;
/*                  replot (CM); */
                    display_cents (CM);
                    Dchoose(MEN.name) ;
		    Restore_Disp_settings ();
		    break;
		case 3:
		    Zero_Disp_settings();
		    Disp_labels = 1;
                    Dchoose(DIG.name) ;
/*                  replot (CM); */
                    display_alabels (CM);
                    Dchoose(MEN.name) ;
		    Restore_Disp_settings ();
		    break;
		case 4:
		    Zero_Disp_settings();
		    Disp_lines = 1;
                    Dchoose(DIG.name) ;
		    replot (CM);
                    Dchoose(MEN.name) ;
		    Restore_Disp_settings ();
		    break;
		case 5:
		    Zero_Disp_settings();
		    Disp_llines = 1;
                    Dchoose(DIG.name) ;
		    replot (CM);
                    Dchoose(MEN.name) ;
		    Restore_Disp_settings ();
		    break;
		case 6:
		    Zero_Disp_settings();
		    Disp_llabels = 1;
                    Dchoose(DIG.name) ;
/*                  replot (CM); */
                    display_llabels (CM);
                    Dchoose(MEN.name) ;
		    Restore_Disp_settings ();
		    break;
		case 7:
		    Zero_Disp_settings();
		    Disp_sites = 1;
                    Dchoose(DIG.name) ;
		    replot (CM);
                    Dchoose(MEN.name) ;
		    Restore_Disp_settings ();
		    break;
		case 8:
		    Zero_Disp_settings();
		    Disp_slabels = 1;
                    Dchoose(DIG.name) ;
/*                  replot (CM); */
                    display_slabels (CM);
                    Dchoose(MEN.name) ;
		    Restore_Disp_settings ();
		    break;
		case 9:
		    Zero_Disp_settings();
		    Disp_nodes = 1;
                    Dchoose(DIG.name) ;
		    replot (CM);
                    Dchoose(MEN.name) ;
		    Restore_Disp_settings ();
		    break;
		case 10:
                    Dchoose(MEN.name) ;
		    zoom_window ();
		    erase_popup("zoom");
		    _zoom = 1;
		    break;
		case 11:
                    Dchoose(DIG.name) ;
		    R_standard_color (dcolors[CLR_ERASE]);
		    erase_window();
		    outline_window();
                    Dchoose(MEN.name) ;
		    break;
		case 12:
                    Dchoose(DIG.name) ;
		    add_scale ();
		    R_flush ();
                    Dchoose(MEN.name) ;
		    break;
		case 13:
                    Dchoose(DIG.name) ;
		    where_am_i (CM);
                    Dchoose(MEN.name) ;
		    break;
	        case 15:
                    Dchoose(DIG.name) ;
	            clear_window ();
	            replot (CM);
                    Dchoose(MEN.name) ;
	            break;
	        case 16:
		       for (help_cnt=0; help_cnt<=18; help_cnt++)
			  sav_opts[help_cnt] = options[help_cnt];
	               if (Help (&M_window))
		       {
                       background_color = D_translate_color(BC_HELP) ;
                       text_color       = D_translate_color(TC_HELP) ;
                       div_color        = D_translate_color(DC_HELP) ;
		       help_for = 1;
                       while(help_for)
                          {
			  for (help_cnt=0; help_cnt<=18; help_cnt++)
			    options[help_cnt] = sav_opts[help_cnt];
		          options[0] = "   HELP for",
                          options[17] = "Exit from Help menu";
                               help_ret =  popup_menu(
                                       background_color,
                                       text_color,
                                       div_color,
                                       menu_top,
                                       menu_left,
                                       MEN_SIZE,
				       "w_help",
				       1
                                       ) ;
	                    switch (help_ret) {
	                          case 2:
	                          case 3:
	                          case 4:
	                          case 5:
	                          case 6:
	                          case 7:
	                          case 8:
	                          case 9:
	                          case 10:
	                          case 11:
	                          case 12:
	                          case 13:
                                       Help_item(&M_window,help_ret);
	                               break;
	                          case 15:
                                       Help_item(&M_main,help_ret-6);
	                               break;
	                          case 16:
                                       Help_item(&M_main,help_ret-5);
	                               break;
	                          case 17:
	                               help_for = 0;
		                       options[0] = "  Window Menu\0";
                                       background_color = D_translate_color(BC_MAIN) ;
                                       text_color       = D_translate_color(TC_MAIN) ;
                                       div_color        = D_translate_color(DC_MAIN) ;
				       erase_popup("w_help");
                                       erase_popup("info_help");
	                               break;
	                          default:
	                               break;
		                    }
                          }
                       }
	               break;
		case 17:
		    Pass = window = 0;
		    break;
		default:
		    break;
	}
    }

    Restore_Disp_settings ();
    return (Pass);
}

display_cents (map)
    struct Map_info *map;
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
    R_flush ();
    return (ret);
}

display_alabels (map)
    struct Map_info *map;
{
    register int i;
    register P_AREA *Area;
    int ret = 0, ier;
    char buf[100];
    struct Categories cats;

    Dchoose(DIG.name) ;
    R_standard_color (dcolors[CLR_ALABEL]);
    if (Disp_names)
    { /* read category file , if it exists*/
        G_suppress_warnings (1);
        ier = G__read_cats ("dig_cats", N_name, G_mapset(), &cats, 1);
        G_suppress_warnings (0);
    }

    set_keyboard ();
    for (i = 1 ; i <= map->n_areas ; i++)
    {
	if (key_hit (buf))
	{
	   if (*buf == ESC)
	   {
	   /* ret = -1; */
	      break;
	   }
        }

	Area = &(map->Area[i]);
	if (AREA_LABELED (Area))
	{
          if (Disp_names && !ier)
	     {      /* check for label available */
	     if (map->Att[Area->att].cat <= cats.num &&
	        cats.list[map->Att[Area->att].cat].label != NULL)
	            sprintf (buf, "%s",
			    cats.list[map->Att[Area->att].cat].label);
             else
		{
                sprintf (buf, "Category/Label error.");
                message[0] = (char *) malloc (strlen (buf) + 1);
                sprintf(message[0],"%s", buf);
                sprintf (buf, " There is NO label for cat %d",
	                                map->Att[Area->att].cat);
                message[1] = (char *) malloc (strlen (buf) + 1);
                sprintf(message[1],"%s", buf);
                message[2] = " ";
                message[3] = '\0';

                Dchoose(MEN.name) ;
                popup_messg( "warning", 1) ;
		sleep(3);
                erase_popup("warning");
                Dchoose(DIG.name) ;
	        sprintf (buf, "%d", map->Att[Area->att].cat);
		}
	     }
          else
	   sprintf (buf, "%d", map->Att[Area->att].cat);

	  Adot (&(map->Att[Area->att].x), &(map->Att[Area->att].y), buf);
	}
    }
    unset_keyboard ();
    R_flush ();
    if (Disp_names) G_free_cats(&cats);
    return (ret);
}

highlight_llabel (map, i)
    struct Map_info *map;
{
    register P_LINE *Line;
    int ier;
    struct Categories cats;
    char buf[100];

    R_standard_color (dcolors[CLR_HIGHLIGHT]);
	Line = &(map->Line[i]);
	if (LINE_ALIVE (Line) && Line->att && map->Att[Line->att].cat)
	{
            if (Disp_names)
                { /* read category file , if it exists*/
                G_suppress_warnings (1);
                ier = G__read_cats ("dig_cats", N_name, G_mapset(), &cats, 1);
                G_suppress_warnings (0);
                }
            if (Disp_names && !ier)
	        {
	        sprintf (buf, "%s",cats.list[map->Att[Line->att].cat].label);
	        G_free_cats(&cats);
	        }
            else
	        sprintf (buf, "%d", map->Att[Line->att].cat);
	    Adot (&(map->Att[Line->att].x), &(map->Att[Line->att].y), buf); 
	}
    R_flush ();
}


display_llabel (map, i)
    struct Map_info *map;
{
    register P_LINE *Line;
    int ier;
    struct Categories cats;
    char buf[100];

    R_standard_color (dcolors[CLR_LLABEL]);
	Line = &(map->Line[i]);
	if (LINE_ALIVE (Line) && Line->att && map->Att[Line->att].cat)
	{
            if (Disp_names)
                { /* read category file , if it exists*/
                G_suppress_warnings (1);
                ier = G__read_cats ("dig_cats", N_name, G_mapset(), &cats, 1);
                G_suppress_warnings (0);
                }
            if (Disp_names && !ier)
	        {
	        sprintf (buf, "%s",cats.list[map->Att[Line->att].cat].label);
	        G_free_cats(&cats);
	        }
            else
	    sprintf (buf, "%d", map->Att[Line->att].cat);
	    Adot (&(map->Att[Line->att].x), &(map->Att[Line->att].y), buf); 
	}
    R_flush ();
}

display_llabels (map)
    struct Map_info *map;
{
    register int i;
    register P_LINE *Line;
    int ier;
    struct Categories cats;
    char buf[100];

    R_standard_color (dcolors[CLR_LLABEL]);
    if (Disp_names)
       { /* read category file , if it exists*/
       G_suppress_warnings (1);
       ier = G__read_cats ("dig_cats", N_name, G_mapset(), &cats, 1);
       G_suppress_warnings (0);
       }
    for (i = 1 ; i <= map->n_lines ; i++)
    {
	Line = &(map->Line[i]);
	if (LINE_ALIVE (Line) && Line->att && map->Att[Line->att].cat)
	{
        if (Disp_names && !ier)
	   {      /* check for label available */
	   if (map->Att[Line->att].cat <= cats.num &&
	       cats.list[map->Att[Line->att].cat].label != NULL)
	           sprintf (buf, "%s",
			    cats.list[map->Att[Line->att].cat].label);
           else
		{
                sprintf (buf, "Category/Label error.");
                message[0] = (char *) malloc (strlen (buf) + 1);
                sprintf(message[0],"%s", buf);
                sprintf (buf, " There is NO label for cat %d",
	                                map->Att[Line->att].cat);
                message[1] = (char *) malloc (strlen (buf) + 1);
                sprintf(message[1],"%s", buf);
                message[2] = " ";
                message[3] = '\0';

                Dchoose(MEN.name) ;
                popup_messg( "warning", 1) ;
		sleep(3);
                erase_popup("warning");
                Dchoose(DIG.name) ;
	        sprintf (buf, "%d", map->Att[Line->att].cat);
		}
	   }
        else
	   sprintf (buf, "%d", map->Att[Line->att].cat);

	    Adot (&(map->Att[Line->att].x), &(map->Att[Line->att].y), buf); 
	}
    }
    R_flush ();
    if (Disp_names) G_free_cats(&cats);
}

display_slabels (map)
    struct Map_info *map;
{
    register int i;
    register P_LINE *Line;
    int ier;
    struct Categories cats;
    char buf[100];

    R_standard_color (dcolors[CLR_LSITE]);
    if (Disp_names)
       { /* read category file , if it exists*/
       G_suppress_warnings (1);
       ier = G__read_cats ("dig_cats", N_name, G_mapset(), &cats, 1);
       G_suppress_warnings (0);
       }
    for (i = 1 ; i <= map->n_plines ; i++)
    {
	Line = &(map->Line[i]);
	if (LINE_ALIVE (Line) && Line->att && map->Att[Line->att].cat)
	{
        if (Disp_names && !ier)
	   {      /* check for label available */
	   if (map->Att[Line->att].cat <= cats.num &&
	       cats.list[map->Att[Line->att].cat].label != NULL)
	           sprintf (buf, "%s",
			    cats.list[map->Att[Line->att].cat].label);
           else
		{
                sprintf (buf, "Category/Label error.");
                message[0] = (char *) malloc (strlen (buf) + 1);
                sprintf(message[0],"%s", buf);
                sprintf (buf, " There is NO label for cat %d",
	                                map->Att[Line->att].cat);
                message[1] = (char *) malloc (strlen (buf) + 1);
                sprintf(message[1],"%s", buf);
                message[2] = " ";
                message[3] = '\0';

                Dchoose(MEN.name) ;
                popup_messg( "warning", 1) ;
		sleep(3);
                erase_popup("warning");
                Dchoose(DIG.name) ;
	        sprintf (buf, "%d", map->Att[Line->att].cat);
		}
	   }
        else
	   sprintf (buf, "%d", map->Att[Line->att].cat);

	    Adot (&(map->Att[Line->att].x), &(map->Att[Line->att].y), buf); 
	}
    }
    R_flush ();
    if (Disp_names) G_free_cats(&cats);
}

/* highlight all lines of category */
display_llines (map)
    struct Map_info *map;
{
    register int i;
    int cat;
    char input[1024];

    G_clear_screen ();
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
    R_flush ();
}

/* highlight all areas of category */
display_lareas (map)
    struct Map_info *map;
{
    register int i;
    int cat;
    char input[1024];
    P_AREA *Area;

    G_clear_screen ();
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
		expand_window (Area->N, Area->S, Area->E, Area->W);
*/
	    _highlight_area (Area, map);
	}
    }
    R_flush ();
}

display_unlabeled_areas (map)
    struct Map_info *map;
{
    register int i;
    P_AREA *Area;
    char buf[100];


    sprintf(buf,"...Press < ESC > key to stop redraw .");
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    message[1] = " ";
    message[2] = '\0';
	
    Dchoose(MEN.name) ;
    popup_messg( "disp_unlab", 1) ;

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
    R_flush ();
    erase_popup("disp_unlab");
}


display_islands (map)
    struct Map_info *map;
{
    register int i;
    P_LINE *Line;
    char buf[60];

    sprintf(buf,"...Press < ESC > key to stop redraw .");
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    message[1] = " ";
    message[2] = '\0';
	
    Dchoose(MEN.name) ;
    popup_messg( "disp_isle", 1) ;

    set_keyboard ();   
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
    R_flush ();
    erase_popup("disp_isle");
}

/* expand current window to include new boundries */
/* window only expands, does not shrink */
expand_window (N, S, E, W)
    double N, S, E, W;
{
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
    window_rout (N, S, E, W);
    outline_window();

    replot(CM);
}

/* 
** area_outside_window    return 1 if  any part of Area extends outside the
** Screen window.  0  if it is completely within 
*/
area_outside_window (Area)
    P_AREA *Area;
{
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

clear_window ()
{
    erase_window ();
    outline_window ();
}

erase_window ()
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
    R_flush ();
/*	this replaces:
    D_erase_window ();
*/
}

static char S_Disp_overlay;
static char S_Disp_lines;
static char S_Disp_points;
static char S_Disp_nodes;
static char S_Disp_labels;
static char S_Disp_outline;
static char S_Disp_markers;
static char S_Disp_llines;
static char S_Disp_llabels;
static char S_Disp_sites;
static char S_Disp_slabels;

Save_Disp_settings ()
{
    S_Disp_overlay = Disp_overlay;
    S_Disp_lines = Disp_lines;
    S_Disp_points = Disp_points;
    S_Disp_nodes = Disp_nodes;
    S_Disp_labels = Disp_labels;
    S_Disp_outline = Disp_outline;
    S_Disp_markers = Disp_markers;
    S_Disp_llines = Disp_llines;
    S_Disp_llabels = Disp_llabels;
    S_Disp_sites = Disp_sites;
    S_Disp_slabels = Disp_slabels;
}

Restore_Disp_settings ()
{
    Disp_overlay = S_Disp_overlay;
    Disp_lines = S_Disp_lines;
    Disp_points = S_Disp_points;
    Disp_nodes = S_Disp_nodes;
    Disp_labels = S_Disp_labels;
    Disp_outline = S_Disp_outline;
    Disp_markers = S_Disp_markers;
    Disp_llines = S_Disp_llines;
    Disp_llabels = S_Disp_llabels;
    Disp_sites = S_Disp_sites;
    Disp_slabels = S_Disp_slabels;
}

Zero_Disp_settings ()
{
    Disp_overlay = 0;
    Disp_lines = 0;
    Disp_points = 0;
    Disp_nodes = 0;
    Disp_labels = 0;
    Disp_outline = 0;
    Disp_markers = 0;
    Disp_llines = 0;
    Disp_llabels = 0;
    Disp_sites = 0;
    Disp_slabels = 0;
}

/****************************************************
#define SCALE_FACTOR 0.8
zoom_window ()
{
    int command, background_color, text_color, div_color;
    int	screen_x, screen_y ;
    int menu_left, menu_top, button;
    int zoom=1, ret, chr;
    char buff[60];
    double ux1, uy1 ;
    double ux2, uy2 ;
    double N, S, E, W;
    double tmp;

    menu_left = Next_l + 1;
    menu_top = Next_t;

    screen_x = screen_y = 1;
    while(zoom)
    {
    options[0] = "   Zoom Menu";
    options[1] = "";
    options[2] = "Select new window";
    options[3] = "Zoom In";
    options[4] = "Zoom Out";
    options[5] = "Specify new Window Center";
**  options[6] = "Select window from database";     FUTURE DEVELOPMENT **
    options[6] = "";
**    options[7] = "Restore previous window";       FUTURE DEVELOPMENT **
    options[7] = "";
    options[8] = "";
    options[9] = "Return to Main menu";
    options[10] = '\0';

    background_color = D_translate_color(BC_MAIN) ;
    text_color      = D_translate_color(TC_MAIN) ;
    div_color       = D_translate_color(DC_MAIN) ;

    ret = popup_menu(
		    background_color,
		    text_color,
		    div_color,
		    menu_top,
		    menu_left,
		    MEN_SIZE,
		    "zoom",
		    _zoom
		    );
   if (_zoom) _zoom = 0;

	   switch (ret) {
		case 2:                     ** select New window **
		       set_window_w_mouse ();
		       clear_window ();
		       replot(CM); 
                       Dchoose(MEN.name) ;
		       break ;
		case 3:                     ** ZOOM IN **
                       Dchoose(DIG.name) ;
                       W = U_west  + (U_east - U_west)   * (1. - SCALE_FACTOR);
                       E = U_east  - (U_east - U_west)   * (1. - SCALE_FACTOR);
                       S = U_south + (U_north - U_south) * (1. - SCALE_FACTOR);
                       N = U_north - (U_north - U_south) * (1. - SCALE_FACTOR);

	               window_rout (N, S, E, W);
                       clear_window ();
                       replot(CM); 
                       Dchoose(MEN.name) ;
		       break;
	        case 4:                     ** ZOOM OUT **
                       Dchoose(DIG.name) ;
                       W = U_west  - (U_east - U_west)   * (1. - SCALE_FACTOR);
                       E = U_east  + (U_east - U_west)   * (1. - SCALE_FACTOR);
                       S = U_south - (U_north - U_south) * (1. - SCALE_FACTOR);
                       N = U_north + (U_north - U_south) * (1. - SCALE_FACTOR);

	               window_rout (N, S, E, W);
                       clear_window ();
                       replot(CM);
                       Dchoose(MEN.name) ;
	               break ;
	        case 5:                    ** PAN **
	               sprintf(buff, "Select new center") ;
                       message[0] = (char *) malloc (strlen (buff) + 1);
                       sprintf(message[0],"%s", buff);
                       message[1] = '\0';

                       Dchoose(MEN.name) ;
                       popup_messg( "info", 1) ;
                       Dchoose(DIG.name) ;
	               R_get_location_with_pointer (&screen_x, &screen_y, &button);
	               flush_keyboard (); **ADDED**
	               erase_popup("info");
		       screen_to_utm ( screen_x, screen_y, &ux1, &uy1) ;
		       tmp =  (ux1 - ((U_east + U_west) / 2));
		       W = U_west + tmp;
		       E = U_east + tmp;

		       tmp =  (uy1 - ((U_north + U_south) / 2));
		       S = U_south + tmp;
		       N = U_north + tmp;

                       clear_window ();
	               window_rout (N, S, E, W);
	               replot(CM);
                       Dchoose(MEN.name) ;
		       break;
		case 6:
		case 7:
		       break;
		case 9:
		       zoom = 0;
		       break;
		default:
		       break ;
	   } 
		
    } ** end while **

    return (0);
}
*******************************************************/


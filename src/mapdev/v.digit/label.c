/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "raster.h"
#include "debug.h"
#include "digit.h"
#include "dig_curses.h"
#include "line_pnts.h"
#include "display_line.h"
#include "display_area.h"
#include "keyboard.h"
#include "Map_proto.h"
#include "local_proto.h"

static double local_x;	/* filled by label_area  */
static double local_y;	/* used by tell_area_label */
static int local_area;
static int local_prev;
static P_AREA local_struct;


int Label (void)
{
    int command;		/* command user enters */
    int ret;			/* get return values from subrs */
    int Pass;			/* Holds value to return to caller */
    int chr;
    int cat;
    int catn;

    int ans=0, ier;
    char message[72];
    struct Categories cats;

    Pass = 0;
    Set_G_Mask (MG_LABEL, OFF);

    while(1) 
    {
	_Clear_info ();
	update_global_menu ();
	Write_generic_win(&M_label);
	Label_settings ();

	if ((command = get_menu_command (&M_label, &chr)) > 0)
	{
	    switch(command)
	    {
	    case MLC_LAREA:
		Clear_base ();
                ans = ask_for_name(AREA, &cats);
		while (1)
		{
		    Clear_base ();
		    Clear_info ();
#ifdef SCS_MODS
                    if (ans) cat = ask_name(&cats);
                    else cat = ask_cat();
#else
		    cat = ask_cat();
		    if (cat && ans){
			    catn = ask_name(cat, &cats);
			    cat = (catn ? catn : cat);
		    }
#endif

		    if (!cat) break;
		    label_area (CMap, cat);

		    if(Cat_name){
			    G_free(Cat_name);
			    Cat_name = NULL;
		    }
		}
		break;
	    case MLC_LLINE:
                Clear_base ();
                ans = ask_for_name(LINE, &cats);

		while (1)
		{
		    Clear_base ();
		    Clear_info ();
#ifdef SCS_MODS
                    if (ans) cat = ask_name(&cats);
                    else cat = ask_cat();
#else
		    cat = ask_cat();
		    if (cat && ans){
			    catn = ask_name(cat, &cats);
			    cat = (catn ? catn : cat);
		    }
#endif

		    if (!cat) break;
		    label_lines (CMap, cat);

		    if(Cat_name){
			    G_free(Cat_name);
			    Cat_name = NULL;
		    }
		}
		break;
	    case MLC_LSITE:
                Clear_base ();
                ans = ask_for_name(DOT, &cats);

		while (1)
		{
		    Clear_base ();
		    Clear_info ();
#ifdef SCS_MODS
                    if (ans) cat = ask_name(&cats);
                    else cat = ask_cat();
#else
		    cat = ask_cat();
		    if (cat && ans){
			    catn = ask_name(cat, &cats);
			    cat = (catn ? catn : cat);
		    }
#endif

		    if (!cat) break;
		    label_sites (CMap, cat);

		    if(Cat_name){
			    G_free(Cat_name);
			    Cat_name = NULL;
		    }
		}
		break;
	    case MLC_LLINES:
		Clear_base ();
		Clear_info ();
		if (!curses_yes_no_default (2, "This function will label EVERY unlabeled line. Proceed?", 0))
		    break;
		cat = ask_cat ();
		if (!cat) break;
		label_all_lines (CMap, cat);
		break;
	    case MLC_ULAREA:
		unlabel_area (CMap);
		break;
	    case MLC_ULLINE:
		unlabel_lines (CMap);
		break;
	    case MLC_ULSITE:
		unlabel_sites (CMap);
		break;
	    case MLC_SLINES:
		display_llines (CMap);
		break;
	    case MLC_SAREAS:
		display_lareas (CMap);
		break;
	    case MLC_MLINES:	/* label multiple lines */
                Clear_base ();
                ans = ask_for_name(LINE, &cats);

		while (1)
		{
		    Clear_base ();
		    Clear_info ();
#ifdef SCS_MODS
                    if (ans) cat = ask_name(&cats);
                    else cat = ask_cat();
#else
		    cat = ask_cat();
		    if (cat && ans){
			    catn = ask_name(cat, &cats);
			    cat = (catn ? catn : cat);
		    }
#endif

		    if (!cat) break;
		    label_mlines (CMap, cat);

		    if(Cat_name){
			    G_free(Cat_name);
			    Cat_name = NULL;
		    }
		}
		break;
	    case MLC_UMLINES:	/* unlabel multiple lines */
		Clear_base ();
		Clear_info ();
		label_mlines (CMap, 0);
		break;
	    case MLC_CONTOUR:	/* label contour lines w/ others */
		while (1)
		    if (0 == label_contour (CMap, Contour_Interval))
			break;
		break;
	    case MLC_INTERV:  /* contour interval */
		{
		    char buf[100];
		    int val;

		    _Clear_info ();
		    Write_info (4, "   Enter New Contour Interval: ") ;
		    Get_curses_text (buf) ;
		    val = atoi (buf);
		    if (!val)
			break;
		    Contour_Interval = val;
		}
		break;
	    case MLC_QUIT:
		Pass = 0;
		goto LABEL_END;
		break;
	    default:
		break;	 /* should not get here */
	    }
	}
	else
	{
	    if ((ret = global_menu (chr, &M_label)) > 0)
	    {
		Pass = ret;
		break;  /* return and execute new command */
	    }
	    if (ret < 0)
		BEEP;
	}
    }
LABEL_END:
    
    Set_G_Mask (MG_LABEL, ON);
    return (Pass);
}

int Label_settings (void)
{
    char tmpstr[30];
    sprintf (tmpstr, "<%5d>", Contour_Interval);
    Base_string (8, 68, tmpstr);
    return 0;
}

int ask_cat (void)
{
    char buf[100];

    Write_info ( 4, "   Enter Category Number (0 to quit):[0] ") ;
    Get_curses_text (buf) ;
    return (atoi (buf));
}


/* ask user to select area to label and create  new area and label */
/* returns 0 OK  or -1 no area created */
int label_area (struct Map_info *map, int cat)
{
    int line, area, att;
    double x, y;
    int ret;

    while (1)
    {
	Clear_info ();
	/* find_line_with_mouse  fills Gpoints */
	new_point_with_mouse (&x, &y, "Select point within area:");
	if (x == 0.0 && y == 0.0)
	    return (-1);

	/* change color so they know something happend */
	R_standard_color (dcolors[CLR_AMARK]); 
	Blot (&x, &y);
	local_x = x; local_y = y;	/* store these for tell_area_label() */

	local_prev = 0;	/* reset static flag */
	/* find_line loads global struct: Garea */
	if (0>=(line = find_line_with_mouse (AREA, "Select a Boundary line:", tell_area_label)))
	{
	    unset_dot (x, y);
	    continue;
	}
	if (make_area_label (map, line) >= 0)	/* completed an area? */
	{

	  if (mouse_yes_no ("Accept this area? "))
	  {
	    /* if this far, then an area is selected, either old or new */
	    /*  if local_area, then is old, else  Garea holds area info */
	    if (local_area)
	    {
		P_ATT *AP;
		char buf[100];

		if (!map->Area[local_area].att)
		{
/*DEBUG*/ debugf ("Label area: creating new attribute\n");
		    map->Area[local_area].att = 
			dig_new_att (map, local_x, local_y, AREA, local_area, cat);
		}
		else
		{
/*DEBUG*/ debugf ("Label area: attribute exists changing it\n");
		    AP = &(map->Att[map->Area[local_area].att]);
		    sprintf (buf, "%d", AP->cat);
		    R_standard_color (dcolors[CLR_ERASE]);
		    _Blot (&(AP->x), &(AP->y));
		    Adot (&(AP->x), &(AP->y), buf);
		    AP->cat = cat;
		    AP->x = x;
		    AP->y = y;
		    dig_update_att (map, map->Area[local_area].att);
		}

		display_area (local_area, map);
		area = local_area;
	    }
	    else
	    {
/*DEBUG*/ debugf ("Label area: new area:  ");
		area = dig_new_area (map, &Garea, 0);	/* give dummy att info*/
		if (area < 0) return (-1);		/* out of memory? */
/*DEBUG*/ debugf (" creating new attribute\n");
		att = dig_new_att (map, x, y, AREA, area, cat);	/* create new att  */
		if (att < 0)
		    return (-1);
		if (att < 0) return (-1);		/* out of memory? */
		map->Area[area].att = att;		/* stick in att info */
		display_area (area, map);
	    }
	    display_area_label (area, map);
	    Changes_Made = 1;
	  }
	  else	/* cleanup and leave */
	  {
	    display_line (map->Line[line].type, &Gpoints, line, map);
	    R_standard_color (dcolors[CLR_ERASE]);
	    Blot (&local_x, &local_y);
	    if (Disp_outline && local_area && AREA_LABELED (&(map->Area[local_area])))
		display_area (local_area, map);
	    else
		if (local_area)
		    reset_area (local_area, map);
		else
		    _reset_area (&Garea, map);
	  }
	}
	else  /* area not made */
	{
	    R_standard_color (dcolors[CLR_ERASE]);
	    Blot (&local_x, &local_y);
	}
    }
    return 0;
}

int label_lines (struct Map_info *map, int cat)
{
    int line;

    while (1)
    {
	Clear_info ();
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (LINE | AREA, "Choose line:", tell_line_label)))
	{
	    return (-1);
	}

	if (0 > label_line (map, line, cat, &Gpoints))
	    return (-1);
    }
}

int label_sites (struct Map_info *map, int cat)
{
    int line;

    while (1)
    {
	Clear_info ();
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (DOT, "Choose site:", tell_line_label)))
	{
	    return (-1);
	}

	if (0 > label_line (map, line, cat, &Gpoints))
	    return (-1);
    }
}
    
int label_line (struct Map_info *map, int line, int cat, struct line_pnts *Points)
{
    int att;
    double x, y;
    int line_type;
    P_ATT *Att;


    line = abs (line); /* dpg 11/89 */

    line_type = map->Line[line].type;

    /* area and line lines all get labelled as LINE */
    if (line_type == AREA)
	line_type = LINE;

    /* remove old label from screen */
    erase_line (map->Line[line].type, Points, line, map);

    get_line_center (&x, &y, Points);

    if (map->Line[line].att) /* if already exists, change it */
    {
	att = map->Line[line].att;
	Att = &(map->Att[att]);
	Att->cat = cat;
	Att->x = x;
	Att->y = y;
	Changes_Made = 1;
	dig_update_att (map, att);
    }
    else
    {
	att = dig_new_att (map, x, y, line_type, line, cat);
	if (att < 0)
	    return (-1);
	map->Line[line].att = att;
	Changes_Made = 1;
    }
    display_line (map->Line[line].type, Points, line, map);
    return (0);
}

int unlabel_area (struct Map_info *map)
{
    int line, area, att;
    double x, y;

    while (1)
    {
	Clear_info ();
	/* find_line_with_mouse  fills Gpoints */
	new_point_with_mouse (&x, &y, "Select point within area:");
	if (x == 0.0 && y == 0.0)
	{
	    unset_dot (x, y);
	    return (-1);
	}
	R_standard_color (dcolors[CLR_AMARK]);
	Blot (&x, &y);
	local_x = x; local_y = y;	/* store these for tell_area_label() */

	if (0>=(line = find_line_with_mouse (AREA, "Select a Boundary line:", tell_area_unlabel)))
	{
	    /* is a bug here.  if accept a line that does not make an area */
	    /* this line will stay highlit */
	    /* if they aborted w/out choosing a line, there will be no line */
	    /* lit.  the previous is the one that needs to be taken care of */
	    /* should probly be fixed in find_w_mouse */
	    unset_dot (x, y);
	    continue;
	}
	unset_dot (x, y);

	if (local_area && AREA_LABELED (&(map->Area[local_area])))
	{
	    P_AREA *Area;
	    P_ATT *Att;
	    char buf[50];

	    Area = &(map->Area[local_area]);
	    Changes_Made = 1;

	    Att = &(map->Att[Area->att]);	

	    /* remove cat on screen */
	    R_standard_color (dcolors[CLR_ERASE]);
	    _Blot (&(Att->x), &(Att->y));
	    sprintf (buf, "%d", Att->cat);
	    Adot (&(Att->x), &(Att->y), buf);
	    _reset_area (Area, map);

	    dig_del_att (map, Area->att);	/* delete its attribute */
	    Area->att = 0;
	    /*  this is too drastic, lets leave the area alone
	    Del_area (map, local_area);
	    */
	}
	else
	{
/*DEBUG*/ debugf ("No attribute.  no action taken\n");
	}
    }
    return 0;
}

int unlabel_lines (struct Map_info *map)
{
    int line;

    while (1)
    {
	Clear_info ();
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (LINE | AREA, "Choose labeled line:", tell_line_label)))
	{
	    return (-1);
	}
	if (map->Line[line].att)
	{
	    erase_line (map->Line[line].type, &Gpoints, line, map);
	    dig_del_att (map, map->Line[line].att);
	    map->Line[line].att = 0;
	    Changes_Made = 1;
	    display_line (map->Line[line].type, &Gpoints, line, map);
	}
    }
    return 0;
}

int unlabel_sites (struct Map_info *map)
{
    int line;

    while (1)
    {
	Clear_info ();
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (DOT, "Choose labeled Site:", tell_line_label)))
	    return (-1);
	if (map->Line[line].att)
	{
	    dig_del_att (map, map->Line[line].att);
	    map->Line[line].att = 0;
	    Changes_Made = 1;
	    display_line (map->Line[line].type, &Gpoints, line, map);
	}
    }
    return 0;
}

int tell_line_label (struct Map_info *map, int line)
{
    char buf[200];

    if (map->Line[line].att)
    {
	sprintf (buf, "Line is Category %d", map->Att[map->Line[line].att].cat);
	Write_info (2, buf);
    }
    else
	Write_info(2, "Line is Not labeled");
    return (0);
}

/* called by find_line_w_mouse */
/* Returns -1 == No area selected as current.	*/
/* Returns 1 == Existing area selected as current.	*/
/* Returns 0 == New area selected as current.	*/
int make_area_label (struct Map_info *map, int line)
{
    int area;
    char buf[200];

    if (local_prev) {
	if (local_area)
	    display_area (local_area, map);
	else
	    _display_area (&Garea, map);
    }
	
    if ((area = check_area (map, line, local_x, local_y)) > 0)
    {
        /* Valid Area selected */
	local_prev = 1;
	local_area = area;

	if (Auto_Window && area_outside_window (&(map->Area[local_area])))
	{
	    P_AREA *Area;

	    Area = &(map->Area[local_area]);
	    expand_window (Area->N, Area->S, Area->E, Area->W, 1);
	}

	highlight_area (area, map);
	if (map->Area[area].att)
	    sprintf (buf, "Area is Category %d", map->Att[map->Area[area].att].cat);
	else
	    sprintf (buf, "Area is not labeled");
	Write_info(2, buf);
	return (1);
    }
    else
    {
        /* No area, create one */
	local_area = 0;
	if (0 >= build_area (map, local_x, local_y, line, &Garea))
	{
            /* Oops, can't create */
	    BEEP;
	    Write_info (2, "Could not create area.");
	    sleep (2);
	    local_prev = 0;
	    display_line (AREA, &Gpoints, line, map);	/* undo highlight */
	    return (-1);	/* NO Current Area */
	}
        if (Auto_Window && area_outside_window (&Garea))
            expand_window (Garea.N, Garea.S, Garea.E, Garea.W, 1);
        _highlight_area (&Garea, map);
    }

    return (0);
}

int tell_area_unlabel (struct Map_info *map, int line)
{
    int area;
    char buf[1024];

    Clear_info ();
    local_area = 0;
    if ((area = check_area (map, line, local_x, local_y)) > 0 &&
	map->Area[area].att)
    {
	sprintf (buf, "Area is labeled as category %d", 
	    map->Att[map->Area[area].att].cat);
	Write_info (1, buf);
	local_area = area;
	return (0);
    }
    else
    {
	Write_info (1, "Area is not labeled");
	local_area = 0;
	return (-1);
    }
}


int tell_area_label (struct Map_info *map, int line)
{
    int area;
    char buf[1024];

    Clear_info ();
    local_area = 0;
    if ((area = check_area (map, line, local_x, local_y)) > 0 &&
	map->Area[area].att)
    {
	sprintf (buf, "Area is labeled as category %d", 
	    map->Att[map->Area[area].att].cat);
	Write_info (1, buf);
	local_area = area;
    }
    else
    {
	Write_info (1, "Area is not labeled");
	local_area = 0;
    }
    return (0);
}

/* given x, y  and line number,  check if x, y is within a predefined 
**  area bounded by  line
*/
int check_area (struct Map_info *map, int line, double x, double y)
{
    line = abs (line);
/*DEBUG*/ debugf ("Check_area: line %d R %d L %d (%lf, %lf)\n", line, map->Line[line].right, map->Line[line].left, x, y);
    if (map->Line[line].right > 0)   /* ISLE */
	if (dig_point_in_area (map, x, y, &(map->Area[map->Line[line].right])) > 0.)
	{
/*DEBUG*/ debugf ("Check_area:  POINT IN AREA(right)  returned TRUE\n");
	    return (map->Line[line].right);
	}
    if (map->Line[line].left > 0)   /* ISLE */
	if (dig_point_in_area (map, x, y, &(map->Area[map->Line[line].left])) > 0.)
	{
/*DEBUG*/ debugf ("Check_area:  POINT IN AREA(left) returned TRUE\n");
	    return (map->Line[line].left);
	}
/*DEBUG*/ debugf ("Check_area:  POINT IN AREA returned FALSE\n");
    return (0);
}

/* need a point to place label on line.
**  for a line w/ > 2 points just pick the middle point
**  for a line w/ only 2 points we dont want to label the node (> 1 line)
**  so we calculate a mid-point
*/
#ifdef OLD_LINE_CENTER
int get_line_center (double *x, double *y, struct line_pnts *Points)
{
    int which_coor;

    if (Points->n_points < 1)
	return (-1);

    if (Points->n_points < 2)
    {
	*x = Points->x[0];
	*y = Points->y[0];
	return (0);
    }
    if (Points->n_points > 2)
    {
	which_coor = Points->n_points >> 1;
	*x = Points->x[which_coor];
	*y = Points->y[which_coor];
	return (0);
    }

    /* calculate the middle of a two points line */
    *x = (Points->x[0] + Points->x[1]) / 2.0;
    *y = (Points->y[0] + Points->y[1]) / 2.0;
    return (0);
}
#else  /* new improved line_center */

/* 
** find a fast approximate point on a chain to place a label
**  uses a city block distance approximation to choose the point
**  In other words use distance x+y to approximate len of hypot
*/

/*
**  return found point in *x and *y
** return 0 on success ,-1 on error
*/

int get_line_center (double *x, double *y, struct line_pnts *Points)
{
    register int i;
    register int n_points;
    register double *ux, *uy;
    double dist;		/* running total of line length */
    double half_dist;		/* half total line length */
    double len;			/* tmp length of current line seg */
    double frac;		/* overshoot / line length */

    n_points = Points->n_points;
    ux = Points->x;
    uy = Points->y;

    if (n_points <= 0)
	return -1;
    if (n_points == 1)
    {
	*x = Points->x[0];
	*y = Points->y[0];
	return (0);
    }
	
    dist = 0.0;
    /* get total dist */
    for (i = 1 ; i < n_points ; i++)
	dist += (fabs(ux[i]-ux[i-1]) + fabs(uy[i]-uy[i-1]));
    if (dist == 0.0)
    {
	*x = Points->x[0];
	*y = Points->y[0];
	return (0);
    }

    half_dist = dist / 2.0;

    dist = 0.0;
    for (i = 1 ; i < n_points ; i++)
    {
	len = (fabs(ux[i]-ux[i-1]) + fabs(uy[i]-uy[i-1]));
	dist += len;
	if (dist >= half_dist)  /* we're there */
	{
	    frac = 1 - (dist - half_dist) / len;
	    *x = frac * (ux[i]-ux[i-1]) + ux[i-1];
	    *y = frac * (uy[i]-uy[i-1]) + uy[i-1];
	    return (0);
	}
    }

    fprintf (stderr, "Get_line_center failed.\n");
    *x = Points->x[0];
    *y = Points->y[0];
    return (-1);
}
#endif

int display_labeled_areas (struct Map_info *map)
{
    display_all_areas (map);
    return 0;
}

int display_all_areas (struct Map_info *map)
{
    register int i;
    int ret = 0 ;
    char buf[100];

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
	if (AREA_LABELED (&(map->Area[i])))
	    display_area (i, map);
    }
    unset_keyboard ();
    V_flush ();
    return (ret);
}

int display_labeled_lines (struct Map_info *map)
{
    register int i;
    for (i = 1 ; i <= map->n_lines ; i++)
	if (LINE_ALIVE (&(map->Line[i])) && map->Line[i].att && 
		line_in_window (&(map->Line[i]))) 
	{
	    V1_read_line (map, &Gpoints, map->Line[i].offset);
	    _display_line (map->Line[i].type, &Gpoints, i, map);
	}
    V_flush ();
    return 0;
}

int display_unlabeled_lines (struct Map_info *map)
{
    register int i;
    for (i = 1 ; i <= map->n_lines ; i++)
	if (LINE_ALIVE (&(map->Line[i])) && !map->Line[i].att && 
		line_in_window (&(map->Line[i]))) 
	{
	    V1_read_line (map, &Gpoints, map->Line[i].offset);
	    _display_line (map->Line[i].type, &Gpoints, i, map);
	}
    V_flush ();
    return 0;
}

/* this is (no longer) a hidden feature for whatever use */
int label_all_lines (struct Map_info *map, int cat)
{
    int line, att;
    double x, y;

    Write_info (2, "Processing ...");
    for (line = 1 ; line <= map->n_lines ; line++)
    {
	Clear_info ();

	/* only do this for LINE lines */
	/* if already labeled, leave it alone */
	if (LINE_ALIVE (&(map->Line[line])) && (map->Line[line].type & (DOT | LINE)) && !map->Line[line].att)
	{

	    if(0 > V1_read_line(map, &Gpoints, map->Line[line].offset))
		return (-1);
	    /*
	    erase_line (map->Line[line].type, &Gpoints, line, map);
	    */
	    get_line_center (&x, &y, &Gpoints);
	    att = dig_new_att (map, x, y, LINE, line, cat);
	    if (att < 0)
		return (-1);
	    map->Line[line].att = att;
	    Changes_Made = 1;
	    display_line (map->Line[line].type, &Gpoints, line, map);
	}
    }
    Write_info (2, "Processing ...   DONE");
    return 0;
}


int ask_for_name (int Type, struct Categories *pcats)
{
    int ans, ans2, ier;
    char buffr[128];
    char *title;

    ans = -1;
    while (ans < 0)
    {
	Clear_info ();
	if (Type == 1)         /* LINE */
	    ans = curses_yes_no_default(2,
	    " Do you wish to enter line names? ", 1);
	else if (Type == 2)    /* AREA */
	    ans = curses_yes_no_default(2,
	    " Do you wish to enter area names? ", 1);
	else if (Type == 4)     /* DOT */
	    ans = curses_yes_no_default(2,
	    " Do you wish to enter site names? ", 1);
	else                   /* PSU  always enter names */
	    ans = 1;

	if (ans)
	{
#ifdef SCS_MODS
	    /* Make Master Category dir, if not existing */
	    G__make_mapset_element("SUBJ") ;
	    while(1)
	    {
		ans = -1;
		while(ans == -1)
		{
		    Clear_info ();
		    Write_info (2, " Enter the SUBJECT matter ");
		    Write_info (3, " Enter 'list' for available Subject files");
		    Write_info (4, " <CR> to Abort/Quit): ");
		    Get_curses_text(buffr,20);
		    if (strlen(buffr) == 0) 
			ans = 0;
		    else
		    if (strcmp(buffr,"list") == 0) 
			list("SUBJ");
		    else
			ans = 1;
		}

		if (ans == 0) break;
		N_subj_file = G_store(buffr);

		/* read category file , if it exists*/
		G_suppress_warnings (1);
		ier = G__read_cats ("SUBJ", N_subj_file, G_mapset(), pcats);
		G_suppress_warnings (0); 
		if (ier < 0)
		{ 
		    if (Type == PSU)
		    {
			_Clear_info ();
			Write_info (2, "PSU SUBJ file must already exist.");
			sleep (3);
			return -1;
		    }
		    Clear_info ();
		    sprintf(buffr," Do you want to create SUBJ/ file <%s>? ",N_subj_file);
		    ans2 = curses_yes_no_default (2, buffr, 1);
		    if (ans2)
		    {
			G_init_cats ((CELL)0, N_subj_file, pcats);
			G_set_cat ((CELL)0, "no data", pcats); 
			return(1);
		    }
		    else break;
		}  /* for ier < 0 */
		else
		    return(1);
	    }   /* end of while */
#else
	        /* read category file , if it exists*/
                G_suppress_warnings (1);
                ier = G_read_vector_cats (N_name, G_mapset(), pcats);
		G_suppress_warnings (0); 
		if (ier < 0) /* o.k., we create a new file dig_cats*/
		{ 
			G_init_cats ((CELL)0,"", pcats); /* create a new list */
			return(1);
		}  /* for ier < 0 */
		else
		    return(1);
#endif
	}    /* for if ans */
	if (ans == 0) return(0);
    }  /* end while */
    return (0);
}

#ifdef SCS_MODS
int ask_name (struct Categories *pcats)
#else
int ask_name (int cat_number, struct Categories *pcats)
#endif
{
    int i, icode, recd, ier;
    char buffr[128], area_name[40], cat_name[40];
    char *nptr, *cptr ;

    if(Cat_name)
    {
	 G_free(Cat_name);
	 Cat_name = NULL;
    }

    while (1)
      {
      Clear_info();
      Write_info( 4, "   Enter a label (<CR> to quit): ");
#ifdef SCS_MODS
      Get_curses_text (buffr,40) ;
#else
      Get_curses_text (buffr) ;
#endif
      if (!strlen(buffr)) 
	 {
         Clear_info();
         return(0);
         }

      strcpy(area_name,buffr);
      nptr = area_name;

	/* find input string in category struct, assign category value to the
		    area_name based on category file record number*/
#ifdef SCS_MODS
      recd = pcats->count;             /* set the number of categories */
      for (i=0;i<recd;i++)                /* category search */
	{		 
	                                    /* get a category label */
        sscanf (pcats->list[i].label, "%s", cat_name);
	cptr = cat_name;                /* first part only */

	if (strcmp(nptr,cptr) == 0)     /* compare for match */
	   {                           /* match, assigned already */
	   icode = pcats->list[i].num; /* set icode to category code */
	   Cat_name = G_store(nptr);
	   return(icode);
	   }
	} 
	/* end of category search, NO category names match */
#else
      recd = cat_number;
#endif

      Clear_info ();
      sprintf(buffr," Add new category <%d>, named <%s> ? ",recd,nptr);
      if (curses_yes_no_default (2, buffr, 1)) 
	 {                                      /* user said YES */
#ifdef SCS_MODS
  	 icode = pcats->count = recd++;         /* next category value */
#else
	 icode = cat_number;
	 pcats->num = (icode > pcats->num ? icode : pcats->num);
#endif
	 G_set_cat ((CELL)icode, nptr, pcats);  /* create entry */
         G_sort_cats (pcats);
#ifdef SCS_MODS
         ier = G__write_cats ("SUBJ", N_subj_file, &pcats);
#else
         ier = G_write_vector_cats (N_name, pcats);
#endif
         if (ier < 0)
            { 
#ifdef SCS_MODS
            sprintf(buffr," Error in writting SUBJ file <%s>",
					     N_subj_file);
#else
            sprintf(buffr," Error in writting dig_cats file <%s>",
					     N_name);
#endif
            Write_info(2,buffr); sleep(2);
            }
	 Cat_name = G_store(nptr);
	 return(icode);
	 }
      }   /* end of while */
    return 0;
}

#ifdef SCS_MODS
int label_psu (struct Map_info *map, int cat)
{
    int line, area, att;
    double x, y;
    int ret;

	Clear_info ();
	if (Digtiz_Device == MOUSE)
	      {
	         /* find_line_with_mouse  fills Gpoints */
	      new_point_with_mouse (&x, &y, "Select point within area:");
	      }
	else
#ifdef CURSORKEYS
	      if (D_cursor_buttons())
#endif
	      {
	         /* find_line_with_dig  fills Gpoints */
	      new_point_with_dig (&x, &y, "Select point within area:");
	      }

	if (x == 0.0 && y == 0.0)
	    return (-1);

	/* change color so they know something happend */
	R_standard_color (dcolors[CLR_AMARK]); 
	Blot (&x, &y);
	local_x = x; local_y = y;	/* store these for tell_area_label() */

	local_prev = 0;	/* reset static flag */
	/* find_line loads global struct: Garea */
	if (Digtiz_Device == MOUSE)
	      {
	      if (0>=(line = find_line_with_mouse (AREA, "Select a Boundary line:", tell_area_label)))
	          {
	          unset_dot (x, y);
	          return (-1);
	          }
	      }
	else
#ifdef CURSORKEYS
	      if (D_cursor_buttons())
#endif
	      {
	      if (0>=(line = find_line_with_dig (AREA, "Select a Boundary line:", tell_area_label)))
	          {
	          unset_dot (x, y);
	          return (-1);
	          }
	      }

	if (make_area_label (map, line) >= 0)	/* completed an area? */
	{

	   if (Digtiz_Device == MOUSE)
	      {
	      if (2 == mouse_yes_no ("Accept this area ? "))
		    return (0);
              ret = 1;
	      }
	   else
#ifdef CURSORKEYS
	      if (D_cursor_buttons())
#endif
	      {
	      if ( ! ask_driver_yes_no("Accept this area ? ") )
			return(0) ;
              ret = 1;
	      }

          if (ret)
	  {
	    /* if this far, then an area is selected, either old or new */
	    /*  if local_area, then is old, else  Garea holds area info */
	    if (local_area)
	    {
		P_ATT *AP;
		char buf[100];

		if (!map->Area[local_area].att)
		{
/*DEBUG*/ debugf ("Label area: creating new attribute\n");
		    map->Area[local_area].att = 
			dig_new_att (map, local_x, local_y, AREA, local_area, cat);
		}
		else
		{
/*DEBUG*/ debugf ("Label area: attribute exists changing it\n");
		    AP = &(map->Att[map->Area[local_area].att]);
		    sprintf (buf, "%d", AP->cat);
		    R_standard_color (dcolors[CLR_ERASE]);
		    _Blot (&(AP->x), &(AP->y));
		    Adot (&(AP->x), &(AP->y), buf);
		    AP->cat = cat;
		    AP->x = x;
		    AP->y = y;
		    dig_update_att (map, map->Area[local_area].att);
		}

		display_area (local_area, map);
		area = local_area;
	    }
	    else
	    {
/*DEBUG*/ debugf ("Label area: new area:  ");
		area = dig_new_area (map, &Garea, 0);	/* give dummy att info*/
		if (area < 0) return (-1);		/* out of memory? */
/*DEBUG*/ debugf (" creating new attribute\n");
		att = dig_new_att (map, x, y, AREA, area, cat);	/* create new att  */
		if (att < 0)
		    return (-1);
		if (att < 0) return (-1);		/* out of memory? */
		map->Area[area].att = att;		/* stick in att info */
		display_area (area, map);
	    }
	    display_area_label (area, map);
	    Changes_Made = 1;
	  }
	  else	/* cleanup and leave */
	  {
	    display_line (map->Line[line].type, &Gpoints, line, map);
	    R_standard_color (dcolors[CLR_ERASE]);
	    Blot (&local_x, &local_y);
	    if (Disp_outline && local_area && AREA_LABELED (&(map->Area[local_area])))
		display_area (local_area, map);
	    else
		if (local_area)
		    reset_area (local_area, map);
		else
		    _reset_area (&Garea, map);
	  }
	}
	else  /* area not made */
	{
	    R_standard_color (dcolors[CLR_ERASE]);
	    Blot (&local_x, &local_y);
	}
    return 0;
}

#endif /* SCS_MODS */

/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
#include "digit.h"
#include "gis.h"
#include "dig_head.h"
#include <stdio.h>

int tell_area_label ();
int tell_area_unlabel ();
int make_area_label ();
int tell_line_label();	/* function called by find_line_with_mouse() */

static double local_x;	/* filled by label_area  */
static double local_y;	/* used by tell_area_label */
static int local_area;
static int local_prev;
static P_AREA local_struct;

extern int Cat;

int
Label(w, command)
    Widget w;
    int  command;
{
    int chr;

debugf ("in Label command = %d\n", command);
	    switch(command)
	     {
	    case MLC_LAREA:
		if (!Cat) 
		{
		    make_monolog (1,"No category specified");
		    break;
		}
		label_area (CM, Cat);
		break;
	    case MLC_LLINE:
		if (!Cat) 
		{
		    make_monolog (1,"No category specified");
		    break;
		}
		label_lines (CM, Cat);
		break;
	    case MLC_LSITE:
		if (!Cat) 
		{
		    make_monolog (1,"No category specified");
		    break;
		}
		label_sites (CM, Cat);
		break;
	    case MLC_LLINES:
		if (!mouse_yes_no 
		("This function will label EVERY unlabeled line. Proceed?"))
		    break;
		if (!Cat) 
		{
		    make_monolog (1,"No category specified");
		    break;
		}
		label_all_lines (CM, Cat);
		break;
	    case MLC_ULAREA:
		unlabel_area (CM);
		break;
	    case MLC_ULLINE:
		unlabel_lines (CM);
		break;
	    case MLC_ULSITE:
		unlabel_sites (CM);
		break;
	    case MLC_SLINES:
		if (!Cat) 
		{
		    make_monolog (1,"No category specified");
		    break;
		}
		display_llines (CM, Cat);
		display_lareas (CM, Cat);
		break;
	    case MLC_SAREAS:
		if (!Cat) 
		{
		    make_monolog (1,"No category specified");
		    break;
		}
		display_lareas (CM, Cat);
		break;
	    case MLC_MLINES:	/* label multiple lines */
		if (!Cat) 
		{
		    make_monolog (1,"No category specified");
		    break;
		}
		label_mlines (CM, Cat);
		break;
	    case MLC_UMLINES:	/* unlabel multiple lines */
		label_mlines (CM, 0);
		break;
	    case MLC_CONTOUR:	/* label contour lines w/ others */
		if (!Contour_Interval)
		{
		    make_monolog (1, "No interval specified");
		    break;
		}
		while (1)
		    if (0 == label_contour (CM, Contour_Interval))
			break;
		break;
	    case MLC_INTERV:  /* contour interval */
		{
		    char *buf;
		    char *XmTextGetString();
		    int val;

		    buf = XmTextGetString (w);
		    val = atoi (buf);
		    XtFree (buf);
		    if (!val)
			break;
		    Contour_Interval = val;
		}
		break;
	    default:
		break;	 /* should not get here */
	    }

}



/* ask user to select area to label and create  new area and label */
/* returns 0 OK  or -1 no area created */
label_area  (map, cat)
    struct Map_info *map;
    int cat;
{
    int line, area, att;
    double x, y;
    int ret;
debugf ("label_area");
    while (1)
    {
	/* find_line_with_mouse  fills Gpoints */
	new_point_with_mouse (&x, &y, "Select point within area:");
	if (x == 0.0 && y == 0.0)
	    return (-1);

	/* change color so they know something happend */
	standard_color (dcolors[CLR_AMARK]); 
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
		    standard_color (dcolors[CLR_ERASE]);
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
	    standard_color (dcolors[CLR_ERASE]);
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
	    standard_color (dcolors[CLR_ERASE]);
	    Blot (&local_x, &local_y);
	}
    }
}

label_lines  (map, cat)
    struct Map_info *map;
    int cat;
{
    int line;

    while (1)
    {
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (LINE | AREA, "Choose line:", tell_line_label)))
	{
	    return (-1);
	}

	if (0 > label_line (map, line, cat, &Gpoints))
	    return (-1);
    }
}

label_sites  (map, cat)
    struct Map_info *map;
    int cat;
{
    int line;

    while (1)
    {
	/* find_line_with_mouse  fills Gpoints */
	if (0 >= (line = find_line_with_mouse (DOT, "Choose site:", tell_line_label)))
	{
	    return (-1);
	}

	if (0 > label_line (map, line, cat, &Gpoints))
	    return (-1);
    }
}
    
label_line (map, line, cat, Points)
    struct Map_info *map;
    int line;
    int cat;
    struct line_pnts *Points;
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

unlabel_area  (map)
    struct Map_info *map;
{
    int line, area, att;
    double x, y;

    while (1)
    {
	/* find_line_with_mouse  fills Gpoints */
	new_point_with_mouse (&x, &y, "Select point within area:");
	if (x == 0.0 && y == 0.0)
	{
	    unset_dot (x, y);
	    return (-1);
	}
	standard_color (dcolors[CLR_AMARK]);
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
	    standard_color (dcolors[CLR_ERASE]);
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
}

unlabel_lines  (map)
    struct Map_info *map;
{
    int line;

    while (1)
    {
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
}

unlabel_sites (map)
    struct Map_info *map;
{
    int line;

    while (1)
    {
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
}

tell_line_label (map, line)
    struct Map_info *map;
    int line;
{
    char buf[200];

    if (map->Line[line].att)
    {
	sprintf (buf, "Line is Category %d", map->Att[map->Line[line].att].cat);
	write_info (1, buf);
    }
    else
	write_info(1, "Line is Not labeled");
    return (0);
}


/* Document this !!   return values?? */
/* called by find_line_w_mouse */
make_area_label (map, line)
    struct Map_info *map;
    int line;
{
    int area;
    char buf[200];

    if (local_prev)
	if (local_area)
	    display_area (local_area, map);
	else
	    _display_area (&Garea, map);
	
    if ((area = check_area (map, line, local_x, local_y)) > 0)
    {
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
	write_info(1, buf);
	return (1);
    }
    else
    {
	local_area = 0;
	if (0 >= build_area (map, local_x, local_y, line, &Garea))	/* create new area */
	{
	    BEEP;
	    write_info (1, "Could not create area.");
	    local_prev = 0;
	    display_line (AREA, &Gpoints, line, map);	/* undo highlight */
	    return (-1);	/* NO Current Area */
	}
	else
	{
	    if (Auto_Window && area_outside_window (&Garea))
		expand_window (Garea.N, Garea.S, Garea.E, Garea.W, 1);
	    _highlight_area (&Garea, map);
	}
    }

    return (0);
}

tell_area_unlabel (map, line)
    struct Map_info *map;
    int line;
{
    int area;
    char buf[1024];

    local_area = 0;
    if ((area = check_area (map, line, local_x, local_y)) > 0 &&
	map->Area[area].att)
    {
	sprintf (buf, "Area is labeled as category %d", 
	    map->Att[map->Area[area].att].cat);
	write_info (1, buf);
	local_area = area;
	return (0);
    }
    else
    {
	write_info (1, "Area is not labeled");
	local_area = 0;
	return (-1);
    }
}


tell_area_label (map, line)
    struct Map_info *map;
    int line;
{
    int area;
    char buf[1024];

    local_area = 0;
    if ((area = check_area (map, line, local_x, local_y)) > 0 &&
	map->Area[area].att)
    {
	sprintf (buf, "Area is labeled as category %d", 
	    map->Att[map->Area[area].att].cat);
	write_info (1, buf);
	local_area = area;
    }
    else
    {
	write_info (1, "Area is not labeled");
	local_area = 0;
    }
    return (0);
}

/* given x, y  and line number,  check if x, y is within a predefined 
**  area bounded by  line
*/
check_area (map, line, x, y)
    struct Map_info *map;
    int line;
    double x, y;
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


/* 
** find a fast approximate point on a chain to place a label
**  uses a city block distance approximation to choose the point
**  In other words use distance x+y to approximate len of hypot
*/

/*
**  return found point in *x and *y
** return 0 on success ,-1 on error
*/

double fabs ();

get_line_center (x, y, Points)
    double *x, *y;
    struct line_pnts *Points;
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

display_labeled_areas (map)
    struct Map_info *map;
{
    display_all_areas (map);
}

display_all_areas (map)
    struct Map_info *map;
{
    register int i;
    int ret = 0 ;
    char buf[100];
    XEvent event;


    XFlush (dpy);
    TimeOutCursor (1);

    for (i = 1 ; i <= map->n_areas ; i++)
    {
	if (Check_for_interrupt())
	{
		ret = -1;
		break;
	}
	if (AREA_LABELED (&(map->Area[i])))
	    display_area (i, map);
    }
    TimeOutCursor (0);

    return (ret);
}

display_labeled_lines (map)
    struct Map_info *map;
{
    register int i;
    for (i = 1 ; i <= map->n_lines ; i++)
	if (LINE_ALIVE (&(map->Line[i])) && map->Line[i].att && 
		line_in_window (&(map->Line[i]))) 
	{
	    V1_read_line (map, &Gpoints, map->Line[i].offset);
	    _display_line (map->Line[i].type, &Gpoints, i, map);
	}
}

/* this is (no longer) a hidden feature for whatever use */
label_all_lines (map, cat)
    struct Map_info *map;
    int cat;
{
    int line, att;
    double x, y;

    for (line = 1 ; line <= map->n_lines ; line++)
    {

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
}



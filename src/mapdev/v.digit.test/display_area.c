/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "raster.h"
#include "display_line.h"
#include "display_area.h"
#include "Map_proto.h"
#include "local_proto.h"
/*
**  display area routines.    
**  routines prefixed w/ '_' are passed pointer to P_AREA instead of
**  area number   
**  
**  all these routines just call _area_display () which does the work
*/
static int _isle_display (P_ISLE *,struct Map_info *,int);
static int _area_display (P_AREA *,struct Map_info *,int);

/* this stuff could be made better, by keeping track of the points from 
** the previous call.  then for each area processed only one pass thru 
** digit file would be neccessary
**
**   Our own static struct of points is used so we dont interfere with
**     other functions assuming Gpoints has valid data
*/

#define DISPLAY   -1		/* display as area */
#define ERASE     -2		/* erase all lines */
#define HIGHLIGHT -3		/* highlight area */
#define RESET     -4		/* reset to normal line colors */

int reset_area (int area, struct Map_info *map)
{
    return (_area_display (&(map->Area[area]), map, RESET));
}

int _reset_area (
    P_AREA *Area,
    struct Map_info *map)
{
    return (_area_display (Area, map, RESET));
}

int erase_area (int area, struct Map_info *map)
{
    return (_area_display (&(map->Area[area]), map, ERASE));
}

int _erase_area (
    P_AREA *Area,
    struct Map_info *map)
{
    return (_area_display (Area, map, ERASE));
}

int highlight_area (int area, struct Map_info *map)
{
    return (_area_display (&(map->Area[area]), map, HIGHLIGHT));
}

int _highlight_area (P_AREA *Area, struct Map_info *map)
{
    return (_area_display (Area, map, HIGHLIGHT));
}

int display_area (int area, struct Map_info *map)
{
    return (_area_display (&(map->Area[area]), map, DISPLAY));
}

int _display_area ( P_AREA *Area, struct Map_info *map)
{
    return (_area_display (Area, map, DISPLAY));
}


    static int area_n_points;	/* zero at startup */
    static struct line_pnts Points;

static int _area_display ( P_AREA *Area, struct Map_info *map, int mode)
{
    register int i, line;
    int save_llabel_state;
    int save_line_state;

    save_llabel_state = Disp_llabels;
    save_line_state = Disp_lines;
    Disp_llabels = 0;	/* turn off pesky line labels for area stuff */
    Disp_lines = 1;	/* turn on lines so areas get displayed properly */

    if (area_n_points == 0)
    {
	Points.alloc_points = 0;	/* executed only once */
	area_n_points = -1; 
    }

    for (i = 0 ; i < Area->n_lines ; i++)
    {
	line = abs(Area->lines[i]);

	/* this shouldnt have to  be here, but... */
	if (!LINE_ALIVE (&(map->Line[line])))	 
	    continue;

	if (!line_in_window (&(map->Line[line])))
	    continue;
	if (0 > V1_read_line (map, &Points, map->Line[line].offset))
	{
	    Disp_llabels = save_llabel_state;
	    Disp_lines = save_line_state;
	    return (-1);
	}
	switch (mode) {
	    case DISPLAY:
		_color_line (map->Line[line].type, &Points, line, map, CLR_LAREA);
		break;
	    case ERASE:
		_erase_line (map->Line[line].type, &Points, line, map);
		break;
	    case HIGHLIGHT:
		_highlight_line (map->Line[line].type, &Points, line, map);
		break;
	    case RESET:
		/* 
		if (!Disp_lines)
		    _erase_line (map->Line[line].type, &Points, line, map);
		else
		*/
		_display_line (map->Line[line].type, &Points, line, map);
		break;
	    default:
		break;
	}
    }
    Disp_llabels = save_llabel_state;
    Disp_lines = save_line_state;

    V_flush ();
    return (0);
}


int highlight_area_label (int area, struct Map_info *map)
{
    P_AREA *Area;
    char buf[100];

    Area = &(map->Area[area]);

    R_standard_color (dcolors[CLR_HIGHLIGHT]);
    _Blot (&(map->Att[Area->att].x), &(map->Att[Area->att].y));

    R_standard_color (dcolors[CLR_HIGHLIGHT]);
    sprintf (buf, "%d", map->Att[Area->att].cat);
    Adot  (&(map->Att[Area->att].x), &(map->Att[Area->att].y), buf);

    return 0;
}

int display_area_label (int area, struct Map_info *map)
{
    P_AREA *Area;
    char buf[100];

    Area = &(map->Area[area]);

    R_standard_color (dcolors[CLR_AMARK]);
    _Blot (&(map->Att[Area->att].x), &(map->Att[Area->att].y));

    R_standard_color (dcolors[CLR_ALABEL]);
    sprintf (buf, "%d", map->Att[Area->att].cat);
    Adot  (&(map->Att[Area->att].x), &(map->Att[Area->att].y), buf);

    return 0;
}

int undisplay_area_label (int area, struct Map_info *map)
{
    P_AREA *Area;
    char buf[100];

    Area = &(map->Area[area]);
    R_standard_color (dcolors[CLR_ERASE]);
    sprintf (buf, "%d", map->Att[Area->att].cat);
    _Blot (&(map->Att[Area->att].x), &(map->Att[Area->att].y));
    Adot  (&(map->Att[Area->att].x), &(map->Att[Area->att].y), buf);

    return 0;
}

int unset_dot (double x, double y)
{
    R_standard_color (dcolors[CLR_ERASE]);
    Blot (&x, &y);

    return 0;
}

/*ISLE*/

int reset_isle (int isle, struct Map_info *map)
{
    return (_isle_display (&(map->Isle[isle]), map, RESET));
}

int _reset_isle (
    P_ISLE *Isle,
    struct Map_info *map)
{
    return (_isle_display (Isle, map, RESET));
}

int erase_isle (int isle, struct Map_info *map)
{
    return (_isle_display (&(map->Isle[isle]), map, ERASE));
}

int _erase_isle (
    P_ISLE *Isle,
    struct Map_info *map)
{
    return (_isle_display (Isle, map, ERASE));
}

int highlight_isle (int isle, struct Map_info *map)
{
    return (_isle_display (&(map->Isle[isle]), map, HIGHLIGHT));
}

int _highlight_isle (
    P_ISLE *Isle,
    struct Map_info *map)
{
    return (_isle_display (Isle, map, HIGHLIGHT));
}

int display_isle (int isle, struct Map_info *map)
{
    return (_isle_display (&(map->Isle[isle]), map, DISPLAY));
}

int _display_isle (
    P_ISLE *Isle,
    struct Map_info *map)
{
    return (_isle_display (Isle, map, DISPLAY));
}


static int _isle_display (
    P_ISLE *Isle,
    struct Map_info *map,
    int mode)
{
    register int i, line;
    int save_llabel_state;
    int save_line_state;

    save_llabel_state = Disp_llabels;
    save_line_state = Disp_lines;
    Disp_llabels = 0;	/* turn off pesky line labels for isle stuff */
    Disp_lines = 1;	/* turn on lines so areas get displayed properly */

    if (area_n_points == 0)
    {
	Points.alloc_points = 0;	/* executed only once */
	area_n_points = -1; 
    }

    for (i = 0 ; i < Isle->n_lines ; i++)
    {
	line = abs(Isle->lines[i]);

	/* this shouldnt have to  be here, but... */
	if (!LINE_ALIVE (&(map->Line[line])))	 
	    continue;

	if (!line_in_window (&(map->Line[line])))
	    continue;
	if (0 > V1_read_line (map, &Points, map->Line[line].offset))
	{
	    Disp_llabels = save_llabel_state;
	    Disp_lines = save_line_state;
	    return (-1);
	}
	switch (mode) {
	    case DISPLAY:
		_color_line (map->Line[line].type, &Points, line, map, CLR_LAREA);
		break;
	    case ERASE:
		_erase_line (map->Line[line].type, &Points, line, map);
		break;
	    case HIGHLIGHT:
		_highlight_line (map->Line[line].type, &Points, line, map);
		break;
	    case RESET:
		/* 
		if (!Disp_lines)
		    _erase_line (map->Line[line].type, &Points, line, map);
		else
		*/
		_display_line (map->Line[line].type, &Points, line, map);
		break;
	    default:
		break;
	}
    }
    Disp_llabels = save_llabel_state;
    Disp_lines = save_line_state;

    V_flush ();
    return (0);
}


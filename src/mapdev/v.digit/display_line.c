/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "raster.h"
#include "line_pnts.h"
#include "display_line.h"
#include "display_site.h"
#include "Map_proto.h"
#include "local_proto.h"

int 
display_line (int type, struct line_pnts *points, int line, struct Map_info *map)
{
    _display_line (type, points, line, map);
    V_flush ();
    return 0;
}
int 
erase_line (int type, struct line_pnts *points, int line, struct Map_info *map)
{
    _erase_line(type, points, line, map);
    V_flush ();
    return 0;
}
int 
highlight_line (int type, struct line_pnts *points, int line, struct Map_info *map)
{
    /* display it first to flash it in case it was already highlit */
    display_line(type, points, line, map);

    _highlight_line(type, points, line, map);
    V_flush ();
    return 0;
}
int 
color_line (int type, struct line_pnts *points, int line, struct Map_info *map, int color)
{
    _color_line(type, points, line, map, color);
    V_flush ();
    return 0;
}

int 
_display_line (int type, struct line_pnts *points, int line, struct Map_info *map)
{
    register int N1, N2, pnts;
    int line_color;
    int N1_color, N2_color, pnts_color;

    if (type == DOT)
	return (_display_site (type, points, line, map));

    line = abs (line);

    /* determine how many lines come into each node. 
    ** as that is how we decide what color they should be
    */
    if (line <= 0 || map == NULL)
    {
	N1 = 2;
	N2 = 2;
    }
    else
    {
	N1 = map->Node[map->Line[line].N1].n_lines;
	N2 = map->Node[map->Line[line].N2].n_lines;
    }
    pnts = LESSER (N1, N2);	

    N1 = dig_node_color (N1);
    N2 = dig_node_color (N2);
    pnts = dig_node_color (pnts);

    switch (type) {
	case AREA:
	    line_color = CLR_AREA;
	    break;
	case LINE:
	    line_color = CLR_LINE;
	    break;
	case DOT:
	    line_color = CLR_SITE;
	    break;
	default:
	    line_color = CLR_UNKNOWN;
	    break;
    }

/* Now after all that work, lets start throwing away info */
    if (!Disp_lines)	/* if they dont want lines, then they dont get em */
	line_color = 0;

    /* unless they fall under another category */
    if (line && map != NULL){
	if (Disp_llines && map->Line[line].att && map->Att[map->Line[line].att].cat)
	    line_color = CLR_LLINE;
	if (type == LINE && Disp_ulines && !map->Line[line].att)
	    line_color = CLR_LINE;
    }

    if (Disp_points)
	pnts_color = pnts;
    else
	pnts_color = 0;

    if (Disp_nodes)
    {
	N1_color = N1;
	N2_color = N2;
    }
    else
    {
	N1_color = 0;
	N2_color = 0;
    }
    nplot_points(type, points, line_color, N1_color, N2_color, pnts_color);

    if (Disp_llabels)
    {
	_display_line_label (map, line, CLR_LLABEL);
    }

    /*
    if (Disp_thresh)
    {
	display_thresh (map, map->Line[line].N1);
	display_thresh (map, map->Line[line].N2);
    }
    */
    return 0;
}

int 
_erase_line (int type, struct line_pnts *points, int line, struct Map_info *map)
{
    register int N1, N2;
    int line_color, point_color;
    P_LINE *Line;

    if (type == DOT)
	return (_erase_site (type, points, line, map));

    line = abs (line);
    if (!line)
	N1 = N2 = CLR_ERASE;
    else
    {
	Line = &(map->Line[line]);
	N1 = map->Node[map->Line[line].N1].n_lines-1;
	if (N1 < 0) N1 = 0;
	if (N1 == 0)	/* force dead nodes to BLACK */
	    N1 = CLR_ERASE;
	else
	    N1 = dig_node_color (N1);

	N2 = map->Node[map->Line[line].N2].n_lines-1;
	if (N2 < 0) N2 = 0;

	if (N2 == 0)	/* force end nodes to BLACK */
	    N2 = CLR_ERASE;
	else
	    N2 = dig_node_color (N2);
	/* special case for lines that connect to themselves */
	if (Line->N1 == Line->N2 && map->Node[Line->N1].n_lines == 2)
	    N1 = N2 = CLR_ERASE;
    }

    point_color = line_color = CLR_ERASE;

/*
    if (!Disp_nodes)
	N1 = N2 = 0;
    if (!Disp_points)
	point_color = 0;
    if (!Disp_lines)
	line_color = 0;
*/

    nplot_points(type, points, line_color, N1, N2, point_color);

    if (Disp_llabels)
	_display_line_label (map, line, CLR_ERASE);
    return 0;
}


int 
_highlight_line (int type, struct line_pnts *points, int line, struct Map_info *map)
{
    register int N1, N2;
    int line_color, point_color;

    if (type == DOT)
	return (_highlight_site (type, points, line, map));

    line = abs (line);

    if (!line || map == NULL)
	N1 = N2 = CLR_HIGHLIGHT;
    else
    {
	N1 = dig_node_color (map->Node[map->Line[line].N1].n_lines);
	N2 = dig_node_color (map->Node[map->Line[line].N2].n_lines);
    }

    line_color = CLR_HIGHLIGHT;
    point_color = CLR_HIGHLIGHT;

    /* if line is lit up, leave nodes alone */
    if (!Disp_lines && !Disp_points)
	if (Disp_nodes)
	    N1 = N2 = CLR_HIGHLIGHT;
	else
	    N1 = N2 = 0;

    if (Disp_points)
	if (Disp_lines)
	    point_color = CLR_2_NODE; /* contrast points */
	else
	    point_color = CLR_HIGHLIGHT;
    else
	point_color = 0;

    if (Disp_lines)
	line_color = CLR_HIGHLIGHT;
    else
	line_color = 0;

    nplot_points(type, points, line_color, N1, N2, point_color);

    if (Disp_llabels)
	_display_line_label (map, line, CLR_HIGHLIGHT);
    return 0;
}

int 
_color_line (int type, struct line_pnts *points, int line, struct Map_info *map, int color)
{
    register int N1, N2;
    int line_color, point_color;

    if (type == DOT)
	return (_color_site (type, points, line, map, color));

    line = abs (line);
/*
    if (!line || !Disp_nodes)
	N1 = N2 = color;
    else
    {
	N1 = dig_node_color (map->Node[map->Line[line].N1].n_lines);
	N2 = dig_node_color (map->Node[map->Line[line].N2].n_lines);
    }
*/

    line_color = color;
    point_color = color;

    if (Disp_nodes)
	N1 = N2 = color;
    else
	N1 = N2 = 0;

    if (Disp_points)
	point_color = color;
    else
	point_color = 0;

    if (Disp_lines)
	line_color = color;
    else
	line_color = 0;

    nplot_points(type, points, color, N1, N2, point_color);

    if (Disp_llabels)
	_display_line_label (map, line, color);
    return 0;
}

int 
_display_line_label (struct Map_info *map, int line, int color)
{
    char buf[100];
    P_LINE *Line;

    if (!line || map == NULL)
	return (-1);
    Line = &(map->Line[line]);
    if (!Line->att)
	return (-1);
    sprintf (buf, "%d", map->Att[Line->att].cat);
    R_standard_color (dcolors[color]);
    Adot (&(map->Att[Line->att].x),&(map->Att[Line->att].y), buf);
    return 0;
}

/*
display_thresh (map, node)
    struct Map_info *map;
    int node;
{
    int dist, junk;

    utm_to_screen (map->snap_thresh, 0.0, &dist, &junk);

    draw_circle (map, dist, map->Node[node].x, map->Node[node].y);
    return 0;
}
*/

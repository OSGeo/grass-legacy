/*
**  Written by Dave Gerdes  8/1989
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "gis.h"

/* this file was shamelessly copied from display_line.c
**  all the display code in these two files needs to be rewritten
**  and condensed.
*/

display_site(type, points, line, map)
    char type;
    struct line_pnts *points;
    int line;
    struct Map_info *map;
{
    _display_site (type, points, line, map);
    R_flush ();
}
erase_site(type, points, line, map)
    char type;
    struct line_pnts *points;
    int line;
    struct Map_info *map;
{
    _erase_site(type, points, line, map);
    R_flush ();
}
highlight_site(type, points, line, map)
    char type;
    struct line_pnts *points;
    int line;
    struct Map_info *map;
{
    /* display it first to flash it in case it was already highlit */
    display_site(type, points, line, map);

    _highlight_site(type, points, line, map);
    R_flush ();
}
color_site(type, points, line, map, color)
    char type;
    struct line_pnts *points;
    int line;
    struct Map_info *map;
    int color;
{
    _color_site(type, points, line, map, color);
    R_flush ();
}

_display_site (type, points, line, map)
    char type;
    struct line_pnts *points;
    int line;
    struct Map_info *map;
{
    int line_color;
    int node_color, node;

    line = ABS (line);

    line_color = CLR_SITE;

    /* unless they fall under another category */
    if (line && map != NULL)
	if (map->Line[line].att && map->Att[map->Line[line].att].cat)
	    line_color = CLR_LSITE;

    if (Disp_sites)
	nplot_points(type, points, line_color, 0, 0, 0);

    if (Disp_slabels)
	_display_site_label (map, line, line_color);

    /* 
    ** if Disp_nodes, then if there are other lines attached
    ** to this node, then display the node.
    **  If the site is the only line attached, do not display it
    **
    ** Also, if only Disp_nodes is on (i.e. Windows Display Node command )
    ** we always want to display them
    */
    if (Disp_nodes)
    {
	if (line && map != NULL)
	{
	    node = map->Line[line].N1;
	    if (!Disp_sites || map->Node[node].n_lines > 2)
	    {
		line_color = dig_node_color (map->Node[node].n_lines);
		R_standard_color (dcolors[line_color]);
		_Blot (points->x, points->y);
	    }
	}
    }
}

_erase_site(type, points, line, map)
    char type;
    struct line_pnts *points;
    int line;
    struct Map_info *map;
{
    int node;

    nplot_points (type, points, CLR_ERASE, 0, 0, 0);

    if (Disp_nodes)
    {
	/* if erasing a    //

	  Hmm, this below was accidentaly commented out in 3.2 
        */
	if (!line)
	{
	R_standard_color (dcolors[CLR_ERASE]);
	_Blot (points->x, points->y);
	}

	/* 
	**  if by itself, back it out just in case
	**  if attached to other lines, then leave the node
	*/
	if (line && map != NULL)
	{
	    node = map->Line[line].N1;
	    if (map->Node[node].n_lines <= 2)
	    {
		R_standard_color (dcolors[CLR_ERASE]);
		_Blot (points->x, points->y);
	    }
	}
    }
    if (Disp_slabels)
	_display_site_label (map, ABS(line), CLR_ERASE);
}


_highlight_site(type, points, line, map)
    char type;
    struct line_pnts *points;
    int line;
    struct Map_info *map;
{
    nplot_points(type, points, CLR_HIGHLIGHT, 0, 0, 0);

    if (Disp_slabels)
	_display_site_label (map, ABS(line), CLR_HIGHLIGHT);
}

_color_site(type, points, line, map, color)
    char type;
    struct line_pnts *points;
    int line;
    struct Map_info *map;
    int color;
{
    if (Disp_sites)
    nplot_points(type, points, color, 0, 0, 0);

    if (Disp_slabels)
	_display_site_label (map, ABS(line), color);
}

_display_site_label (map, line, color)
    struct Map_info *map;
    int line;
    int color;
{
    int ier;
    struct Categories cats;
    char buf[100];
    P_LINE *Line;

    if (!line || map == NULL)
	return (-1);
    Line = &(map->Line[line]);
    if (Disp_names)
        { /* read category file , if it exists*/
        G_suppress_warnings (1);
        ier = G__read_cats ("dig_cats", N_name, G_mapset(), &cats, 1);
        G_suppress_warnings (0);
        }
    if (Disp_names && !ier)
	{      /* check for label available */
	if (map->Att[Line->att].cat <= cats.num &&
	    cats.list[map->Att[Line->att].cat].label != NULL)
	   {
	   sprintf (buf, "%s",
			    cats.list[map->Att[Line->att].cat].label);
	   G_free_cats(&cats);
	   }
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

    R_standard_color (dcolors[color]);
    Adot (&(map->Att[Line->att].x),&(map->Att[Line->att].y), buf);
}

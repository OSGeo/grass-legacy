/*
**  Written by: Dave Gerdes 5 1988
**  US Army Construction Engineering Research Lab
**
**  Modified by Dave Gerdes  1/1991  for  dig_head/fileno stuff
*/

/*
**
**INTERFACE LEVEL II
**==================
**
*/

#include "digit.h"
#include "dig_head.h"
#include "gis.h"


static struct line_pnts Points;
static int first_time;
static int current_line;
static int Writable = 0;	/* Open Plus file for WRITE/READONLY */
static char *RW_str = "r";
			

dig_P_init (name, mapset, map)
    char *name;
    char *mapset; 
    struct Map_info *map; 
{
    char *error;

    if (NULL != (error = dig__P_init (name, mapset, map)))
    {
	fprintf (stderr, "%s\n", error);
	exit (-1);
    }
    return (0);
}

/***************************************************************************/
/*     Undocumented Feature:  I.E.  use at your own risk! 
*/

/*
**  Simply ensure that files are opened as read/write.  everything
**  else is up to programmer.  Note that dig_att file is not openned
**  by P_init ().
**
**  This is used buy Vcadlabel and Vclean to modify an existing
**    dig_plus file.  You have to know what is going on in the background
**    to safely use this.
** 
**   The standard method of creating and modifying dig files
**    is to use only the level_one I/O calls to read and write dig files
**    and att files only.
*/
dig__P_writeable (x)
    int x;
{
    Writable = x;
    if (x)
	RW_str = "r+";
    else
	RW_str = "r";
}

/***************************************************************************/



/* 
**  Returns NULL   or a pointer to an error message
*/
char *
dig__P_init (name, mapset, map)
    char *name;
    char *mapset; 
    struct Map_info *map; 
{
    FILE *plusfp;
    int have_old, have_plus, have_attr;
    char file[100];

    dig__Init_V ();

    if (first_time == 0)
    {
	first_time = -1;
	Points.alloc_points = 0;
	Points.n_points = 0;
	if (current_line == 0)
	    current_line = 1;
    }
    /* need dig open,  need to open plus for load_plus, and att for verify */



    have_old = have_plus = have_attr = 0;
    G__file_name (file, "dig", name, mapset);
    map->digit_file = G_store (file);
    if ( (map->digit = fopen(file, RW_str)) != NULL )
	have_old = 1;
    else
	return ("Cannot open digit file");



    G__file_name (file, "dig_plus", name, mapset);
    map->plus_file = G_store (file);


    if ((plusfp = fopen (file, RW_str)) != NULL)
    {
	fclose (plusfp);
	have_plus = 1;
    }
    else
	return ("Cannot open dig_plus file");


    G__file_name (file, "dig_att", name, mapset);
    map->att_file = G_store (file);

    /*  set up level_one access to head structure */
    dig__set_head (map->digit, &(map->head));

    dig_read_head_binary(map->digit, &(map->head));



    map->Line = NULL;
    map->Area = NULL;
    map->Isle = NULL;
    map->Att = NULL;
    map->Node = NULL;

    if (0 > dig_load_plus (map, map->digit, 1))
	return ("Error reading dig_plus file");

    /* OK */
    return (NULL);
}


dig_P_rewind (map)
    struct Map_info *map;
{
    current_line = 1;
}

dig_P_fini (map)
    struct Map_info *map;
{
    register int i;

    dig__set_head (map->digit, NULL);	/* 4.0*/

    /* close files */
    fclose (map->digit);

    /* release all memory */
    if (map->Line != NULL)
    {
	free (map->Line);
    }
    if (map->Area != NULL)
    {
	for (i = 1 ; i <= map->n_areas ; i++)
	    if (map->Area[i].alloc_lines > 0)
		if (map->Area[i].lines != NULL)
		    free (map->Area[i].lines);
	free (map->Area);
    }
    if (map->Isle != NULL)
    {
	for (i = 1 ; i <= map->n_isles ; i++)
	    if (map->Isle[i].alloc_lines > 0)
		if (map->Isle[i].lines != NULL)
		    free (map->Isle[i].lines);
	free (map->Isle);
    }
    if (map->Node != NULL)
    {
	for (i = 1 ; i <= map->n_nodes ; i++)
	    if (map->Node[i].alloc_lines > 0)
	    {
		if (map->Node[i].lines != NULL)
		    free (map->Node[i].lines);
		if (map->Node[i].angles != NULL)
		    free (map->Node[i].angles);
	    }
	free (map->Node);
    }
    if (map->Att != NULL)
    {
	free (map->Att);
    }

/*  just leave points alloced, in case they are going to still be using them 
    free (Points.x);
    free (Points.y);
    Points.n_points = Points.alloc_points = 0;
*/
}

dig_P_tmp_close (map)
    struct Map_info *map;
{
    dig__set_head (map->digit, NULL);

    fclose (map->digit);
}

dig_P_tmp_open (map)
    struct Map_info *map;
{
    /* BUG, to use tmp_open along with dig__P_writable(), 
    **  you must be sure that the writable flag is set to 
    **  the appropriate value each time you call tmp_open
    */
    map->digit = fopen (map->digit_file, RW_str);

    dig__set_head (map->digit, &(map->head));
}

dig_P_num_lines (map)
    struct Map_info *map;
{
    return (map->n_lines);
}

dig_P_num_areas (map)
    struct Map_info *map;
{
    return (map->n_areas);
}

dig_P_read_line (map, line, points)
    struct Map_info *map;
    int line;
    struct line_pnts **points;
{
    long offset, ftell();
    int ret;

    offset = map->Line[line].offset;
    ret = dig__Read_line (&Points, map->digit, offset);
    if (ret > 0)
	*points = &Points;
    return (ret);
}

/* reads next unread line each time called.  use P_rewind to reset */
dig_P_read_next_line (map, points)
    struct Map_info *map;
    struct line_pnts **points;
{
    long offset, ftell();
    int ret;

    offset = map->Line[current_line++].offset;
    ret = dig__Read_line (&Points, map->digit, offset);
    if (ret > 0)
	*points = &Points;
    return (ret);
}

/* returns category of line */
/* or 0 on any error */
dig_P_line_att (map, line)
    struct Map_info *map;
    int line;
{
    P_LINE *Line;

    Line = &(map->Line[line]);

    if (line <= 0 || line > map->n_lines || !LINE_ALIVE (Line) || ! Line->att)
	return (0);
    return (map->Att[Line->att].cat);
}
dig_P_area_att (map, area)
    struct Map_info *map;
    int area;
{
    P_AREA *Area;

    if (area <= 0 || area > map->n_areas)
	return (0);

    Area = &(map->Area[area]);
    if (!AREA_ALIVE (Area) || ! Area->att)
	return (0);
    return (map->Att[Area->att].cat);
}

/* returns -1 on error */
/* note all areas may not be labeled */
/* use get_area_att  > 0 for test of labelled */

dig_P_get_area (map, num, Area)
    struct Map_info *map;
    P_AREA **Area;
    int num;
{
    if (num <= 0 || num > map->n_areas)
	return (-1);
    if (!AREA_ALIVE (&(map->Area[num])))
	return (-1);
    *Area = &(map->Area[num]);
    return (0);
}

/* get Area bounding box info in NSEW */
dig_P_get_area_bbox (map, area, N, S, E, W)
    struct Map_info *map;
    int area;
    double *N, *S, *E ,*W;
{
    P_AREA *Area;

    if (area <= 0 || area > map->n_areas)
	return (-1);
    if (!AREA_ALIVE (&(map->Area[area])))
	return (-1);
    Area = &(map->Area[area]);
    *N = Area->N;
    *E = Area->E;
    *W = Area->W;
    *S = Area->S;
    return (0);
}

/* get Line bounding box info in NSEW */
dig_P_get_line_bbox (map, line, N, S, E, W)
    struct Map_info *map;
    int line;
    double *N, *S, *E ,*W;
{
    P_LINE *Line;

    if (line <= 0 || line > map->n_lines)
	return (-1);
    if (!LINE_ALIVE (&(map->Line[line])))
	return (-1);
    Line = &(map->Line[line]);
    *N = Line->N;
    *E = Line->E;
    *W = Line->W;
    *S = Line->S;
    return (0);
}

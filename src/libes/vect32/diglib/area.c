#include <stdlib.h>
#include "Vect.h"
/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

/*
** build_area_with_line ()
** this is the real guts of build area stuff
**
**  take a given line and start off to the RIGHT/LEFT and try to complete
**  an area.   
**  Possible Scenarios:
**	path runs into a dead end (no other area lines at node) : no area
**	path runs into a previous line that is not 1st line,    : no area
**	path runs into first line.   				: AREA!
**
**  after we find an area then we call point_in_area to see if the
**   specified point is w/in the area
**
**  returns  -1:  error   0:  no area    1:  point in area
**  -2: island  !!
*/

int dig_build_area_with_line (
    struct Map_info *map,
    int first_line,
    P_AREA *Area)
{
    register int i;
    int prev_line, next_line;
    /*
    double cent_x, cent_y;
    */
    double totalarea;
			    /* holds lines in area */
    static plus_t *array;
    char *p;
    static int array_size; /* 0 on startup */
    int n_lines;

    if (array_size == 0)	/* first time */
    {
	array_size = 1000;
	array = (plus_t *) dig__falloc (array_size, sizeof (plus_t));
	if (array == NULL)
	    return (dig_out_of_memory());
    }
#ifdef DEBUG
debugf ("_BUILD_AREA: first_line %d\n", first_line);
#endif
	array[0] = first_line;
	prev_line = -first_line;
	n_lines = 1;
	while (1)			/* when in doubt, start w/ an infinite loop */
    {
	next_line = dig_angle_next_line (map, prev_line, RIGHT);
	if (next_line == 0)
	    return (-1);

	/*  this also handles the problem w/ 1 single area line */
	if (first_line == next_line)
	{
	    /* GOT ONE!  fill area struct  and return */
#ifdef DEBUG
debugf ("GOT ONE! :");
#endif

	    if (0 > dig_area_alloc_line (Area, n_lines))
		return (-1);
	    for (i = 0 ; i < n_lines ; i++)
	    {
#ifdef DEBUG
debugf ("    %d, ", array[i]);
#endif
		Area->lines[i] = array[i];
	    }
#ifdef DEBUG
debugf ("\n");
#endif
	    Area->n_lines = n_lines;
	    /* att will be filled in later */
	    Area->att = 0;
	    dig_area_bound_box (map, Area);

	    /* check if we are going have an island instead of an area */
	    dig_find_area2 (map, Area, &totalarea);
	    if (totalarea < 0)
	    {
#ifdef DEBUG
debugf ("Area space was Negative. An Island.\n");  /* ISLE */
#endif
		return (-2);
	    }

	    return (1);
	}
	    
	/* note this is a dead end  IFF  prev_line != -first_line */
	/*  so it goes after the previous test */
	if (prev_line == next_line)
	{
#ifdef DEBUG
debugf ("Dead_end:");
#endif
	    return (0);		/* dead end */
	}

	for (i = 0 ; i < n_lines ; i++)
	    /*
	    if (next_line == array[i])
	    */
	    if (abs(next_line) == abs(array[i]))
	    {
#ifdef DEBUG
debugf ("Unclosed area:");
#endif
		return (0);			/* ran into a different area */
	    }

	/* otherwise keep going */
	if (n_lines >= array_size)
	{
	    p = dig__frealloc(array,array_size+100,sizeof(plus_t),array_size);
	    if (p == NULL)
		return (dig_out_of_memory ());
	    array = (plus_t *) p;
	    array_size += 100;
	}
	array[n_lines++] = next_line;
	prev_line = -next_line;
    }

    return 0;
}

/* add new area
**  allocate space for new area and copy info from Area to main array
**  Area has previously been filled with correct area info
**
**  then for each line in area, update line (right,left) info
*/
/* need to clean up error handling stuff */
int dig_new_area (struct Map_info *map, P_AREA *Area,int att)
{
    register int i;
    register int area, line;
    P_AREA *TO;

    if (0 > dig_alloc_area (map, 1))
	return (-1);

    area = ++(map->n_areas);
    TO = &(map->Area[area]);
    dig_struct_copy (Area, TO, sizeof (P_AREA));

    TO->alive = 1;
    TO->att = att;
    TO->alloc_lines = 0;
    TO->n_lines = 0;

    /* island stuff */
    TO->alloc_isles = 0;
    TO->n_isles = 0;
    TO->isles = NULL;

    dig_area_alloc_line (TO, Area->n_lines);

    for (i = 0 ; i < Area->n_lines ; i++)
    {
	line = Area->lines[i];
	TO->lines[i] = line;		/* copy line info */

	if (line < 0)			/* reference lines to area */
	{
#ifdef DEBUG
if (map->Line[abs(line)].left)
debugf ("Line %d already had area %d to left.\n", line, map->Line[abs(line)].left);
#endif
	    map->Line[abs(line)].left = area;
	}
	else
	{
#ifdef DEBUG
if (map->Line[abs(line)].right)
debugf ("Line %d already had area %d to right.\n", line, map->Line[abs(line)].right);
#endif
	    map->Line[line].right = area;
	}
    }
    TO->n_lines = Area->n_lines;
    return (area);
}


/*
** 
**  Same as dig_del_area, except does not write out deleted attribute
**  mark area as deleted,  delete its attribute 
**  unmark references to it in Line array
*/
int dig__del_area (
    struct Map_info *map,
    int area)
{
    register int i, line;
    P_AREA *Area;

    Area = &(map->Area[area]);
    if (Area->att)
    {
	dig__del_att (map, Area->att);	/* delete its attribute */
    }
    for (i = 0 ; i < Area->n_lines ; i++)	/* delete references in lines */
    {
	line = Area->lines[i];
	if (line < 0)
	{
	    line = abs (line);
	    map->Line[line].left = 0;
	}
	else
	    map->Line[line].right = 0;
    }
    if (Area->alloc_lines)
	free (Area->lines);		/* release memory */
    Area->alloc_lines = 0;
    Area->n_lines = 0;

    /* island stuff */
    for (i = 0 ; i < Area->n_isles ; i++)	/* delete references in lines */
    {
	dig_del_isle (map, Area->isles[i]);
    }
    if (Area->alloc_isles)
	free (Area->isles);		/* release memory */
    Area->alloc_isles = 0;
    Area->n_isles = 0;

    Area->att = 0;
    Area->alive = 0;	/* and mark itself as deleted */

    return (0);
}
/*
** 
**  mark area as deleted,  delete its attribute 
**  unmark references to it in Line array
**  note this does NOT affect map->n_areas   because deleting does not 
**  affect number of spaces in the array.  it just nullifies one of them
*/
int dig_del_area (struct Map_info *map, int area)
{
    register int i, line;
    P_AREA *Area;

    Area = &(map->Area[area]);
    if (Area->att)
    {
	dig_del_att (map, Area->att);	/* delete its attribute */
    }
    for (i = 0 ; i < Area->n_lines ; i++)	/* delete references in lines */
    {
	line = Area->lines[i];
	if (line < 0)
	{
	    line = abs (line);
	    map->Line[line].left = 0;
	}
	else
	    map->Line[line].right = 0;
    }
    if (Area->alloc_lines)
	free (Area->lines);		/* release memory */
    Area->alloc_lines = 0;
    Area->n_lines = 0;

    /* island stuff */
    for (i = 0 ; i < Area->n_isles ; i++)	/* delete references in lines */
    {
	dig_del_isle (map, Area->isles[i]);
    }
    if (Area->alloc_isles)
	free (Area->isles);		/* release memory */
    Area->alloc_isles = 0;
    Area->n_isles = 0;

    Area->att = 0;
    Area->alive = 0;	/* and mark itself as deleted */

    return (0);
}

/* 
** angle_next_line ()
**   current_line will be negative if we are looking at Node 2
**    return line number of next angle to follow an area boundary
**	assume that lines are sorted in increasing angle order
**    returns 0 on error or not found
*/
int dig_angle_next_line (
    struct Map_info *map,
    int current_line,		/* current line number */
    int direction)
{
    register int next;
    register int current;
    plus_t node;
    P_NODE *Node;

    if (current_line < 0)
	node = map->Line[abs(current_line)].N2;
    else
	node = map->Line[current_line].N1;
    Node = &(map->Node[node]);

    /* first find index for that line */
    for (current = 0 ; current < Node->n_lines ; current++)
	if (Node->lines[current] == current_line)
	    goto start;
#ifdef DEBUG
debugf ("Angle_next_line:  Line NOT found at node %d\n", (int) node);
#endif
    return (0);	/* not found */

start:
    next = current;
    if (direction == RIGHT)
    {
	do {
	    if (next == Node->n_lines - 1)
		next = 0;
	    else
		next++;
	} while (map->Line[abs(Node->lines[next])].type != AREA && next != current);
    }
    else
    {
	do {
	    if (next == 0)
		next = Node->n_lines -1;
	    else
		next--;
	} while (map->Line[abs(Node->lines[next])].type != AREA && next != current);
    }
    return ((int) (Node->lines[next]));
}
    
/* 
** area_bound_box
**   create bounding box for area from line bboxes
*/

int dig_area_bound_box (
    struct Map_info *map,
    P_AREA *Area)
{
    register int i;
    P_LINE *LP;

    /* initialize N and E */
    Area->N = Area->S = map->Line[abs(Area->lines[0])].N;
    Area->E = Area->W = map->Line[abs(Area->lines[0])].E;

    for (i = 0 ; i < Area->n_lines ; i ++)
    {
	LP = &(map->Line[abs(Area->lines[i])]);
	if (LP->N > Area->N) Area->N = LP->N;
	if (LP->E > Area->E) Area->E = LP->E;

	if (LP->S < Area->S) Area->S = LP->S;
	if (LP->W < Area->W) Area->W = LP->W;
    }

    return 0;
}

/*   AREAS:

    ///
    Areas exist only if labeled.
    If label is removed, area is removed.

    no longer true.  area are built by import  but can be destroyed
    by editing
    ///

    If line is removed that borders 1 or more areas, those areas are removed.
    Snapping lines CAN cause the same effect (snapping a line that was a border)
    as well as snapping to an area, in some cases.
    Digitizing a new AREA-border that snaps to an area;
    Moving points (END POINTS) CAN cause the same (if such is supported)

    User: Choose 'label areas'
    User: specify label number

    User: Place point w/in area
    User: Select a line on area border

    Program:
	
	check if in existing area!
	CREATE up to 2 areas w/ line.
	for each area
	    Pass line from point thru area line segments
	    if (n_intersections  is odd)
		have area
	if (have area)
	    build new area info
		allocate space for new area
		copy line info to area
		for each line set area info in line struct
	    new_att()
*/

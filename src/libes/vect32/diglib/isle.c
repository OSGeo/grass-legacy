#include <stdlib.h>
#include "Vect.h"
/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/* add new isle
**  allocate space for new isle and copy info from Area to main array
**  Area has previously been filled with correct isle info
**
**  then for each line in isle, update line (right,left) info
*/
/* need to clean up error handling stuff */
int dig_new_isle (
    struct Map_info *map,
    P_AREA *Area,
    plus_t area)
{
    register int i;
    /*
    register int j;
    */
    register int isle, line;
    P_ISLE *TO;

    if (0 > dig_alloc_isle (map, 1))
	return (-1);

    isle = ++(map->n_isles);
    TO = &(map->Isle[isle]);

    TO->N = Area->N;/*ISLE*/
    TO->S = Area->S;
    TO->E = Area->E;
    TO->W = Area->W;
    TO->area = area;  /*ISLE*/

    TO->alive = 1;
    TO->alloc_lines = 0;
    TO->n_lines = 0;

    dig_isle_alloc_line (TO, Area->n_lines);

    /*
    for (i = 0, j = Area->n_lines-1 ; i < Area->n_lines ; i++, j--)
    */
    for (i = 0 ; i < Area->n_lines ; i++)
    {
	/*
	line = Area->lines[j];
	TO->lines[i] = line;		
	*/
	/* Chris Emmerich of Autometric found this bug  12/20/89
	**  I was reversing the order of the lines found that created
	**  the island, thinking they should be the same as areas 
	**  i.e. clockwise around the island (and doing it incorrectly.  I 
	**  should have negated line lines after reversal).  The correct
	**  soln is to leave them as found going counter clockwise around
	**  island.
	*/
	line = Area->lines[i];
	TO->lines[i] = line;		/* copy line info */

	/* all code must be cleaned up */
	if (line < 0)	/*ISLE*/		/* reference lines to isle */
	{
#ifdef DEBUG
if (map->Line[abs(line)].left)
debugf ("Line %d already had isle %d to left.\n", line, map->Line[abs(line)].left);
#endif
	    map->Line[abs(line)].left = -isle;
	}
	else
	{
#ifdef DEBUG
if (map->Line[line].right)
debugf ("Line %d already had isle %d to right.\n", line, map->Line[line].right);
#endif
	    map->Line[line].right = -isle;
	}
    }
    TO->n_lines = Area->n_lines;
    return (isle);
}


/*
** 
**  mark isle as deleted,  delete its attribute 
**  unmark references to it in Line array
*/
int dig_del_isle ( struct Map_info *map, int isle)
{
    register int i, line;
    P_ISLE *Isle;

    isle = abs (isle);
    Isle = &(map->Isle[isle]);

    for (i = 0 ; i < Isle->n_lines ; i++)	/* delete references in lines */
    {
	line = Isle->lines[i];
	if (line < 0)	/* ISLE */
	{
	    map->Line[abs(line)].left = 0;
	}
	else
	    map->Line[line].right = 0;
    }
    if (Isle->alloc_lines)
    {
	free (Isle->lines);		/* release memory */
    }
    Isle->alloc_lines = 0;
    Isle->n_lines = 0;

    Isle->alive = 0;	/* and mark itself as deleted */

    return (0);
}

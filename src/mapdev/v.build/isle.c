/*ISLE*/
#include "Vect.h"
#include "vbuildlib.h"

int 
matchup_isles (struct Map_info *map)
{
    register int i;

    for (i = 1 ; i <= map->n_isles ; i++)
    {
	if (0 > matchup_isle (map, i))
	    return (-1);
    }
    return 0;
}

/* returns 0 on area not found, -1 on error and area number otherwise */
int 
matchup_isle (struct Map_info *map, register int isle)
{
    register int line, i, j;
    register int area, match;
    P_ISLE *Isle;
    P_AREA *Area;
    P_NODE *Node;
    double x, y, min_dist;

    Isle = &(map->Isle[isle]);
    line = abs (Isle->lines[0]);
    Node = &(map->Node[map->Line[line].N1]);

    x = map->Node[map->Line[line].N1].x;
    y = map->Node[map->Line[line].N1].y;

/* have to check to guarantee that the found area is not itself part
** of the island
*/
    min_dist = -1.0;
    while (1)
    {
	match = 0;
#ifdef DEBUG
debugf ("ISLE %d, pointing to next area %lf\n", isle, min_dist);
#endif
	if (0 == (area = dig_point_to_next_area (map, x, y, &min_dist)))
	{
#ifdef DEBUG
debugf ("Could not find area for isle %d (%lf, %lf)\n", isle, x, y);
#endif
	    Isle->area = 0;
	    return (0);
	}
#ifdef DEBUG
debugf ("point to area returned area %d\n", area);
#endif

	Area = &(map->Area[area]);
	for (j = 0 ; j < Node->n_lines ; j++)
	{
	    line = abs (Node->lines[j]);
	    for (i = 0 ; i < Area->n_lines ; i++)
	    {
		if (line == abs (Area->lines[i]))
		{
		    match = 1;
		    goto foundone;
		}
	    }
	}
foundone:
	if (!match)   /* no lines in island are in area */
		break;
    }

    Isle->area = area;
    if (0 > dig_area_alloc_isle (Area, Area->n_isles + 1))
    {
	return (-1);
    }

    Area->isles[Area->n_isles++] = isle;
    return (area);
}

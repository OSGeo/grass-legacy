#include "Vect.h"

#define MAX_TRIES 4

extern struct line_pnts Points;

int 
label_interval (struct Map_info *map, double interval)
{
#ifdef FOO
    plus_t line;
    int segment;
    P_LINE *Line;

    if (interval == 0.0)
	return (-1);

    for (line = 1 ; line <= map->n_lines ; line++)
    {
	Line = &(map->Line[line]);

	if (LINE_LABELED (Line))
	    continue;

	if (0 > dig__Read_line (&Points, map->digit, Line->offset)) 
	{
	    fprintf (stderr, "interval: Can't read line %d\n", line);
	    exit (-1);
	}
	limit = LESSER (MAX_TRIES, Points.n_points-1);
	segment = rand () % Points.n_points-1;

    }


#endif
    return (0);
}

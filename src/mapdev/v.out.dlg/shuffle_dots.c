#include "Vect.h"
#include "local_proto.h"

static int next_dot(struct Map_info *,int);
static int next_undot (struct Map_info *,int);

int shuffle_dots (struct Map_info *map)
{
    P_LINE *Line;
    register int hi, lo;

    Line = map->Line;
    lo = 0;
    hi = map->n_lines + 1;

    while (1)
    {
	lo = next_dot (map, lo);
	hi = next_undot (map, hi);

	if (lo > map->n_lines || hi < 1)
	    break;
	if (lo >= hi)
	    break;
	line_swap (map, lo, hi);
    }

    return 0;
}

static int next_dot(struct Map_info *map,int lo)
{
    while (++lo)
    {
	if (lo > map->n_lines || map->Line[lo].type == DOT)
	    break;
    }
    return (lo);
}

static int next_undot (struct Map_info *map,int hi)
{
    while (--hi)
    {
	if (map->Line[hi].type != DOT)
	    break;
    }
    return (hi);
}

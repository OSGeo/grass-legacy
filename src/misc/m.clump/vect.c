#include "glob.h"

static struct line_pnts *points = NULL;
static struct Map_info mapinfo;

char *
find_vector(name)
    char *name;
{
    char *mapset;
    mapset = G_find_vector (name, "");
    if (mapset == NULL)
    {
	fprintf (stderr, "ERROR: vector file [%s] not found\n", name);
	exit(1);
    }
    return mapset;
}

void
open_vector_map (name)
    char *name;
{
    char *mapset;

    mapset = find_vector(name);

    if (points == NULL)
	points = Vect_new_line_struct ();
    if (points == NULL)
    {
	fprintf (stderr, "ERROR: Out of memory\n");
	exit(1);
    }

    Vect_set_open_level (1);
    if (Vect_open_old (&mapinfo, name, mapset) < 1)
    {
	fprintf (stderr, "ERROR: can't properly open vector file %s\n", name);
	exit(1);
    }
}

void
close_vector_map ()
{
    Vect_close (&mapinfo);
}

read_next_vector_arc (arc)
    struct arc *arc;
{
    switch (Vect_read_next_line (&mapinfo, points))
    {
    case -1:
	fprintf (stderr, "ERROR reading vector file\n");
	exit(1);
	break;
    case -2: /* EOF */
	arc->x = arc->y = NULL;
	arc->n = 0;
	return 0;
    }
    arc->x = points->x;
    arc->y = points->y;
    arc->n = points->n_points;
    return 1;
}

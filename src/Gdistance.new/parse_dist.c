#include "distance.h"
parse_distances (argc, argv, to_meters)
    char *argv[];
    double to_meters;
{
    double dist;
    double ew2;
    int i;
    int count;
    int cmp();

    count = argc;
    if (count <= 0) return 0;

	/* create an array to hold the distances */

    distances = (struct Distance *) G_calloc (count, sizeof(struct Distance));

	/* scan the command line for the distances */

    if (window.proj != PROJECTION_LL)
    {
	ew2 = window.ew_res * window.ew_res;
	ns_to_ew_squared = window.ns_res * window.ns_res / ew2;
    }
    for (i = 0; i < count; i++)
    {
	if (!scan_dist(argv[i], &dist))
	{
	    fprintf (stderr, "%s: %s - illegal distance specification\n",
		pgm_name, argv[i]);
	    return 0;
	}
	else
	{
	    dist *= (to_meters * meters_to_grid);
	    if (window.proj != PROJECTION_LL)
		dist = dist*dist/ew2;
	    distances[i].dist = dist;
	}
    }

	/* sort the distances in increasing order */

    qsort (distances, count, sizeof(struct Distance), cmp);

    return count;
}

static
cmp (a, b)
    struct Distance *a, *b;
{
    if (a->dist < b->dist) return -1;
    return a->dist > b->dist;
}

static
scan_dist (s, dist)
    char *s;
    double *dist;
{
    char dummy[2];

    *dummy = 0;
    if (sscanf (s, "%lf%1s", dist, dummy) != 1) return 0;
    if (*dummy) return 0;
    if (*dist <= 0.0) return 0;
    return 1;
}

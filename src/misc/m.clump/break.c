#include "glob.h"

/* break those connections that either don't share same attributes
 * or are seperated by a vector line
 */

/* only process edges in one direction pt1 < pt2
 * also if pt2 < 0 the edge isn't there
 */
#define skip(pt1,pt2) (pt2 < pt1)

void
break_connections()
{
    int pt1, pt2, count, i, n;
    struct arc arc;
    struct box box_a, box_p;
    double x1,y1,x2,y2;
    char *name;


/* attributes  */
    if (have_attributes())
    {
	if (!be_quiet())
		fprintf (stderr, "Processing attributes ...\n");
	for (pt1 = 0; pt1 < pointlist.npoints; pt1++)
	{
	    count = get_number_of_neighbors (pt1);
	    for (n = 0; n < count; n++)
	    {
		pt2 = get_neighbor (pt1, n);
		if (skip(pt1, pt2))
		    continue;
		if (!attributes_are_the_same (pt1, pt2))
		    disconnect (pt1, pt2);
	    }
	}
    }

/* vector barriers */
    for (i = 0; name = (char *) get_barrier_name(i); i++)
    {
	if (!be_quiet())
	    fprintf (stderr, "Processing vector barrier map [%s] ...\n", name);
	open_vector_map (name);
	if (i == 0)
	    get_pointlist_bounding_box (&box_p);
	while (read_next_vector_arc (&arc))
	{
	    get_arc_bounding_box (&arc, &box_a);
	    if(!boxes_overlap (&box_a, &box_p))
		continue;

	    for (pt1 = 0; pt1 < pointlist.npoints; pt1++)
	    {
		x1    = pointlist.x[pt1];
		y1    = pointlist.y[pt1];

		count = get_number_of_neighbors (pt1);
		for (n = 0; n < count; n++)
		{
		    pt2 = get_neighbor (pt1, n);
		    if (skip(pt1, pt2))
			continue;

		    x2    = pointlist.x[pt2];
		    y2    = pointlist.y[pt2];

		    if (segment_may_fall_in_box (x1, y1, x2, y2, &box_a)
		    &&  arc_intersects_segment (&arc, x1, y1, x2, y2))
			disconnect (pt1, pt2);
		}
	    }
	}
	close_vector_map();
    }
}

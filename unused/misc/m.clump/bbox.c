#include "glob.h"

void
get_bounding_box (x, y, n, box)
    double *x, *y;
    int n;
    struct box *box;
{
    int i;

    if (n > 0)
    {
	box->min_x = box->max_x = *x++;
	box->min_y = box->max_y = *y++;
    }
    else
    {
	box->min_x = box->max_x = 0;
	box->min_y = box->max_y = 0;
    }
	

    for (i = 1; i < n; i++)
    {
	if (*x > box->max_x) box->max_x = *x;
	if (*x < box->min_x) box->min_x = *x;
	x++;

	if (*y > box->max_y) box->max_y = *y;
	if (*y < box->min_y) box->min_y = *y;
	y++;
    }
}

void
get_arc_bounding_box (arc, box)
    struct arc *arc;
    struct box *box;
{
    get_bounding_box (arc->x, arc->y, arc->n, box);
}

void
get_pointlist_bounding_box (box)
    struct box *box;
{
    get_bounding_box (pointlist.x, pointlist.y, pointlist.npoints, box);
}

int
segment_may_fall_in_box (x1, y1, x2, y2, box)
    double x1, y1, x2, y2;
    struct box *box;
{
    if(y1 > box->max_y && y2 > box->max_y) return 0;
    if(y1 < box->min_y && y2 < box->min_y) return 0;

    if(x1 > box->max_x && x2 > box->max_x) return 0;
    if(x1 < box->min_x && x2 < box->min_x) return 0;

    return 1;
}

boxes_overlap (box1, box2)
    struct box *box1, *box2;
{
    if (box1->max_x < box2->min_x) return 0;
    if (box2->max_x < box1->min_x) return 0;

    if (box1->max_y < box2->min_y) return 0;
    if (box2->max_y < box1->min_y) return 0;

    return 1;
}

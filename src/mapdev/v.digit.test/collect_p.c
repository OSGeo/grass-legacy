/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/


/*  
*    Front end for now  to the real  collect_points in libes/digitizers
*
*   collect_points() - collect a set of points.
*    coll_a_pnt() -  collect a point from digitizer.
*/

#include "Vect.h"
#include "local_proto.h"

int Collect_points(
    int mode,
    char type,
    struct line_pnts *p)
{
    register int ret;

    set_priority ();

    /* collect_points still uses OLD type from mode.h */
    /* this is temporary till i can get around to fixing it */
    /*
    type = dig_new_to_old_type (type);
    */

    ret = collect_points (mode, (int) type, &(p->n_points), &(p->x), &(p->y));
    unset_priority ();
    return (ret);
}

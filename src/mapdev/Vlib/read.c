#include "V_.h"


int
Vect_read_next_line (Map, line_p)
    struct Map_info *Map;
    struct line_pnts *line_p;
{
    if (!VECT_OPEN (Map))
	return -1;

    switch (Map->level) {
	case 1:
	    return V1_read_next_line (Map, line_p);
	    /*NOTREACHED*/
	    break;
	case 2:
	    return V2_read_next_line (Map, line_p);
	    /*NOTREACHED*/
	    break;
	case 3:
	default:
	    return -1;
    }

    /*NOTREACHED*/
}

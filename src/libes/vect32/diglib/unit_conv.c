#include "gis.h"
#include "Vect.h"

/* 
**  HACK.  this is just to get us up this round.
**   in the future we would like to try to support different
**   coordinate systems, and we need a conversion factor from
**   meters to whatever units are needed
**
**  What this is tho, is conversions from digitizer units (inches)
**  to another given unit
*/
double dig_unit_conversion ()
{
    /*  3.x code
    units = G_projection_units(); 
    switch (units) {
	case -1:
	    fprintf (stderr, "Unknown Projection.\n");
	    exit (-1);
	    break;
	case 0:
	    return (1.0);
	    break;
	case FEET:
	    return (0.083);
	    break;
	case METERS:
	    return (0.0254);
	    break;
    }
    return (0.0254);
    */

    /* and the 4.0 version */
    return G_database_units_to_meters_factor () * 0.0254;
}

#include "global.h"
#include <string.h>

int 
conv_units ()
{
    int     i;
    double  f=1.0, sq_f=1.0;

    switch ( options.units)
    {
	case U_METERS:
	    f      = 1.0;
	    sq_f   = 1.0;
	    break;

	case U_KILOMETERS:
	    f      = 1.0e-3;
	    sq_f   = 1.0e-6;
	    break;

	case U_ACRES:
	    sq_f   = 2.471e-4;
	    break;

	case U_HECTARES:
	    sq_f   = 1.0e-4;
	    break;

	case U_MILES:
	    f      = 6.213e-4;
	    sq_f   = 3.861e-7;
	    break;

	case U_FEET:
	    f      = 3.2808;
	    sq_f   = 10.7639;
	    break;
    }

    switch (options.option) {
	case O_LENGTH:
	    for ( i = 0; i < vstat.rcat; i++ ) 
		list_cd[i].d1 *= f; 
	    break;
	case O_AREA:
	    for ( i = 0; i < vstat.rcat; i++ ) 
		list_cd[i].d1 *= sq_f; 
	    break;
	
	case O_COOR: 
	    for ( i = 0; i < vstat.rcat; i++ ) {
		list_ci2d[i].d1 *= f;
		list_ci2d[i].d2 *= f;
	    }
	    break;
    }

    return 0;
}

#include "Vect.h"
#include "conv.h"

/* conversion of old file format elment type codes to new */ 
char dig_old_to_new_type (char type)
{
    switch (type) {
	case FILE_LINE:
	    type = LINE;
	    break;
	case FILE_AREA:
	    type = BOUNDARY;
	    break;
	case FILE_DOT:
	    type = DOT;
	    break;
	case FILE_DEAD_LINE:
	    type = DEAD_LINE;
	    break;
	case FILE_DEAD_AREA:
	    type = DEAD_BOUNDARY;
	    break;
	case FILE_DEAD_DOT:
	    type = DEAD_DOT;
	    break;
	default:
	    fprintf (stderr, "SYSTEM_ERROR: OLD_T_NEW Got a bad type code %x\n", type);
	    type = 0;
	    break;
    }
    return (type);
}

/* conversion of new element types to old file format elment type codes */ 
char dig_new_to_old_type ( char  type)
{
    switch (type) {
	case LINE:
	    type = FILE_LINE;
	    break;
	case BOUNDARY:
	    type = FILE_AREA;
	    break;
	case DOT:
	    type = FILE_DOT;
	    break;
	case DEAD_LINE:
	    type = FILE_DEAD_LINE;
	    break;
	case DEAD_BOUNDARY:
	    type = FILE_DEAD_AREA;
	    break;
	case DEAD_DOT:
	    type = FILE_DEAD_DOT;
	    break;
	default:
	    fprintf (stderr, "SYSTEM_ERROR: NEW_T_OLD Got a bad type code %x\n", type);
	    type = 0;
	    break;
    }
    return (type);
}



#include "Vect.h"

/*
**   TYPE codes:		 old codes are still supported in dig file
**
**		OLD		NEW
** LINE	 	 0		0x01
** AREA	 	 1		0x02
** DOT	 	 2		0x04
**
** DEAD_LINE	 4		0x10
** DEAD_AREA	 5		0x20
** DEAD_DOT	 6		0x40
*/

char dig_old_to_new_type (char type)
{
    /* see defines.h for new and old codes */
    /* old codes are  FILE_* 		   */
    /*
    type = 1 << type;
    return (type);
    */
    switch (type) {
	case FILE_LINE:
	    type = LINE;
	    break;
	case FILE_AREA:
	    type = AREA;
	    break;
	case FILE_DOT:
	    type = DOT;
	    break;
	case FILE_DEAD_LINE:
	    type = DEAD_LINE;
	    break;
	case FILE_DEAD_AREA:
	    type = DEAD_AREA;
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

char dig_new_to_old_type ( char  type)
{
    switch (type) {
	case LINE:
	    type = FILE_LINE;
	    break;
	case AREA:
	    type = FILE_AREA;
	    break;
	case DOT:
	    type = FILE_DOT;
	    break;
	case DEAD_LINE:
	    type = FILE_DEAD_LINE;
	    break;
	case DEAD_AREA:
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

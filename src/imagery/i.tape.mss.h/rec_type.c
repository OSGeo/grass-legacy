/*********************************************************************

NAME:		record_type ()

FUNCTION:       print record type

USAGE:		record_type(type)
		unsigned char type

**********************************************************************/
#include "tape.h"

int record_type (int type)
{
    char *name;

    fprintf (stdout, "\n");
    switch ((int)type)
    {
    case TAPE_DIR:	name = "TAPE_DIR:"; break;
    case HEADER:	name = "HEADER:"; break;
    case ANCILLARY:	name = "ANCILLARY:"; break;
    case ANNOTATION:	name = "ANNOTATION:"; break;
    case IMAGE:		name = "IMAGE:"; break;
    case TRAILER:	name = "TRAILER:"; break;

    default:	fprintf (stdout,"0%o: ", (int)type);
		name = "** unknown record type **";
		break;
    }

    fprintf (stdout,"%s\n\n", name);

    return 0;
}

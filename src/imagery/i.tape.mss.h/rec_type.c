/*********************************************************************

NAME:		record_type ()

FUNCTION:       print record type

USAGE:		record_type(type)
		unsigned char type

**********************************************************************/
#include "tape.h"

record_type(type)
    unsigned char type;
{
    char *name;

    printf ( "\n");
    switch ((int)type)
    {
    case TAPE_DIR:	name = "TAPE_DIR:"; break;
    case HEADER:	name = "HEADER:"; break;
    case ANCILLARY:	name = "ANCILLARY:"; break;
    case ANNOTATION:	name = "ANNOTATION:"; break;
    case IMAGE:		name = "IMAGE:"; break;
    case TRAILER:	name = "TRAILER:"; break;

    default:	printf("0%o: ", (int)type);
		name = "** unknown record type **";
		break;
    }

    printf ("%s\n\n", name);
}

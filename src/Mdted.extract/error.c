/* %W% %G% */

#include "dma.h"

error (msg, system_error) char *msg;
{
    fprintf (stderr, "%s: ERROR: ", PGM);
    if (system_error)
	perror (msg);
    else
	fprintf (stderr, "%s\n", msg);
}

#include "dma.h"

error (msg, system_error) char *msg;
{
    fprintf (stderr, "%s: ERROR: ", G_program_name());
    if (system_error)
	perror (msg);
    else
	fprintf (stderr, "%s\n", msg);
}

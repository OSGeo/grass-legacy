#include "dma.h"

int 
error (char *msg, int system_error)
{
    fprintf (stderr, "%s: ERROR: ", G_program_name());
    if (system_error)
	perror (msg);
    else
	fprintf (stderr, "%s\n", msg);

    return 0;
}

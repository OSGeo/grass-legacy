/* %W% %G% */
#include "gis.h"
list_drivers()
{
    char buf[1024];
    char *drivers();

    printf ("\nAvailable PAINTERS\n\n");
    sprintf (buf, "ls %s\n", drivers(""));
    system (buf);
}

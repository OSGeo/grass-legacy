#define GLOBAL
#include "P.h"

Popen (port)
    char *port;
{
    int i;
    if (port == NULL || *port == 0)
	out = stderr ;
    else
	out = fopen (port, "w");
    if (out == NULL)
    {
	char msg[100];
	sprintf (msg, "unable to open %s", port);
	error (msg);
    }
}

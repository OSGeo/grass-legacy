/* %W% %G% */
#define GLOBAL
#include "P.h"

Popen (port)
    char *port;
{
    int zero = 0;

    punit = 8;

    {
	popen_ (&punit);
	plots_ (&zero, &zero, &punit);
    }
}

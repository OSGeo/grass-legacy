#include <stdlib.h>
#include "P.h"
Pnpixels (rows, cols)
    int *rows, *cols;
{
    char *s;

    s = getenv ("NPIXELS");
    if (s == NULL)
	error ("NPIXELS not set");
    if (sscanf (s, "%d", cols) != 1 || *cols <= 0)
	error ("NPIXELS not valid");
    *rows = 0;
}

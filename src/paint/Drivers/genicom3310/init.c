#include <stdlib.h>
#define GLOBAL
#include "P.h"
Pinit()
{
    char *q;

    quality = 1;

    if (q = getenv ("QUALITY"))
    {
	if (strcmp (q,"2") == 0) quality = 2;
	else if (strcmp (q,"3") == 0) quality = 3;
    }

    ras_row = 0;
    if (quality == 3)
	ras_nrows = 6;
    else
	ras_nrows = 3;
    formfeed();
    esc("[r");	/* reset forms */
}

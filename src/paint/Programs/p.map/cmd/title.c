#include <string.h>
#include "gis.h"
#include "Paintlib.h"

int title (char *string)
{
    char buf[100];
    char temp[100];
    static int first = 1;

    if (!first) return 0;
    first = 0;

    strcpy (temp, string);
    G_strip (temp);
    if (*temp)
    {
	sprintf (buf, "%-13s %s", "TITLE:", temp);
	Ptext (buf);
    }

    strcpy (temp, G_myname()) ;
    G_strip(temp);
    if (*temp == 0)
	strcpy (temp, G_location()) ;

    sprintf (buf, "%-13s %s", "LOCATION:", temp);
    Ptext (buf);
    Ptext("");

    return 0;
}

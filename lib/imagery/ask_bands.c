#include <string.h>
#include <stdlib.h>
#include <grass/gis.h>
#include <grass/imagery.h>

static char prefix[100];

char *I_bandname (int n)
{
    static char name[INAME_LEN];

    sprintf (name, "%s.%d",prefix,n+1);
    return name;
}

char *I_bandname_prefix()
{
    return prefix;
}

int I_set_band_prefix(char *p)
{
	strcpy (prefix, p);

	return 0;
}


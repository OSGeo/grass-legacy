#include <stdio.h>
#include "gis.h"
#include "ps_map.h"

extern char *LOCATION;
extern char *MAPSET;
extern char *MASK;

#define LEN 13

int newscreen (int pflag)
{
    fprintf (stdout,"%*s\n\n",35,"PS-PAINT");
    fprintf (stdout,"%-*s %s\n", LEN, "PSPAINTER:", PSpainter_name());
    fprintf (stdout,"%-*s %s\n", LEN, "LOCATION:", LOCATION);
    fprintf (stdout,"%-*s %s\n", LEN, "MAPSET:", MAPSET);
    fprintf (stdout,"%-*s %s\n\n", LEN, "MASK:", MASK);

    if (pflag)
	print_record();

    return 0;
}

char *PSpainter_name (void)
{
    char *name;
    static char *none = "none selected, using default configuration";

    name = G__getenv("PSPAINTER");
    if (name == NULL) return none;
    return name;
}

#include <stdio.h>

extern char *LOCATION;
extern char *MAPSET;
extern char *MASK;

#define LEN 13

newscreen (pflag)
{
    char *PSpainter_name();
    G_clear_screen();

    printf ("%*s\n\n",35,"PS-PAINT");
    printf ("%-*s %s\n", LEN, "PSPAINTER:", PSpainter_name());
    printf ("%-*s %s\n", LEN, "LOCATION:", LOCATION);
    printf ("%-*s %s\n", LEN, "MAPSET:", MAPSET);
    printf ("%-*s %s\n\n", LEN, "MASK:", MASK);

    if (pflag)
	print_record();
}

char *PSpainter_name()
{
    char *name, *G__getenv();
    static char *none = "none selected, using default configuration";

    name = G__getenv("PSPAINTER");
    if (name == NULL) return none;
    return name;
}

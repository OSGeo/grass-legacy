#include <stdio.h>

extern char *LOCATION;
extern char *MAPSET;
extern char *MASK;

#define LEN 11

newscreen (pflag)
{
    char *Ppainter_name();
    G_clear_screen();

    fprintf (stdout,"%*s\n\n",35,"PAINT");
    fprintf (stdout,"%-*s %s\n", LEN, "PAINTER:", Ppainter_name());
    fprintf (stdout,"%-*s %s\n", LEN, "LOCATION:", LOCATION);
    fprintf (stdout,"%-*s %s\n", LEN, "MAPSET:", MAPSET);
    fprintf (stdout,"%-*s %s\n\n", LEN, "MASK:", MASK);

    if (pflag)
	print_record();
}

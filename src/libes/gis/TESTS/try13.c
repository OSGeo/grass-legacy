#include "gis.h"
#include "signal.h"

main()
{
    char name[30], *mapset;
    struct Colors colr;
    int r,g,b;
    CELL n;
    char buf[100];
    FILE *more, *popen();

    signal (SIGPIPE, SIG_IGN);
    G_gisinit("try");
    while (mapset = G_ask_cell_old ("", name))
    {
	if(G_make_colors (name, mapset, &colr) < 0)
		continue;
	more = popen ("more","w");
	if (more == NULL) more = stdout;
	if (colr.min > 0)
	{
	    G_get_color ((CELL)0, &r, &g, &b, &colr);
	    fprintf (more, "%d: %d %d %d\n", 0, r, g, b);
	}
	for (n = colr.min; n <= colr.max; n++)
	{
	    G_get_color (n, &r, &g, &b, &colr);
	    fprintf (more, "%ld: %d %d %d\n", (long)n, r, g, b);
	}
	if (colr.max < 0)
	{
	    G_get_color ((CELL)0, &r, &g, &b, &colr);
	    fprintf (more, "%d: %d %d %d\n", 0, r, g, b);
	}
	if (more != stdout) pclose(more);
	printf ("Update? ");
	if (!gets(buf)) exit(0);
	if (*buf == 'y')
	{
	    printf ("updating\n");
	    G_write_colors (name, mapset, &colr);
	}
	G_free_colors (&colr);
    }
}

#include "gis.h"

static char *color[] =
{
	"black",
	"blue",
	"gray",
	"green",
	"indigo",
	"orange",
	"red",
	"violet",
	"white",
	"yellow",

	NULL
};

char *
getcolor (argc, argv, msg, n)
    char **argv;
    char *msg;
    int *n;
{
    static char name[128];
    int i;

    if (argc)
    {
	*n = 1;
	if (find_color(argv[0]))
	    return(argv[0]);
    }
    *n = argc;
    show_colors();
    if (!isatty(0)) return NULL;

    while(1)
    {
	printf ("select a color for %s: ", msg);
	if (!G_gets(name)) continue;
	G_strip (name);
	if (*name == 0) return NULL;
	if(find_color (name))
	    break;
	show_colors();
    }

    return name;
}

find_color (name) char *name;
{
    int i;

    for (i = 0; color[i]; i++)
	if (strcmp (name, color[i]) == 0)
		return 1;
    printf ("[%s] - invalid color\n\n", name);
    return 0;
}

show_colors()
{
    int i;

    printf ("\nAvailable colors:\n");
    for (i = 0; color[i]; i++)
	printf ("%s%s",  i%5?" ":"\n\t", color[i]);
    printf ("\n\n");
}

#include "gis.h"

static char *icons[] =
{
	"+",
	"x",
	"b(ox)",
	"d(iamond)",

	NULL
};

char *
geticon (argc, argv, msg, n)
    char **argv;
    char *msg;
    int *n;
{
    static char name[128];
    int i;

    if (argc)
    {
	strcpy (name, argv[0]);
	*n = 1;
	if (find_icon(name))
	    return(name);
    }
    *n = argc;
    show_icons();
    if (!isatty(0)) return NULL;

    while(1)
    {
	printf ("select a icon for %s: ", msg);
	if (!G_gets(name)) continue;
	G_strip (name);
	if (*name == 0) return NULL;
	if(find_icon (name))
	    break;
	show_icons();
    }

    return name;
}

find_icon (name) char *name;
{
    int i;

    switch (*name)
    {
    case '+': strcmp (name, "+"); return 1;
    case 'b': strcpy (name,  "box"); return 1;
    case 'd': strcpy (name,  "diamond"); return 1;
    case 'x': strcpy (name,  "x"); return 1;
    }
    printf ("[%s] - invalid icon\n\n", name);
    return 0;
}

show_icons()
{
    int i;

    printf ("\nAvailable icons:");
    for (i = 0; icons[i]; i++)
	printf (" %s", icons[i]);
    printf ("\n\n");
}

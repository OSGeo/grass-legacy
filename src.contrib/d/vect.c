#include <stdio.h>
#include "level.h"
vect (argc, argv) char **argv;
{
    char *getname();
    char *getcolor();
    char *name;
    char *color;
    char command[128];
    char name_part[50];
    int n;

    name = getname (argc, argv, "dig", "vector", &n);
    if (!name) return 0;
    sscanf (name, "%s", name_part);

    argv += n;
    argc -= n;
    color = getcolor (argc, argv, "for the vector lines", &n);

    if (color == NULL)
    {
	if(isatty(0)) return 1;
	color = "";
    }
    sprintf (command, "echo 'vector(%s)'", name_part);
    script (command,VECTOR_LEVEL);
    sprintf (command, "Dvect '%s' %s > /dev/null", name, color);
    script (command,VECTOR_LEVEL);
    printf ("vector(%s)\n", name_part);
    G_system (command);
    return 1;
}

#include "level.h"
sites (argc, argv) char **argv;
{
    char *getname();
    char *name;
    char *geticon(), *icon;
    char *getcolor(), *color;
    int  getsize(), size;
    char command[128];
    char name_part[50];
    int n;

    name = getname (argc, argv, "site_lists", "sites", &n);
    if (!name) return 0;
    sscanf (name, "%s", name_part);

    argc -= n;
    argv += n;

    color = getcolor (argc, argv, name, &n);
    if (!color) return 0;

    argc -= n;
    argv += n;

    icon = geticon (argc, argv, name, &n);
    if (!icon) return 0;

    argc -= n;
    argv += n;

    size = getsize (argc, argv, name, &n);
    if (!size) return 0;

    sprintf (command, "echo 'sites(%s)'", name_part);
    script (command,SITES_LEVEL);
    sprintf (command, "Gsites -w '%s' | Dpoints %s %d %s", name, color, size, icon);
    script (command,SITES_LEVEL);
    printf ("sites(%s)\n", name_part);
    G_system (command);
}

#include "level.h"
overlay (argc, argv) char **argv;
{
    char *getname();
    char *name;
    char command[128];
    int n;
    char name_part[50];

    name = getname (argc, argv, "cell", "cell", &n);
    if (!name) return 0;

    sscanf (name, "%s", name_part);

    sprintf (command, "echo 'overlay(%s)'", name_part);
    script (command,OVERLAY_LEVEL);

    sprintf (command, "Doverlay '%s'", name);
    script (command,OVERLAY_LEVEL);

    printf ("overlay %s\n", name_part);
    G_system (command);
    run_script (0, OVERLAY_LEVEL+1);
}

#include "level.h"
labels (argc, argv) char **argv;
{
    char *getname();
    char *name;
    char command[128];
    int n;
    char name_part[50];


    name = getname (argc, argv, "paint/labels", "labels", &n);
    if (!name) return 0;

    sscanf (name, "%s", name_part);

    sprintf (command, "echo 'labels(%s)'", name_part);
    script (command, LABEL_LEVEL);

    sprintf (command, "Dpaint.labels '%s'", name);
    script (command, LABEL_LEVEL);

    run_script (0, LABEL_LEVEL);
}

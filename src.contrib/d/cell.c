#include <stdio.h>
cell (argc, argv) char **argv;
{
    char *getname();
    char *name;
    char command[128];
    int n;

    if (argc == 1 && strcmp (argv[0], "-") == 0)
	name = NULL;
    else
    {
	name = getname (argc, argv, "cell", "cell", &n);
	if (!name) return 0;
    }

    set_cell (name);
    run_script(0,0);
}

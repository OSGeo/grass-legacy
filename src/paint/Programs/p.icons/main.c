#include "gis.h"

main(argc, argv) char *argv[];
{
    G_gisinit (argv[0]);

    while (1)
    {
	switch (menu())
	{
	case 1: preview_icons(); break;
	case 2: edit_icons(); break;
	default: exit(0);
	}
    }
}

#include "gis.h"
main(argc, argv) char *argv[];
{
    char path[1024];
    char *element;
    char *mapset;
    char *name;

    G_gisinit (argv[0]);

    if (argc != 4)
    {
	fprintf (stderr, "Usage: %s element mapset name\n", argv[0]);
	exit(1);
    }
    element = argv[1];
    mapset = argv[2];
    name = argv[3];

    if (strcmp (mapset, ".") == 0 || strcmp (mapset, "") == 0)
	mapset = G_mapset();

    G__make_mapset_element(element);
    G__file_name (path, element, name, mapset);

    printf ("file='%s'\n", path);
    exit(0);
}

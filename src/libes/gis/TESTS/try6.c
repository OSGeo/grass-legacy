#include "gis.h"

main()
{
    char name[30], *mapset;
    char rname[30], rmapset[30];
    struct Reclass reclass;
    int stat;
    int i;

    mapset = G_ask_cell_old ("", name);
    if (mapset == NULL) exit(0);
    G_open_cell_old (name, mapset);

    stat = G_get_reclass (name, mapset, &reclass);
    printf ("stat=%d\n", stat);
    if (stat <= 0) exit(0);
    printf ("reclass [%s in %s] (%d)\n", reclass.name, reclass.mapset, reclass.num);
    for (i = 0; i < reclass.num; i++)
	printf (" %ld->%ld\n", i+reclass.min, reclass.table[i]);
}

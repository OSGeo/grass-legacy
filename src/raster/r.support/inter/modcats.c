
#include <string.h>
#include "gis.h"

int 
main (int argc, char *argv[])
{
    char name[128];
    char *mapset;
    struct Categories cats;
    static int vector = 0;
    int stat;

    G_gisinit(argv[0]) ;
    if (argc > 1 && strcmp (argv[1], "-v") == 0)
    {
	vector = 1;
	argc--;
	argv++;
    }
    if (argc < 2)
    {
	if (vector)
	    mapset = G_ask_vector_in_mapset ("Which vector file needs updated categories", name);
	else
	    mapset = G_ask_cell_in_mapset ("Which raster file needs updated categories", name);
	if (mapset == NULL) exit(0);
    }
    else
    {
	strcpy (name, argv[1]);
	if (vector)
	    mapset = G_find_vector2 (name, G_mapset());
	else
	    mapset = G_find_cell2 (name, G_mapset());
	if (mapset == NULL)
	{
	    fprintf (stderr, "%s - %s file not found\n", argv[1],
		vector?"vector":"raster");
	    exit(1);
	}
    }
    if (vector)
	stat = G_read_vector_cats (name, mapset, &cats);
    else
	stat = G_read_cats (name, mapset, &cats);
    if (stat < 0)
	G_init_cats ((CELL)0, "", &cats);

    if(!vector && G_raster_map_is_fp(name, mapset))
    {
        if (G_edit_fp_cats (name, &cats) < 0)
        {
    	    fprintf (stdout,"Category file for [%s] not updated\n", name);
	    exit(0);
        }
    }
    else
    {
        if(G_edit_cats (name, &cats, stat<0) < 0)
        {
	    fprintf (stdout,"Category file for [%s] not updated\n", name);
	    exit(0);
        }
    }
    if (vector)
	G_write_vector_cats (name, &cats);
    else
	G_write_cats (name, &cats);
    fprintf (stdout,"Category file for [%s] updated\n", name);
    exit(0);
}

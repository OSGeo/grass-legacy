#include "gis.h"

static char rcsid[]="$Header: /gis3/4.0/src/raster/r.support/RCS/modcats.c,v 1.1 1991/09/28 21:51:01 paul Exp $";



main (argc, argv) char *argv[];
{
    char name[40];
    char *mapset;
    struct Categories cats;
    static int vector = 0;
    int stat;
/*ADD MLH 1/23/92 */
	CELL tmpnum=0;
	char *tmpstr;
/*END ADD */

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
/* ADD/CHANGE MLH 1/23/92 */
	if (cats.num > 0){
		tmpnum = cats.num;
		tmpstr = cats.title;
		}
    if (stat < 0) {
		if (tmpnum > 0) 
			G_init_cats (tmpnum, tmpstr, &cats);
		else
			G_init_cats ((CELL)0, "", &cats);
	}
/* END change */

    if(G_edit_cats (name, &cats, 0) < 0)
    {
	printf ("Category file for [%s] not updated\n", name);
	exit(0);
    }
    if (vector)
	G_write_vector_cats (name, &cats);
    else
	G_write_cats (name, &cats);
    printf ("Category file for [%s] updated\n", name);
    exit(0);
}

#include "gis.h"

main(argc, argv)
    int argc ;
    char *argv[] ;
{
    long *table;
    int i ;
    int ncats;
    char old_name[64], *old_mapset ;
    char new_name[64], *new_mapset ;
    char real_name[64], real_mapset[64];
    int reclassed;

    struct Categories cats;
    struct Categories new_cats;
    struct Range range;
    CELL min, max;

/* Initialize GIS routines ****************************************************/
    G_gisinit(argv[0]) ;

/* Dump the advertising *******************************************************/
    G_clear_screen() ;
    fprintf(stderr,"\n\nRECLASS:  Program for reassigning category codes") ;

/* Get names for old and new files ********************************************/
    old_mapset = G_ask_cell_old(
	    "Enter name of data layer to be reclassified", old_name) ;
    if (old_mapset == NULL)
	exit(0);
    reclassed = G_is_reclass (old_name, old_mapset, real_name, real_mapset);
    if (reclassed < 0)
	exit(1);

    new_mapset = G_ask_cell_new(
	    "Enter name of NEW RECLASSIFIED map", new_name) ;
    if (new_mapset == NULL)
	exit(0);

/* Establish conversion table *************************************************/
    if(G_read_cats(old_name, old_mapset, &cats) < 0)
	exit(1);
    if(G_read_range(old_name, old_mapset, &range) < 0)
	exit(1);
    G_get_range_min_max(&range, &min, &max);

/* allocate the reclass table */
/* make sure that an int can hold the number of cats.
 * if cats.num is a long with a very large number
 * we won't be able to malloc a table big enough since
 * malloc only can handle ints
 */
    i = max - min + 1;
    if (i != (max - min+1))
	G_fatal_error ("Too many categories");
    table = (long *) G_calloc (i, sizeof(long));

/* let the user build reclass table */
    maketable (&cats, table, min, max, 0) ; 
    ncats = 0;     
    i = min;

    while (!ncats && i <= max) {
	if (table[i-min] != i)
	    ncats = 1;
	i++;
	}

    if (!ncats)
    {
	printf ("No new categories specified. [%s] not created\n", new_name);
	sleep(3);
	exit(1);
    }

/* prepare the new category labels */
    G_init_cats (0, "", &new_cats);
    set_new_cats (&cats, &new_cats, table, min, max);
    G_edit_cats (new_name, &new_cats, -1);

/* run reclass now */
    do_reclass (old_name, old_mapset, new_name, &cats, &new_cats, table, min, max);

/* Tell the user what they have just done *************************************/
    printf("\n\nYou have just reclassified map [%s in %s]\n", old_name, old_mapset) ;
    printf("into new map [%s in %s]\n", new_name, new_mapset) ;
    if (reclassed)
    {
	printf("\nNOTE:\n\n");
	printf("Map [%s in %s] is itself a reclassification of [%s in %s]\n",
	    old_name, old_mapset, real_name, real_mapset) ;
	printf("Therefore, the new map [%s] is actually a reclassification of\n", new_name) ;
	printf("the original map [%s in %s]\n", real_name, real_mapset) ;
    }
    printf("\n");
    exit(0);
}

static char rcsid[]="$Header: /usr3/4.0/src.contrib/SCS/vector/v.reclass/inter/RCS/reclass.c,v 1.1 1992/12/13 11:18:30 grass Exp grass $";

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
    int reclassed,area,dissolve;

    struct Categories cats;
    struct Categories new_cats;

/* Initialize GIS routines ****************************************************/
    G_gisinit(argv[0]) ;

/* Dump the advertising *******************************************************/
    G_clear_screen() ;
    fprintf(stderr,"\n\nRECLASS:  Program for reassigning category codes\n") ;

/* Get names for old and new files ********************************************/
    old_mapset = G_ask_vector_old(
	    "Enter name of data layer to be reclassified", old_name) ;
    if (old_mapset == NULL)
	exit(0);

    new_mapset = G_ask_vector_new(
	    "Enter name of NEW RECLASSIFIED map", new_name) ;
    if (new_mapset == NULL)
	exit(0);

/* Is this an AREA map? */
    if(area = G_yes("Is this an AREA map?", 1))
	dissolve = G_yes("Dissolve common boundaries?",1);

/* Establish conversion table *************************************************/
    if(G_read_vector_cats(old_name, old_mapset, &cats) < 0)
	exit(1);

/* allocate the reclass table */
/* make sure that an int can hold the number of cats.
 * if cats.num is a long with a very large number
 * we won't be able to malloc a table big enough since
 * malloc only can handle ints
 */
    i = cats.num + 1;
    if (i != (cats.num+1))
	G_fatal_error ("Too many categories");
    table = (long *) G_calloc (i, sizeof(long));

/* let the user build reclass table */
    maketable (&cats, table, 0) ; 
    ncats = -1;
    for (i=0; i <= cats.num; i++)
	if (table[i] != 0 && table[i] > ncats)
	    ncats = table[i];
    if (ncats < 0)
    {
	printf ("No new categories specified. [%s] not created\n", new_name);
	sleep(3);
	exit(1);
    }

/* prepare the new category labels */
    G_init_cats (ncats, "", &new_cats);
    set_new_cats (&cats, &new_cats, table);
    G_edit_cats (new_name, &new_cats, -1);

/* run reclass now */
    do_reclass (old_name, old_mapset, new_name, &cats, &new_cats, table, area, dissolve);

/* Tell the user what they have just done *************************************/
    printf("\n\nYou have just reclassified map [%s in %s]\n", old_name, old_mapset) ;
    printf("into new map [%s in %s]\n", new_name, new_mapset) ;
    printf("\n");
    exit(0);
}

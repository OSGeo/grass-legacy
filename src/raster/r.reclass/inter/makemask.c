#include "gis.h"

makemask()
{
    long *table;
    int i ;
    int ncats;
    char old_name[64], *old_mapset ;
    char real_name[64], real_mapset[64];
    int reclassed;

    struct Categories cats;
    struct Categories new_cats;


/* Get names for old and new files ********************************************/
    old_mapset = G_ask_cell_old(
	    "Enter name of data layer to be used for mask", old_name) ;
    if (old_mapset == NULL)
	return 0;

    reclassed = G_is_reclass (old_name, old_mapset, real_name, real_mapset);
    if (reclassed < 0)
	return 0;

/* Read category labels */
    if(G_read_cats(old_name, old_mapset, &cats) < 0)
	return 0;

/* allocate the reclass table */
    i = cats.num + 1;
    if (i != (cats.num+1))
	G_fatal_error ("Too many categories");

    table = (long *) G_calloc (i, sizeof(long));

/* let the user build reclass table */
    maketable (&cats, table, 0, cats.num, 1) ; 
    ncats = -1;
    for (i=0; i <= cats.num; i++)
	if (table[i])
	{
	    table[i] = 1;
	    ncats = 0;
	}
    if (ncats < 0)
    {
	free (table);
	G_free_cats (&cats);
	printf ("No new categories specified. Mask not created\n");
	hitreturn();
	return 0;
    }

/* prepare the new category labels */
    G_init_cats ((CELL)1, "", &new_cats);
    G_set_cat ((CELL)1, "Mask data", &new_cats);

/* run reclass now */
    do_reclass (old_name, old_mapset, "MASK", &cats, &new_cats, table, 0, cats.num);
    G_free_cats (&cats);
    G_free_cats (&new_cats);
    free(table);

/* Tell the user what they have just done *************************************/
    if (reclassed)
    {
	printf("\nNOTE:\n\n");
	printf("Map [%s in %s] is a reclassification of [%s in %s]\n",
	    old_name, old_mapset, real_name, real_mapset) ;
	printf("Therefore, the mask is actually based on the\n") ;
	printf("original map [%s in %s]\n", real_name, real_mapset) ;
	hitreturn();
    }
    return 1;
}

hitreturn()
{
    char buf[128];
    printf ("\nhit RETURN to continue -->");
    G_gets(buf);
}

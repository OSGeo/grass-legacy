/* %W% %G% */

#include "gis.h"
#define MAIN
#include "disfrm.h"

struct Command_keys keys[] = {
    { "oldcell", 1 },
    { "newcell", 2 },
    { "direction", 3 },
    { "old", 1 },
    { "new", 2 },
    { "dir", 3 },
    { NULL, 0 }
};

int stash_keys ();
int my_help ();
int Got_old = 0, Got_new = 0, ToFrom = 0;
char *Newfile, *Oldfile;
char *old_mapset;

main (argc, argv)
    int argc;
    char *argv[];
{
    int oldcell, newcell ;
    struct Categories cats;
    int search_cell ;
    int search_area ;
    int all_cell ;
    int ans ;
    double from_cell_time, to_cell_time ;
    double sqrt() ;
    char new_name[30] ;
    char old_name[30] ;
    char *old_mapset ;
    char buffer[256] ;
    struct Cell_head window ;
    int dist_cnt;
    int ret;

    char tmpbuf[512];
    FILE *popen (), *Pipe;
      
    G_gisinit (argv[0]);

    G_set_parse_command_usage (my_help);
    if ((ret = G_parse_command (argc, argv, keys, stash_keys)) < 0)
    {
	my_help (argv[0], keys);
	exit (-1);
    }
    if (ret > 0)	/* help requested */
	exit (1);

    /* SET DEFAULTS */
    if (!ToFrom)
	ToFrom = TO;
    if (!Got_old || !Got_new)
    {
	my_help (argv[0], keys);
	exit (-1);
    }

    if (G_legal_filename (Newfile) < 0)
    {
	sprintf (tmpbuf, "Cellfile '%s' is not a legal file name", Newfile);
	G_fatal_error (tmpbuf);
    }
    if ((old_mapset = G_find_cell2 (Oldfile, "")) == NULL)
    {
	sprintf (tmpbuf, "Cellfile '%s' does not exist", Oldfile);
	G_fatal_error (tmpbuf);
    }

    oldcell = G_open_cell_old(Oldfile, old_mapset) ;
    if (oldcell < 0)
	exit(-1);
    if(G_read_cats (Oldfile, old_mapset, &cats) < 0)
	exit(-1);

    table_len = cats.num+1;
    G_free_cats (&cats);
    table = (int *) G_calloc (table_len, sizeof (int));


    newcell = G_open_cell_new(Newfile) ;
    if (newcell < 0)
	exit(-1);

/*
 * 3) Request category numbers from user which define cells
 *    from which distances shall be calculated.
 */
    dist_cnt = Get_input ();

/*
 * 4) While reading in oldfile, convert to binary (1/0) and
 *    write out to new file.  (Cells containing one of the
 *    categories of interest get a one (1); all others assigned
 *    zero (0).  Keep track of number of border cells. (Border
 *    cells are those which are assigned one (1) and do not
 *    have either a data base boundary, or a one (1) cell on
 *    all four sides; above, below, left, and right.)
 */

    makenewcell(oldcell, newcell, table, table_len, &search_cell, &all_cell) ;
    free (table);

/*
 *  5) Close new file (now containing zeros, ones, and twos ). 
 */

    G_close_cell(oldcell)  ;
    G_close_cell(newcell)  ;

/*
 *  6) Request distance categories from user.
 */

    search_area = 0;  /* this needs to be calculated yet */

/*
 *  7) Put category information in the cats file.  This will be
 *     used by the analysis packages.
 */

    writecats(dist, Newfile) ;

/*
 * 8) Determine which dist-from method is likely quickest and
 *    most efficient.
 */

#ifdef DEBUG
fprintf(stderr,"all_cell    : %d\n", all_cell) ;
fprintf(stderr,"search_cell : %d\n", search_cell) ;
fprintf(stderr,"search_area : %d\n", search_area) ;
#endif

/*
    from_cell_time = search_area * search_cell * .000018855 + 5.0 ;
    to_cell_time = (chead.rows + chead.cols) * 28.2 / sqrt((double)all_cell) ;

system("clear") ;
printf("Estimated execution times:\n") ;
printf("TOCELL:    %10.2f secs.\n", to_cell_time)   ;
printf("FROMCELL:  %10.2f secs.\n", from_cell_time) ;
*/

	switch (ToFrom)
	{
	case 1:
		fprintf(stderr,"\n\n Distances calculated using TOCELL\n") ;
		sprintf(buffer, "%s/etc/tocell", G_gisbase() ) ;
		execl(buffer, "tocell", Newfile, "nomail", 0) ;
		G_fatal_error("Can't execute tocell") ;
		break ;
	case 2:
		fprintf(stderr,"\n\n Distances calculated using FROMCELL\n") ;
		sprintf(buffer, "%s/etc/fromcell", G_gisbase() ) ;
		execl(buffer, "fromcell", Newfile, "nomail", 0) ;
		G_fatal_error("Can't execute fromcell") ;
		break ;
	
	default:
		G_fatal_error ("Bad ToFrom switch\n");
		break ;
	}
}

my_help (prog, keys)
    char *prog, *keys[];
{
    G_parse_command_usage (prog, keys);
}

stash_keys (position, key)
    char *key;
    int position;
{
    switch (position) {
	case 1:
	    Got_old = 1;
	    Oldfile = G_store (key);
	    break;
	case 2:
	    Got_new = 1;
	    Newfile = G_store (key);
	    break;
	case 3:
	    G_tolcase (key);
	    if (strcmp (key, "to") == 0)
		ToFrom = TO;
	    else if (strcmp (key, "from") == 0)
		ToFrom = FROM;
	    else
	    {
		fprintf (stderr, "Bad direction option '%s'\n", key);
		return (-1);
	    }
	    return (0);
	    break;
	default:
	    return (-1);
	    break;
    }
    return (0);
}

/* %W% %G% */

/*
 * distance is the user interface to two (or more) 
 *    proximity analysis routines.
 *
 * PROGRAMS:
 *             -> tocell --> cluster
 *           /                       \
 * distance -                         --> MAP
 *           \                       /
 *             -> fromcell ---------
 *
 * PROGRAM FUNCTIONS:
 * distance-  distance provides the user interface to the distance-from
 *            package.  This front end program does the following:
 *            1) Determine which cell map contains the categories from
 *               which distances shall be generated.
 *               a) ensure that the file exists and open.
 *            2) Determine name for new cell file
 *               a) ensure the file is new and open.
 *            3) Request category numbers from user which define cells
 *               from which distances shall be calculated.
 *            4) While reading in oldfile, convert to binary (1/0) and
 *               write out to new file.  (Cells containing one of the
 *               categories of interest get a one (1); all others assigned
 *               zero (0).  Keep track of number of border cells. (Border
 *               cells are those which are assigned one (1) and do not
 *               have either a data base boundary, or a one (1) cell on
 *               all four sides; above, below, left, and right.)
 *            5) Close new file (now containing ones and zeros). Start
 *               dis-frm process in background (one of two branches above).
 *            6) Request distance categories from user.
 *            7) Record these distances in cats file.
 *            8) Determine which dist-from method is likely quickest and
 *               most efficient.
 *            9) Say goodbye.  With luck, newmap will be available soon.
 */


#include	<stdio.h>
#include	<math.h>
#include	"gis.h"
#include	"disfrm.h"
#include	<signal.h>

main (argc,argv) char *argv[];
{
    int oldcell, newcell ;
    int *table ;
    int dist[MAXDIST];
    int table_len;
    int search_area ;
    int search_cell ;
    int all_cell ;
    int ans ;
    double from_cell_time, to_cell_time ;
    double sqrt() ;
    char new_name[30] ;
    char old_name[30] ;
    char *old_mapset ;
    char buffer[256] ;
    struct Cell_head window ;
    struct Categories cats;

    setbuf(stderr,NULL) ;

    fprintf(stderr, "\n\nDISTANCE-FROM ANALYSIS PACKAGE\n") ;

    G_gisinit(argv[0]) ;

/*  Make sure that the window has square grid cells */

    G_get_window(&window) ;
    if (window.ns_res != window.ew_res)
	G_fatal_error("DISTANCE requires that the current window have square gridcells") ;

/*
 *  1) Determine which cell map contains the categories from
 *     which distances shall be generated.
 *     a) ensure that the file exists and open.
 *     b) get category labels
 *     c) allocate category choice table
 */

    old_mapset = G_ask_cell_old(
	    "Enter name of existing data layer which contains the categories",
	    old_name) ;
    if (!old_mapset) exit(0);

    oldcell = G_open_cell_old(old_name, old_mapset) ;
    if (oldcell < 0)
	exit(-1);
    if(G_read_cats (old_name, old_mapset, &cats) < 0)
	exit(-1);

    table_len = cats.num+1;
    table = (int *) G_calloc (table_len, sizeof (int) );

/*
 *  2) Determine name for new cell file (and open it)
 */
    if(!G_ask_cell_new(
	"Enter name for new file to contain the distance-from results", new_name))
	    exit(0);

    newcell = G_open_cell_new(new_name) ;
    if (newcell < 0)
	exit(-1);


/*
 * 3) Request category numbers from user which define cells
 *    from which distances shall be calculated.
 */
    if (maketable(&cats, table) == -1)
    {
	printf("\nNo categories chosen from which to calculate distances\n") ;
	printf("Goodbye\n") ;
	sleep(2) ;
	exit(-1) ;
    }
    G_free_cats (&cats);
 
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

    search_area = getdists(window.ns_res, dist) ;

/*
 *  7) Put category information in the cats file.  This will be
 *     used by the analysis packages.
 */

    writecats(dist, new_name) ;

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

    for(;;)
    {
	printf("\nWhich method would you like to use?\n") ;
	printf("1:  to_cell\n") ;
	printf("2:  from_cell\n") ;
	printf("3:  read some help\n") ;
	printf("> ") ;
	if (!G_gets(buffer)) continue;
	if (sscanf (buffer, "%d", &ans) != 1) continue;
	switch (ans)
	{
	case 1:
		fprintf(stderr,"\n\n Distances calculated using TOCELL\n") ;
		fprintf(stderr,"You will be notified by mail when process is complete\n\n") ;
		if (G_fork())    /* Creates a child as a background process */
			exit(0) ;
		sprintf(buffer, "%s/etc/tocell", G_gisbase() ) ;
		execl(buffer, "tocell", new_name, 0) ;
		G_fatal_error("Can't execute tocell") ;
		break ;

	case 2:
		fprintf(stderr,"\n\n Distances calculated using FROMCELL\n") ;
		fprintf(stderr,"You will be notified by mail when process is complete\n\n") ;
		if (G_fork()) 
			exit(0) ;
		sprintf(buffer, "%s/etc/fromcell", G_gisbase() ) ;
		execl(buffer, "fromcell", new_name, 0) ;
		G_fatal_error("Can't execute fromcell") ;
		break ;
	
	case 3:
		G_gishelp("DISTANCE","DIFFS") ;
		fprintf(stderr,"Hit RETURN to continue > ") ;
		G_gets(buffer) ;
		break ;

	default:
		break ;
	}
    }
}

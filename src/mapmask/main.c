/* %W% %G% */
#include "mapmask.h"

main(argc, argv) char *argv[];
{
    struct Categories cats;
    struct Colors colr ;
    char line[400];
    char name[50];
    char *mapset;

    int num_pts;
    int flag;
    int areas;
    int errs;
    int incr;


    G_gisinit (argv[0]);
    G_get_window (&window);

/* Window must be square */
    if (window.ns_res != window.ew_res)
	G_fatal_error ("Window resolutions are not the same. Run window and set them equal");

    tmpname1 = G_tempfile ();
    tmpname2 = G_tempfile ();

    setbuf(stderr,NULL);

    Ux = (double *) G_calloc (50,sizeof(double));
    Uy = (double *) G_calloc (50,sizeof(double));

    give_intro();

/* ask for data layer */
    mapset = G_ask_cell_new("Enter name of New Data Layer",name);
    if(!mapset)
	exit(0);
    fprintf (stderr, "creating empty cell file ...");

/* since we will be create a 0/1 layer, can tell open that the format
 * is 0 (ie, 1 byte per cell)
 */
    G_set_cell_format(0);
    if((cellfd = G_open_cell_new_random(name)) == -1)
    {
	sprintf (line,"can't create cell file [%s in %s]", name, mapset);
	G_fatal_error (line);
	    exit(1);
    }
    fprintf (stderr,"\n");

/* allocate space for buffers   */
    cellbuf  = G_allocate_cell_buf () ;

/*******************************************************/

/* multiple areas of interest?  */
    fprintf(stderr,"\n\nHow many discrete areas of interest do you ");
    fprintf(stderr,"wish to identify?\n> ");
    sscanf(gets(line),"%d",&areas);

    method_text();

    for(incr = 0; incr < areas; incr++)
    {
	errs = -1;
	do
	{
	    front_end(&num_pts,incr);
	    errs++;
	}
	while((flag = xy_verify(num_pts,errs)) == -1);

/* -2 signifies a circle */
	if(flag != -2)
	{
	    Ax	= (double *)G_calloc(num_pts+1,sizeof(double));
	    Ay	= (double *)G_calloc(num_pts+1,sizeof(double));
	    gripsfill(num_pts);
	    free(Ax);
	    free(Ay);
	}
    }		/* end of multiple areas	*/

    fprintf(stderr,"\n\nCreating Support Files\n");
    G_close_cell (cellfd);

    if (G_read_cats (name, mapset, &cats) >= 0)
    {
	G_set_cat ((CELL) 1, "area of interest", &cats) ;
	G_write_cats (name, &cats);
    }
    G_init_colors (&colr);
    G_set_color ((CELL) 0, 204, 204, 204, &colr);
    G_set_color ((CELL) 1,   0, 255,   0, &colr);
    G_write_colors (name, mapset, &colr);

    fprintf (stderr, "\n\nCompressing [%s]\n", name);
    sprintf (line, "compress %s > /dev/null", name);
    system (line);
}

/*  DLG_TO_DIG     convert BDLG format files into digit files for editing.
**     This process will remove labels from the data.   The dlg_label
**	program   can be used to save labels and restore them after editting.
*/

/* if area exists and has attributes, then consider its lines as area lines
**  Have to deal with Area 1.  Digit data should not normally  have an A 1 type 
**  boundry.  We add it in dig_to_dlg as a bounding box that has 1 line and does
**  NOT intersect with any lines in data.   A 1 should also have attributes 
**  000 0000.  This means that A2 on one of our processed dlg files will 
**  have an A2 that is == -A1.  On dlg files that we get in from the out-
**  side world, anything goes.  It is possible that A1 could be made up  
**  of lines from the data, and I dont see a way to guarantee that we are
**  not throwing away valid data if we remove A1.
**
**  Algorithm: 
**	If A1 has 1 and only 1 line  AND A1 has no attrs or attrs 000 0000
**         AND  A2 == -A1,   then remove the line in A1.
**
**  Result:  
**	On files we have created w/ digit and dig_to_dlg, This will remove
**      A1 and A2 when going back to dig. These will be replace again in
**	dig_to_dlg.
**
**	On files that come in from outside, if they are similar to ours, then
**	A1 and A2 will be removed, otherwise nothing will be removed and
** 	dig_to_dlg will add another box around the data.  The editor in digit
**	is of course free to remove any box that is not real data.
*/
/*
**  Written by Dave Gerdes and Mike Higgins 4/1988
**  US Army Construction Engineering Research Lab
*/
#include "Vect.h"
#include "gis.h"
#include "dlg.h"

#include <stdio.h>


int Force_lines = 0;

#define BOUND 8
/*  POINT is also known as DOT */

#define ATT_AREA 'A'
#define ATT_LINE 'L'
#define ATT_POINT 'P'

main(argc, argv)
	int argc ;
	char **argv ;
{
	/*FILE *fopen(), *f_digit, *f_dlg, *f_att ; */
	FILE *fopen(),  *f_dlg, *f_att ;
	register int i, n;
	int step ;
	int degenerate ;
	int last_n ;
	int status ;
	int one_line ;		/* which line  is used by A1 */

	char type ;
	char *linetypes ;
	char message[128] ;
	char errmsg[200];

	double *xarray ;
	double *yarray ;
	struct dlg dlg;
	struct dig_head head;
	/*new, replaces *f_digit--and more!*/
	struct Map_info Map;
	struct line_pnts *Points;

	setbuf(stderr, 0) ;
	step = 0 ;

	/*
	if (argc != 4 && argc != 5)
	*/
	if (argc < 4)
	{
		fprintf (stdout,"Usage: dlgtodig dlg_file digit_file att_file [ \"line\" ]\n") ;
		exit(-1) ;
	}

	G_gisinit(argv[0]) ;

/* open necessary files */
	fprintf (stderr, "STEP %d: Open all necessary files\n", ++step) ;

	if (! (f_dlg = fopen(argv[1], "r")) )
	{
		fprintf (stderr, "  PROBLEM: Can't open dlg file: %s", argv[1]) ;
		G_fatal_error (message) ;
	}

/*superceded by Vect_open_new()
	if (! (f_digit = fopen(argv[2], "w")) )
	{
		fprintf (stderr, "  PROBLEM: Can't open digit for writing: %s", argv[2]) ;
		G_fatal_error (message) ;
	}
*/

    if (0 > Vect_open_new (&Map, argv[2]))
    {
        sprintf(errmsg, "Not able to open vector file <%s>\n", argv[2]) ;
        G_fatal_error (errmsg);
    }


	if (! (f_att = fopen(argv[3], "w")) )
	{
		fprintf (stderr, "  PROBLEM: Can't open attribute for writing: %s", argv[3]) ;
		G_fatal_error (message) ;
	}


   /*  check for answer to the unlabeled area edges converted to Line question
   */
	if (argc == 5)
	    if (!strncmp (argv[4], "line", 4))
		Force_lines = 1;
	    else
		fprintf (stderr, "Bad argument '%s'.   Ignoring.\n", argv[4]);

/* Read dlg header information */
	fprintf (stderr,"STEP %d: Read dlg header\n", ++step) ;


    if (dlg_init (f_dlg, &dlg) != 0)
	G_fatal_error ("dlg_init failed");

    if (dlg_read (f_dlg, &dlg) != 0)
	G_fatal_error ("dlg_read failed");

    Points = Vect_new_line_struct();

    hd_dlg_to_dig (&dlg, &head);

/*  create and intialize array of line numbers of area edges  */
    linetypes = (char *) G_calloc (dlg.max_lines+1, sizeof (char));
    for (n = 1 ; n <= dlg.max_lines ; n++)
	linetypes[n] = LINE;

	fprintf (stderr,"STEP %d: Analyze\n", ++step) ;

#ifndef abs
#define abs(x) ((x) >= 0 ? (x) : -(x))
#endif

    one_line = 0;
    last_n = 0;
    for (n = 1 ; n <= dlg.max_areas ; n++)
    {

	if ( (status = dlg_read_area (f_dlg, &dlg, n)) < 0)
	{
	    sprintf( message, " dlg_to_dig: DLG_READ_AREA FAILED,  area before this: %d\n", last_n) ;
	    G_fatal_error (message);
	}

	if ( status == 1)
		continue ;

	last_n = n ;

	if (dlg.area.n_atts || !Force_lines)
	{

		/*  Area 1, is it one line (universe box)  */
		if ( n == 1 && dlg.area.n_lines == 1)
		    if (dlg.area.n_atts == 0 || (dlg.area.n_atts == 1 &&
			dlg.area.atts[0] == 0 && dlg.area.atts[1] == 0))
			    one_line = dlg.area.lines[0] ;

		/*  lines making up area set to Area types.

		**  check only lines making up area and do not check
		**  island lines  added 7/88, The Digits
		*/
		for (i = 0 ; i < dlg.area.n_lines ; i++)
/**/		    if (dlg.area.lines[i] == 0)
/**/			break;
/**/		    else
		    	linetypes[abs(dlg.area.lines[i])] = AREA;

		/*  one line makes up a useless bounding box  */
		if ( n == 2 && one_line  &&  dlg.area.lines[0] == -one_line)
		    linetypes[abs(one_line)] = BOUND;

		/*  if there is an Area label save it  */
        	if (dlg.area.n_atts > 0  &&  dlg.area.atts[1] > 0)
	    		write_att (f_att, FILE_AREA, dlg.area.x, dlg.area.y,
			dlg.area.atts[1]) ;
	}

    }	/*  for()  */

	fprintf (stderr,"STEP %d: Write Vect.header\n", ++step) ;
	/*obsolete with new Vect.lib
     write_head_bdig (f_digit, &head);
     */
     /*replaces the above*/
     Vect_copy_head_data (&head, &Map.head);


    /*  now we have line types (A or L) , just get each Line and dump it in
    *   digit format and write the attribute 
    */
	fprintf (stderr,"STEP %d: Write digit Lines and attributes\n", ++step) ;
	degenerate = 0 ;

	last_n = 0 ;
	for (n = 1 ; n <= dlg.max_lines ; n++)
	{

		if ( (status = dlg_read_line (f_dlg, &dlg, n)) < 0)
		{
	    	sprintf( message, " dlg_to_dig: DLG_READ_LINE FAILED,  line before this: %d\n", last_n) ;
	    	G_fatal_error (message);
		}

		if ( status == 1)
			continue ;

		last_n = n ;
	

		if (dlg.line.n_coors < 2 || dlg.line_off[n] == 0L)
			continue ;

	   	breakout_xy ( dlg.line.coors, dlg.line.n_coors,  &xarray, &yarray) ;

		type = linetypes[n] ;
		if (type == BOUND)
		{
			continue ;
		}

		/**  if degenerate line,  toss **/
		if (dlg.line.n_coors == 2
			&&  xarray[0]  ==  xarray[1]
			&&  yarray[0]  ==  yarray[1])
		{
			++degenerate ;
			continue ;
		}


   		dig_write_line (&Map, Points, xarray, yarray,
			dlg.line.n_coors, type == AREA ? AREA : LINE);

		/*  valid non-zero attribute  */
    	   	if (dlg.line.n_atts > 0  &&  dlg.line.atts[1] > 0)
   			write_att_line (f_att, xarray, yarray, dlg.line.n_coors,
				dlg.line.atts[1]) ;
	}


	if (degenerate)
		fprintf (stderr,"    degenerate lines %d\n", degenerate) ;

	fprintf (stderr,"STEP %d: Write digit Points and attributes\n", ++step) ;

	for (n = 1 ; n <= dlg.max_nodes ; n++)
	{
		/*if (dlg.node_off[n] != 0L)*/
		if (dlg.node_off[n] == 0L)
			continue ;

   		dlg_read_node (f_dlg, &dlg, n);


		/*dpg*/
		/* if have a Site, then write a degen line */
		if (dlg.node.n_lines == 0 || dlg.node.n_atts > 0)
		    dig_write_point (&Map, Points, &(dlg.node.x),
			    &(dlg.node.y), DOT) ;

		/*  have a valid attribute??  */

		/*  Change by D. Satnik to allow negative atts
		if (dlg.node.n_atts <= 0  ||  dlg.node.atts[1] <= 0)
			continue ;
		*/
		if (dlg.node.n_atts <= 0  ||  dlg.node.atts[1] != 0)
			continue ;

		/*  save Node label as a dig Point  */
    		write_att (f_att, FILE_DOT, dlg.node.x,
			dlg.node.y, dlg.node.atts[1]) ;

	}


	Vect_destroy_line_struct (Points);
    Vect_close (&Map);
	fclose(f_dlg) ;
	fclose(f_att) ;

	exit(0) ;
}

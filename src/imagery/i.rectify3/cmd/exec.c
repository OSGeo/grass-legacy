/*======================================================================
                             i.rectify.c

  exec.c --

         Loop through all files to be rectified and do the retifiecation.
	 Handles things like support files.

  NOTE:  This version no longer runs in the backgroun or uses the
         mail to notifiy the user when completed.

======================================================================*/


#include <time.h>
#include "global.h"
#include "protodefs.h"

int 
exec_rectify (struct Ref ref)
{
    char *name;
    char *mapset;
    char *result;
    char *type;
    int i,n;
    struct Colors colr;
    struct Categories cats;
    struct History hist;
    int colr_ok, hist_ok, cats_ok;
    long start_time, rectify_time, compress_time;
    char *mailfile = NULL;
    char msg[100];      /* message buffer */


/* allocate the output cell matrix */
    cell_buf = (CELL **) G_calloc (NROWS, sizeof (CELL *));
    n = NCOLS * sizeof (CELL);
    for (i=0; i < NROWS; i++)
    {
	cell_buf[i] = (CELL *) G_malloc (n);
    }


/* go into background */
/**************  DO NOT FORK OR MAIL ANYTHING 
/**    fprintf (stderr, "\nYou will receive mail when %s is complete\n",
/**	G_program_name());
/**    if (G_fork()) exit(0);
*********************************************/

/* note: all calls to G_tempfile() should happen after the fork */

/* create a mailfile */
/*******************************************
/**    mailfile = G_tempfile();
/**    unlink (mailfile);
/**    close(creat(mailfile,0666));
*******************************************/

/* open stderr to /dev/null so all GRASS error messages will be
 * mailed to the user
 */
/******************************************
/**    freopen ("/dev/null","w",stderr);
/**    freopen ("/dev/null","w",stdout);
******************************************/

/* rectify each file */
    for (n = 0; n < ref.nfiles; n++)
    {
	if ((i = ref_list[n]) < 0)
	    continue;
	name   = ref.file[i].name;
	mapset = ref.file[i].mapset;
	result = new_name[n];

	select_current_env();

	G_suppress_warnings(1);
	cats_ok = G_read_cats (name, mapset, &cats) >= 0;
	colr_ok = G_read_colors (name, mapset, &colr) > 0;
/*
	hist_ok = G_read_history (name, mapset, &hist) >= 0;
*/
	/* Initialze History */
        type = "raster";
        G_short_history(name, type, &hist);

	G_suppress_warnings(0);

	time (&start_time);

	/* tell user about progress */
	/** TODO verbose flag **/
	sprintf (msg, "Starting to rectify <%s> into <%s> \n",
		name, mapset);
	fprintf  (stderr, msg);

	if (rectify (name, mapset, result))
	{
	    select_target_env();
	    if(cats_ok)
	    {
		G_write_cats (result, &cats);
		G_free_cats (&cats);
	    }
	    if(colr_ok)
	    {
		G_write_colors (result, G_mapset(), &colr) ;
		G_free_colors (&colr);
	    }

                /* Write out History Structure History */
                sprintf(hist.title, "%s", result);
                sprintf(hist.datsrc_1, "%s", name);
                sprintf(hist.edhist[0], "Created from: i.rectify3");
                hist.edlinecnt = 1;
                G_write_history (result, &hist) ;

	    select_current_env();
	    time (&rectify_time);
	    if (compress(result))
		time (&compress_time);
	    else
		compress_time = rectify_time;
	    report (mailfile, name, mapset, result, rectify_time-start_time, compress_time-rectify_time, 1);
	}
	else
	    report (mailfile, name, mapset, result, (long)0, (long)0, 0);
    }
/******************
/**    mail (mailfile);
/**    unlink (mailfile);
/**    G_done_msg ("Check your mail");
******************/

    G_done_msg ("All files have been rectified!");

    return 0;
}

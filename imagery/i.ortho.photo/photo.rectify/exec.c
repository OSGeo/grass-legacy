/*
  exec.c --
  
           Loop through all files to be rectified and do the retification.
                    Handles things like support files.
*/                   

#include <unistd.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "global.h"

int exec_rectify (void)
{
    char *name;
    char *mapset;
    char *result;
    int i,n;
    struct Colors colr;
    struct Categories cats;
    struct History hist;
    int colr_ok, hist_ok, cats_ok;
    long start_time, rectify_time, compress_time;
    char *mailfile;

    /* allocate the output cell matrix */
    cell_buf = (CELL **) G_calloc (NROWS, sizeof (void *));
    n = NCOLS * G_raster_size(map_type);
    for (i=0; i < NROWS; i++)
    {
	cell_buf[i] = (void *) G_malloc (n);
	G_set_null_value(cell_buf[i], NCOLS, map_type);
    }

    /* go into background */
    fprintf (stderr, "\nYou will receive mail when %s is complete\n",
	G_program_name());
    sleep(3);

    if (G_fork())  exit(0);

    /* note: all calls to G_tempfile() should happen after the fork */

    /* create a mailfile */
    mailfile = G_tempfile();
    unlink (mailfile);
    close(creat(mailfile,0666));

/* open stderr to /dev/null so all GRASS error messages will be
 * mailed to the user
 */

    freopen ("/dev/null","w",stderr);
    freopen ("/dev/null","w",stdout);

    /* rectify each file */
    for (n = 0; n < group.group_ref.nfiles; n++)
    {
#ifdef DEBUG3
        fprintf (Bugsr,"I look for files to ortho rectify \n");
#endif
	if ((i = ref_list[n]) < 0)
	    continue;
	name   = group.group_ref.file[i].name;
	mapset = group.group_ref.file[i].mapset;
	result = new_name[n];

#ifdef DEBUG3
        fprintf (Bugsr,"ORTHO RECTIFYING: \n");
        fprintf (Bugsr,"NAME %s \n", name);
        fprintf (Bugsr,"MAPSET %s \n", mapset);
        fprintf (Bugsr,"RESULT %s \n", result);
#endif

#ifdef DEBUG3
        fprintf (Bugsr,"select_current_env...\n");
#endif        
	select_current_env();

	G_suppress_warnings(1);
	cats_ok = G_read_cats (name, mapset, &cats) >= 0;
	colr_ok = G_read_colors (name, mapset, &colr) > 0;
	hist_ok = G_read_history (name, mapset, &hist) >= 0;
#ifdef DEBUG3
        fprintf (Bugsr,"reading was fine...\n");
#endif
	G_suppress_warnings(0);

	time (&start_time);
#ifdef DEBUG3
        fprintf (Bugsr,"Starting the rectification...\n");
#endif
	if (rectify (name, mapset, result))
	{
#ifdef DEBUG3
        fprintf (Bugsr,"Done. Writing results...\n");
#endif
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
	    if(hist_ok)
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
	{
#ifdef DEBUG3
        fprintf (Bugsr,"Could not rectify. Mhhh.\n");
#endif

	    report (mailfile, name, mapset, result, (long)0, (long)0, 0);
	}
    }
    mail (mailfile);
    unlink (mailfile);

    return 0;
}

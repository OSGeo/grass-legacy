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

int exec_rectify(void)
{
    char *name;
    char *mapset;
    char *result;
    int i, n;
    struct Colors colr;
    struct Categories cats;
    struct History hist;
    int colr_ok, hist_ok, cats_ok;
    long start_time, rectify_time, compress_time;


    /* allocate the output cell matrix */
    cell_buf = (CELL **) G_calloc(NROWS, sizeof(void *));
    n = NCOLS * G_raster_size(map_type);
    for (i = 0; i < NROWS; i++) {
	cell_buf[i] = (void *)G_malloc(n);
	G_set_null_value(cell_buf[i], NCOLS, map_type);
    }

    /* rectify each file */
    for (n = 0; n < group.group_ref.nfiles; n++) {
	G_debug(2, "I look for files to ortho rectify");

	if ((i = ref_list[n]) < 0)
	    continue;
	name = group.group_ref.file[i].name;
	mapset = group.group_ref.file[i].mapset;
	result = new_name[n];

	G_debug(2, "ORTHO RECTIFYING:");
	G_debug(2, "NAME %s", name);
	G_debug(2, "MAPSET %s", mapset);
	G_debug(2, "RESULT %s", result);
	G_debug(2, "select_current_env...");

	select_current_env();

	cats_ok = G_read_cats(name, mapset, &cats) >= 0;
	colr_ok = G_read_colors(name, mapset, &colr) > 0;
	hist_ok = G_read_history(name, mapset, &hist) >= 0;
	G_debug(2, "reading was fine...");

	time(&start_time);

	G_debug(2, "Starting the rectification...");

	if (rectify(name, mapset, result)) {
	    G_debug(2, "Done. Writing results...");
	    select_target_env();
	    if (cats_ok) {
		G_write_cats(result, &cats);
		G_free_cats(&cats);
	    }
	    if (colr_ok) {
		G_write_colors(result, G_mapset(), &colr);
		G_free_colors(&colr);
	    }
	    if (hist_ok)
		G_write_history(result, &hist);
	    select_current_env();
	    time(&rectify_time);
	    if (compress(result))
		time(&compress_time);
	    else
		compress_time = rectify_time;
	    report(name, mapset, result, rectify_time - start_time,
		   compress_time - rectify_time, 1);
	}
	else {
	    G_debug(2, "Could not rectify. Mhhh.");
	    report(name, mapset, result, (long)0, (long)0, 0);
	}
    }

    G_done_msg(" ");
    return 0;
}

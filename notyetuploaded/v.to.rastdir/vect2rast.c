#include "digit.h"
#include "gis.h"
#include "format.h"

vect_to_rast (vector_map, raster_map, nrows, use_angles)
    char *vector_map, *raster_map;
    int use_angles;
{
    char *vector_mapset;
    char msg[1024];
    struct Map_info Map;
    struct line_pnts *Points;
    int fd;	/* for raster map */
    int nareas, nlines, nsites;
    int stat;
    int level;
    int format;
    int pass, npasses;
    long timer;

    vector_mapset = G_find_vector2 (vector_map, "");
    if (vector_mapset == NULL)
    {
	sprintf (msg, "Vector map <%s> not found", vector_map);
	G_fatal_error (msg);
    }

    start_clock (&timer);

    start_clock (NULL);
    inform ("Loading vector information ...");
    level = Vect_open_old (&Map, vector_map, vector_mapset);
    if (level < 2)
    {
	sprintf (msg, "You must run v.support on <%s> before running %s\n",
	    vector_map, G_program_name());
	G_fatal_error (msg);
    }
    Points = Vect_new_line_struct();
    inform(NULL);
    stop_clock(NULL);


    start_clock(NULL);
    inform ("Sorting areas by size ...");
    if((nareas = sort_areas (&Map, Points)) < 0)
    {
	sprintf (msg, "ERROR processing areas from vector map <%s>\n",
		vector_map);
	G_fatal_error (msg);
    }
    sprintf (msg, " %d areas", nareas);
    inform (msg);
    inform (NULL);
    stop_clock(NULL);

    fd = G_open_cell_new (raster_map);
    if (fd < 0)
    {
	sprintf (msg, "Can't create raster map <%s>", raster_map);
	G_fatal_error (msg);
	exit(1);
    }


    nsites = 1;
    nlines = 1;
    format = getformat(&Map);
    npasses = begin_rasterization(nrows, format);
    pass = 0;
    do
    {
	pass++;
	if (npasses > 1) printf ("Pass #%d (of %d)\n", pass, npasses);
	stat = 0;

	if (nareas)
	{
	    start_clock(NULL);
	    if (npasses > 1)
		inform ("  ");
	    inform ("Processing areas ...");

	    if(do_areas (&Map, Points) < 0)
	    {
		fprintf (stderr, "\nERROR processing areas from vector map <%s>\n", vector_map);
		stat = -1;
		break;
	    }
	    sprintf (msg, " %d areas", nareas);
	    inform (msg);
	    inform (NULL);
	    stop_clock(NULL);
	}

	if (nlines)
	{
	    start_clock(NULL);
	    if (npasses > 1)
		inform ("  ");
	    inform ("Processing lines ...");

	    if((nlines = do_lines (&Map, Points, use_angles)) < 0)
	    {
		fprintf (stderr, "\nERROR processing lines from vector map <%s>\n", vector_map);
		stat = -1;
		break;
	    }
	    sprintf (msg, " %d lines", nlines);
	    inform (msg);
	    inform (NULL);
	    stop_clock (NULL);
	}

	if (nsites)
	{
	    start_clock(NULL);
	    if(npasses > 1)
		inform ("  ");
	    inform ("Processing sites ...");

	    if((nsites = do_sites (&Map, Points)) < 0)
	    {
		fprintf (stderr, "\nERROR processing sites from vector map <%s>\n", vector_map);
		stat = -1;
		break;
	    }
	    sprintf (msg, " %d sites", nsites);
	    inform (msg);
	    inform (NULL);
	    stop_clock (NULL);
	}

	start_clock (NULL);
	if(npasses > 1)
	    inform ("  ");
	inform ("Writing raster map ...");

	stat = output_raster(fd);
	inform (NULL);
	stop_clock (NULL);
    } while (stat == 0);
    /* stat: 0 means repeat
     *       1 means done
     *      -1 means error
     */

    Vect_destroy_line_struct (Points);

    if (stat < 0)
    {
	G_unopen_cell(fd);
	return 1;
    }

    start_clock(NULL);
    inform ("Creating support files for raster map ...");
    G_close_cell(fd);
    update_hist(raster_map, vector_map, vector_mapset, Map.head.orig_scale);
    update_colors (raster_map);
    update_cats(raster_map, vector_map, vector_mapset);
    inform(NULL);
    stop_clock(NULL);

    printf ("\n");
    sprintf (msg, "Raster map <%s> done.\nTotal processing time:", raster_map);
    inform(msg);
    inform(NULL);
    stop_clock (&timer);
    return 0;
}

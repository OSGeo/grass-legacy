#include <unistd.h>
#include <stdlib.h>
#include "gis.h"
#include "report.h"
#include "local_proto.h"

#define LAYER_NUMBER    layers->field[1]
#define LAYER_NAME      layers->field[2]
#define LAYER_FULLNAME  layers->field[3]

int process (FILE *out, REPORT *layers, REPORT *ref)
{
    long npoints;
    int layer_num;
    CELL cat;
    long ncats;
    long window_total;
    long window_total_no_0;
    long pcount, wcount;
    struct Cell_stats window_stats;
    struct Cell_stats point_stats;
    float total_chi_square;
    float chi_square;
    float expected;
    float coverage;
    float diff;
    long  total_cells;
    long  total_sites;
    float total_coverage;
    float total_expected;
    int first;


    layer_num = atol (LAYER_NUMBER);

    fprintf (out, ".blocktitle\n");
    fprintf (out, "Layer: %-20s %s\n\n", LAYER_NAME, LAYER_FULLNAME);

    G_init_cell_stats (&window_stats);
    G_init_cell_stats (&point_stats);

    window_total = 0;
    window_total_no_0 = 0;

    report_seek_cats (ref);
    while (report_read_record(ref,"cat"))
    {
	if (layer_num != atol(ref->field[1]))
		continue;
	cat = atol (ref->field[2]);
	wcount = atol (ref->field[3]);
	window_total += wcount;
	if (cat)
	    window_total_no_0 += wcount;
	while (wcount-- > 0)
	    G_update_cell_stats (&cat, 1, &window_stats);
    }

    npoints = 0;
    total_expected = 0;

    report_seek_data (ref);
    while (report_read_record(ref,"data"))
    {
	if (layer_num != atol(ref->field[1]))
	    continue;

	cat = mode(ref);
	G_update_cell_stats (&cat, 1, &point_stats);
	npoints++;
	if (cat != 0)
	    total_expected++;
    }


    fprintf (out, "cells in analysis region: %10ld\n", window_total);
    fprintf (out, "sites in analysis region: %10ld\n\n", npoints);

    fprintf (out, ".setcol\n 30 7 7 8 6 8 7\n");
    fprintf (out, ".col\n");
    fprintf (out, "\n\n\n\n\n\nCdegrees\n");
    fprintf (out, ".end\n");
    fprintf (out, ".col\n");
    fprintf (out, "\nCcells\nC%%\nCexpected\nCactual\nCchi\nCof\n");
    fprintf (out, ".end\n");
    fprintf (out, ".col\n");
    fprintf (out, "LSite Characteristics\n");
    fprintf (out, "Ccover\nCcover\nCsites\nCsites\nCsquare\nCfreedom\n");
    fprintf (out, ".end\n");
    fprintf (out, ".col\n");
    fprintf (out, "L--------------------\n");
    fprintf (out, "F-\nF-\nF-\nF-\nF-\nF-\n");
    fprintf (out, ".end\n");

    fprintf (out, ".end\n");	/* end block title */

    fprintf (out, ".block\n");

    ncats            = 0;
    total_cells      = 0;
    total_sites      = 0;
    total_coverage   = 0.0;
    total_chi_square = 0.0;

/* from now on exclude category 0 */

    fprintf (out, ".setcol\n 30 7 7 8 6 8 5\n");

    G_rewind_cell_stats (&window_stats);
    first = 1;
    while (G_next_cell_stat (&cat, &wcount, &window_stats))
    {
	if (!report_find_cat (ref, layer_num, cat))
	    continue;

	if (!G_find_cell_stat (cat, &pcount, &point_stats))
		pcount = 0;
	if (cat)
	{
	   coverage = (float) wcount / window_total_no_0;
	   expected = coverage * total_expected;
	   diff = (float) pcount - expected;
	   if (expected != 0.0)
	   {
		chi_square = diff * diff / expected;
		total_chi_square += chi_square;
		/*
		diff = diff * 100.0 / expected;
		*/
	   }

	   total_coverage += coverage;
	   total_cells    += wcount;
	   total_sites    += pcount;
	   ncats++;
	}

	fprintf (out, ".col\n");
	fprintf (out, "L(%3ld) %s\n", (long)cat, ref->field[4]);
	fprintf (out, "R%ld\n", wcount);
	if (cat)
	{
	    fprintf (out, "R%.1f\n", coverage * 100.0);
	    fprintf (out, "R%.1f\n", expected);
	    fprintf (out, "R%ld\n", pcount);
	    if (diff != 0.0)
	    {
		fprintf(out, "R%.3f\n", chi_square);
		fprintf(out, "R1\n");
	    }
	    else
		fprintf(out, "C***\n"); 
	}
	else
	{
	    fprintf (out, "\n");
	    fprintf (out, "\n");
	    fprintf (out, "R%ld\n", pcount);
	    fprintf (out, "\n");
	    fprintf (out, "\n");
	}
	fprintf(out, ".end\n");
	if (first)
	{
	    fprintf(out, "\n");
	    first = 0;
	}
    }
    fprintf (out, ".setcol\n 30 7 7 8 6 8 7\n");
    fprintf (out, ".col\n");
    fprintf (out, "\n");
    fprintf (out, "F-\nF-\nF-\nF-\nF-\nF-\n");
    fprintf (out, ".end\n");
    fprintf (out, ".setcol\n 30 7 7 8 6 8 5\n");
    fprintf (out, ".col\n");
    fprintf (out ,"L      Totals\n");
    fprintf (out, "R%ld\nR%.1f\nR%.1f\nR%ld\n",
	    total_cells,total_coverage*100.0,
	    total_expected, total_sites);

    ncats--;
    fprintf (out, "R%.3f\nR%ld\n", total_chi_square, ncats);
    fprintf (out, ".end\n");
    fprintf (out, ".end\n");	/* end block */

    G_free_cell_stats (&window_stats);
    G_free_cell_stats (&point_stats);

    return 0;
}

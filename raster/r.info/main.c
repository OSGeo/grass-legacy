
/***************************************************************************
*
* MODULE:       r.info
*
* AUTHOR(S):    Michael O'Shea
*
* PURPOSE:      Outputs basic information about a user-specified raster map layer.
*
* COPYRIGHT:    (C) 2005 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include "local_proto.h"
#include <grass/glocale.h>

#define printline(x) fprintf (out," | %-74.74s |\n",x)
#define divider(x) \
    fprintf (out," %c",x);\
    for (i = 0; i < 76; i++)\
        fprintf(out,"-");\
    fprintf (out,"%c\n",x)

/*local prototype */
int format_double(double value, char *buf);
static char *name;

/**************************************************************************/
int main(int argc, char *argv[])
{
    char *mapset;
    char *line = NULL;
    char tmp1[100], tmp2[100], tmp3[100];
    char timebuff[256];
    int i, ret;
    CELL mincat = 0, maxcat = 0, cat;
    double zmin, zmax;		/* min and max data values */
    FILE *out;
    struct Range crange;
    struct FPRange range;
    struct Cell_head cellhd;
    struct Categories cats;
    struct History hist;
    struct TimeStamp ts;
    int time_ok = 0, first_time_ok = 0, second_time_ok = 0;
    int head_ok;
    int cats_ok;
    int hist_ok;
    int is_reclass;
    RASTER_MAP_TYPE data_type;
    struct Reclass reclass;
    struct GModule *module;
    struct Option *opt1;
    struct Flag *rflag;
    struct Flag *sflag;
    struct Flag *tflag;
    struct Flag *timestampflag;
    struct Flag *gflag;
    struct Flag *hflag;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("raster");
    module->description =
	_("Outputs basic information about a user-specified raster map layer.");

    opt1 = G_define_standard_option(G_OPT_R_MAP);

    rflag = G_define_flag();
    rflag->key = 'r';
    rflag->description = _("Print range only");

    sflag = G_define_flag();
    sflag->key = 's';
    sflag->description = _("Print raster map resolution (NS-res, EW-res) only");

    tflag = G_define_flag();
    tflag->key = 't';
    tflag->description = _("Print raster map type only");

    gflag = G_define_flag();
    gflag->key = 'g';
    gflag->description = _("Print raster map region only");

    hflag = G_define_flag();
    hflag->key = 'h';
    hflag->description = _("Print raster history instead of info");

    timestampflag = G_define_flag();
    timestampflag->key = 'p';
    timestampflag->description =
	_("Print raster map timestamp (day.month.year hour:minute:seconds) only");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    name = G_store(opt1->answer);
    if ((mapset = G_find_cell2(name, "")) == NULL)
	G_fatal_error(_("Cannot find %s"), name);

    head_ok = G_get_cellhd(name, mapset, &cellhd) >= 0;
    cats_ok = G_read_cats(name, mapset, &cats) >= 0;
    hist_ok = G_read_history(name, mapset, &hist) >= 0;
    is_reclass = G_get_reclass(name, mapset, &reclass);
    data_type = G_raster_map_type(name, mapset);

    /*Check the Timestamp */
    time_ok = G_read_raster_timestamp(name, mapset, &ts) > 0;
    /*Check for valid entries, show none if no timestamp available */
    if (time_ok) {
	if (ts.count > 0)
	    first_time_ok = 1;
	if (ts.count > 1)
	    second_time_ok = 1;
    }

    if (G_read_fp_range(name, mapset, &range) < 0)
	G_fatal_error(_("could not read range file"));
    G_get_fp_range_min_max(&range, &zmin, &zmax);

    out = stdout;

    if (!rflag->answer && !sflag->answer && !tflag->answer && !gflag->answer &&
	!hflag->answer && !timestampflag->answer) {
	divider('+');

	if (G_asprintf
	    (&line, "Layer:    %-29.29s  Date: %s", name,
	     hist_ok ? hist.mapid : "??") > 0)
	    printline(line);
	else
	    G_fatal_error(_("Cannot allocate memory for string"));

	if (G_asprintf
	    (&line, "Mapset:   %-29.29s  Login of Creator: %s", mapset,
	     hist_ok ? hist.creator : "??") > 0)
	    printline(line);
	else
	    G_fatal_error(_("Cannot allocate memory for string"));

	if (G_asprintf(&line, "Location: %s", G_location()) > 0)
	    printline(line);
	else
	    G_fatal_error(_("Cannot allocate memory for string"));

	if (G_asprintf(&line, "DataBase: %s", G_gisdbase()) > 0)
	    printline(line);
	else
	    G_fatal_error(_("Cannot allocate memory for string"));

	if (G_asprintf
	    (&line, "Title:    %s ( %s )", cats_ok ? cats.title : "??",
	     hist_ok ? hist.title : "??") > 0)
	    printline(line);
	else
	    G_fatal_error(_("Cannot allocate memory for string"));

	/*This shows the TimeStamp */
	if (time_ok && (first_time_ok || second_time_ok)) {

	    G_format_timestamp(&ts, timebuff);

	    /*Create the r.info timestamp string */
	    if (G_asprintf(&line, "Timestamp: %s", timebuff) > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));

	}
	else {
	    if (G_asprintf(&line, "Timestamp: none") > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));
	}

	divider('|');
	printline("");

	if (cats_ok) {
	    format_double((double)cats.num, tmp1);
	}

	if (G_asprintf
	    (&line, "  Type of Map:  %-20.20s Number of Categories: %-9s",
	     hist_ok ? hist.maptype : "??", cats_ok ? tmp1 : "??") > 0)
	    printline(line);
	else
	    G_fatal_error(_("Cannot allocate memory for string"));

	if (G_asprintf(&line, "  Data Type:    %s",
		       (data_type == CELL_TYPE ? "CELL" :
			(data_type == DCELL_TYPE ? "DCELL" :
			 (data_type == FCELL_TYPE ? "FCELL" : "??")))) > 0)
	    printline(line);
	else
	    G_fatal_error(_("Cannot allocate memory for string"));

	if (head_ok) {
	    if (G_asprintf(&line, "  Rows:         %d", cellhd.rows) > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));

	    if (G_asprintf(&line, "  Columns:      %d", cellhd.cols) > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));

	    if (G_asprintf
		(&line, "  Total Cells:  %ld",
		 (long)cellhd.rows * cellhd.cols) > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));

	    if (G_asprintf
		(&line, "       Projection: %s (zone %d)",
		 G_database_projection_name(), G_zone()) > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));

	    G_format_northing(cellhd.north, tmp1, cellhd.proj);
	    G_format_northing(cellhd.south, tmp2, cellhd.proj);
	    G_format_resolution(cellhd.ns_res, tmp3, cellhd.proj);
	    if (G_asprintf
		(&line, "           N: %10s    S: %10s   Res: %5s", tmp1, tmp2,
		 tmp3) > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));

	    G_format_easting(cellhd.east, tmp1, cellhd.proj);
	    G_format_easting(cellhd.west, tmp2, cellhd.proj);
	    G_format_resolution(cellhd.ew_res, tmp3, cellhd.proj);
	    if (G_asprintf(&line, "           E: %10s    W: %10s   Res: %5s",
			   tmp1, tmp2, tmp3) > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));

	    if (data_type == CELL_TYPE) {
		if( 2 == G_read_range(name, mapset, &crange) )
		    ret = G_asprintf(&line, "  Range of data:    min = NULL  max = NULL");
		else
		    ret = G_asprintf(&line, "  Range of data:    min = %i  max = %i",
		      (CELL) zmin, (CELL) zmax);

		if (ret > 0)
		    printline(line);
		else
		    G_fatal_error(_("Cannot allocate memory for string"));
	    }
	    else {
		if (G_asprintf(&line, "  Range of data:    min = %f  max = %f",
			zmin, zmax) > 0)
		    printline(line);
		else
		    G_fatal_error(_("Cannot allocate memory for string"));
	    }
	}

	printline("");

	if (hist_ok) {
	    printline("  Data Source:");
	    if (G_asprintf(&line, "   %s", hist.datsrc_1) > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));

	    if (G_asprintf(&line, "   %s", hist.datsrc_2) > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));

	    printline("");

	    printline("  Data Description:");
	    if (G_asprintf(&line, "   %s", hist.keywrd) > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));

	    printline("");
	    if (hist.edlinecnt) {
		printline("  Comments:  ");

		for (i = 0; i < hist.edlinecnt; i++)

	    /**************************************/
		{
		    if (G_asprintf(&line, "   %s", hist.edhist[i]) > 0)
			printline(line);
		    else
			G_fatal_error(_("Cannot allocate memory for string"));

		}
	    }

	    printline("");
	}

	if (is_reclass > 0) {
	    int first = 1;

	    divider('|');
	    if (G_asprintf(&line, "  Reclassification of [%s] in mapset [%s]",
			   reclass.name, reclass.mapset) > 0)
		printline(line);
	    else
		G_fatal_error(_("Cannot allocate memory for string"));

	    printline("");
	    printline("        Category        Original categories");
	    printline("");

	    for (i = 0; i < reclass.num; i++) {
		CELL x = reclass.table[i];
		if (G_is_c_null_value(&x))
		    continue;
		if (first || x < mincat)
		    mincat = x;
		if (first || x > maxcat)
		    maxcat = x;
		first = 0;
	    }

	    if (!first)
		for (cat = mincat; cat <= maxcat; cat++) {
		    char text[80];
		    char *num;
		    int next;

		    if (cat == 0)
			continue;
		    if (G_asprintf(&num, "%5ld", (long)cat) < 1)
		        G_fatal_error(_("Cannot allocate memory for string"));

		    next = 0;
		    do {
			next = reclass_text(text, cat, &reclass, next);
			if (G_asprintf
			    (&line, "     %5s              %s", num, text) > 0)
			    printline(line);
			else
			    G_fatal_error(_
					  ("Cannot allocate memory for string"));

			*num = 0;
		    }
		    while (next >= 0);
		}
	}
	divider('+');

	fprintf(out, "\n");
    }
    else {			/* rflag or sflag or tflag or gflag or hflag */

	if (rflag->answer) {
	    if (data_type == CELL_TYPE) {
		if( 2 == G_read_range(name, mapset, &crange) ) {
		    fprintf(out, "min=NULL\n");
		    fprintf(out, "max=NULL\n");
		}
		else {
		    fprintf(out, "min=%i\n", (CELL) zmin);
		    fprintf(out, "max=%i\n", (CELL) zmax);
		}
	    }
	    else {
		fprintf(out, "min=%f\n", zmin);
		fprintf(out, "max=%f\n", zmax);
	    }
	}
	if (gflag->answer) {
	    G_format_northing(cellhd.north, tmp1, cellhd.proj);
	    G_format_northing(cellhd.south, tmp2, cellhd.proj);
	    fprintf(out, "north=%s\n", tmp1);
	    fprintf(out, "south=%s\n", tmp2);

	    G_format_easting(cellhd.east, tmp1, cellhd.proj);
	    G_format_easting(cellhd.west, tmp2, cellhd.proj);
	    fprintf(out, "east=%s\n", tmp1);
	    fprintf(out, "west=%s\n", tmp2);
	}
	if (sflag->answer) {
	    G_format_resolution(cellhd.ns_res, tmp3, cellhd.proj);
	    fprintf(out, "nsres=%s\n", tmp3);

	    G_format_resolution(cellhd.ew_res, tmp3, cellhd.proj);
	    fprintf(out, "ewres=%s\n", tmp3);
	}
	if (tflag->answer) {
	    fprintf(out, "datatype=%s\n",
		    (data_type == CELL_TYPE ? "CELL" :
		     (data_type == DCELL_TYPE ? "DCELL" :
		      (data_type == FCELL_TYPE ? "FCELL" : "??"))));
	}
	if (timestampflag->answer) {
	    if (time_ok && (first_time_ok || second_time_ok)) {

		G_format_timestamp(&ts, timebuff);

		/*Create the r.info timestamp string */
		fprintf(out, "timestamp=\"%s\"\n", timebuff);

	    }
	    else {
		fprintf(out, "timestamp=\"none\"\n");
	    }
	}

	if (hflag->answer) {
	    if (hist_ok) {
		fprintf(out, "Data Source:\n");
		fprintf(out, "   %s\n", hist.datsrc_1);
		fprintf(out, "   %s\n", hist.datsrc_2);
		fprintf(out, "Data Description:\n");
		fprintf(out, "   %s\n", hist.keywrd);
		if (hist.edlinecnt) {
		    fprintf(out, "Comments:\n");
		    for (i = 0; i < hist.edlinecnt; i++)
			fprintf(out, "   %s\n", hist.edhist[i]);
		}
	    }
	}
    }				/* else rflag or sflag or tflag or gflag or hflag */

    return EXIT_SUCCESS;
}

/**************************************************************************/
int format_double(double value, char *buf)
{

    sprintf(buf, "%.8lf", value);
    G_trim_decimal(buf);
    return 0;
}


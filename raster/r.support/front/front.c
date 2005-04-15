#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "glocale.h"
#include "local_proto.h"


/*
**********************************************************************
*
* MODULE:       r.support (GRASS core)
*
* AUTHOR(S):    Original by Michael Shapiro - CERL
*               Preliminary parser support by Markus Neteler
*               Port to 6.x by Brad Douglas
*
* PURPOSE:      Build support files for raster map
*               - Edit header
*               - Update status (histogram, range)
*
* COPYRIGHT:    (C) 2000-2005 by the GRASS Development Team
*
*               This program is free software under the GNU General 
*               Public License (>=v2). Read the file COPYING that comes
*               with GRASS for details.
*
**********************************************************************/

int main(int argc, char *argv[])
{
    char *rname   = NULL;	/* Reclassed map name */
    char *rmapset = NULL;	/* Reclassed mapset   */
    char *mapset;		/* Raster mapset      */
    struct Cell_head cellhd;
    struct GModule *module;
    struct Option *raster;
    char element[255];
    char buf[512];
    int cellhd_ok;		/* Is cell header OK? */
    int is_reclass;		/* Is raster reclass? */
    char *infile;


    /* Initialize GIS engine */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description = _("Allows creation and/or modification of "
                          "raster map layer support files.");

    raster = G_define_standard_option(G_OPT_R_INPUT);
    raster->key = "map";
    raster->required = YES;

    /* Parse command-line options */
    if (G_parser(argc, argv)) {
        G_warning(_("Unable to parse arguments."));
        G_usage();
    }

    /* Make sure raster exists and set mapset */
    infile = raster->answer;
    mapset = G_find_cell2(infile, "");
    if (mapset == NULL)
        G_fatal_error(_("Unable to find [%s]."), infile);

    cellhd_ok = (G_get_cellhd(raster->answer, mapset, &cellhd) >= 0);
    is_reclass = (G_is_reclass(raster->answer, mapset, rname, rmapset) > 0);

    /* Cell header */
    sprintf(buf, _("Edit header for [%s]? "), raster->answer);
    if (is_reclass) {
        G_message(_("\nNOTE: [%s] is a reclass of [%s in %s]"),
                  raster->answer, rname, rmapset);
    } else if (G_yes(buf, cellhd_ok ? 0 : 1)) {
	G_clear_screen();

        run_etc_support("modhead", G_fully_qualified_name(raster->answer, mapset));

        if ((cellhd_ok = G_get_cellhd(raster->answer, mapset, &cellhd) > 0)) {
            hitreturn();
            G_clear_screen();
        } else if (!cellhd_ok)
            G_fatal_error(_("Canceling from edit header."));
    }

    /* Check the histogram and range */
    check_stats(raster->answer, mapset);

    /* Category file */
    sprintf(buf, _("Edit the category file for [%s]? "), raster->answer);
    if (G_yes(buf, 0)) {
        G_clear_screen();
        run_etc_support("modcats", G_fully_qualified_name(raster->answer, mapset));
        hitreturn();
        G_clear_screen();
    }

    /* Color table */
    sprintf(buf, _("Create/Update the color table for [%s]? "), raster->answer);
    if (G_yes(buf, 0)) {
        G_clear_screen();
        run_etc_support("modcolr", G_fully_qualified_name(raster->answer, mapset));
        hitreturn();
        G_clear_screen();
    }

    /* History file */
    sprintf(buf, _("Edit the history file for [%s]? "), raster->answer);
    if (G_yes(buf, 0)) {
        G_clear_screen();
        run_etc_support("modhist", G_fully_qualified_name(raster->answer, mapset));
        hitreturn();
        G_clear_screen();
    }

    /* null file */
    G_message(_("\nThe null file for [%s] may indicate that some "
              "cells contain\n no data. If the null file for [%s] "
              "doesn't exist, zero cells in\n it are treated by "
              "GRASS application programs as no data."), 
              raster->answer, raster->answer);

    sprintf(buf, _("\nDo you want to create/reset the null file "
            "for [%s] so that null cell values are considered valid data? "), raster->answer);
    if (G_yes(buf, 0)) {
        unsigned char *null_bits;
        int row, col;
        int null_fd;

        if (is_reclass)
            G_fatal_error(_("[%s] is a reclass of another map. Exiting."), raster->answer);

        G_clear_screen();
        /* Create a file of no-nulls */
        null_bits = G__allocate_null_bits(cellhd.cols);
        for (col = 0; col < G__null_bitstream_size(cellhd.cols); col++)
            null_bits[col] = 0;

        /* Open null file for writing */
        sprintf(element, "cell_misc/%s", raster->answer);
        null_fd = G_open_new(element, "null");
 
        G_message(_("Writing new null file for [%s]... "), raster->answer);
        for (row = 0; row < cellhd.rows; row++) {
            G_percent(row, cellhd.rows, 1);
            if (G__write_null_bits(null_fd, null_bits, row, cellhd.cols, 0) < 0)
                G_fatal_error(_("Error writing null row [%d]."), row);
        }
        G_percent(row, cellhd.rows, 1);

        /* Cleanup */
        close(null_fd);
        G_free(null_bits);

        hitreturn();
        G_clear_screen();
    }

    sprintf(buf, _("\nDo you want to delete the null file for [%s]\n"
            "(all zero cells will then be considered no data)? "), raster->answer);
    if (G_yes(buf, 0)) {
        int null_fd;
        char path[400];

        if (is_reclass)
            G_fatal_error(_("[%s] is a reclass of another map. Exiting."), raster->answer);

        G_clear_screen();

        /* Write a file of no-nulls */
        G_message(_("Removing null file for [%s]...\n"), raster->answer);

        snprintf(element, sizeof(element), "cell_misc/%s", raster->answer);
        null_fd = G_open_new(element, "null");
        G__file_name(path, element, "null", mapset);
        unlink(path);
        close(null_fd);

        G_done_msg(_("Done."));
    }

    return EXIT_SUCCESS;
}


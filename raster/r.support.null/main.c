/*
**********************************************************************
*
* MODULE:        r.support.null
*
* AUTHOR(S):     Brad Douglas <rez touchofmadness com>
*
* PURPOSE:       Modify raster categories
*
* COPYRIGHT:     (C) 2006 by the GRASS Development Team
*
*                This program is free software under the GNU General
*                Purpose License (>=v2). Read the file COPYING that
*                comes with GRASS for details.
*
***********************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <grass/gis.h>
#include <grass/glocale.h>


int main (int argc, char *argv[])
{
    char *mapset  = NULL;
    char *rmapset = NULL;
    char *rname   = NULL;
    char element[255];
    struct GModule *module;
    struct {
        struct Option *raster;
        struct Flag *valid_null, *delete_null;
    } parm;

    /* Initialize GIS engine */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description = _("Update raster map NULLs");

    parm.raster = G_define_standard_option(G_OPT_R_MAP);

    parm.valid_null              = G_define_flag();
    parm.valid_null->key         = 'n';
    parm.valid_null->description = _("Creates or resets null file so null files are considered valid data");

    parm.delete_null              = G_define_flag();
    parm.delete_null->key         = 'd';
    parm.delete_null->description = _("Delete null file (all zero cells will be considered no data");

    /* parse command-line options */
    if (G_parser(argc, argv))
        exit(EXIT_FAILURE);

    mapset = G_find_cell2(parm.raster->answer, G_mapset());
    if (mapset == NULL)
        G_fatal_error(_("Raster file [%s] not found"), parm.raster->answer);

    /* make sure only one flag is used */
    if ((parm.valid_null->answer && parm.delete_null->answer) ||
       (!parm.valid_null->answer && !parm.delete_null->answer))
    {
        G_warning(_("You must choose a single flag"));
        G_usage();
    }

    /* make sure the map is not a reclass */
    if (0 > G_is_reclass(parm.raster->answer, mapset, rname, rmapset))
        G_fatal_error(_("[%s] is a reclass of [%s in %s]. Exiting."), 
                        parm.raster->answer, rname, rmapset);

    /* handle valid nulls */
    if (parm.valid_null->answer)
    {
        struct Cell_head cellhd;
        unsigned char *null_bits = NULL;
        int row, col, null_fd;

        /* create a file of no-nulls */
        null_bits = G__allocate_null_bits(cellhd.cols);
        for (col = 0; col < G__null_bitstream_size(cellhd.cols); col++)
            null_bits[col] = 0;

        /* open null file for writing */
        sprintf(element, "cell_misc/%s", parm.raster->answer);
        null_fd = G_open_new(element, "null");

        G_message(_("Writing new null file for [%s]..."), parm.raster->answer);

        for (row = 0; row < cellhd.rows; row++) {
            G_percent(row, cellhd.rows, 1);

            if (0 > G__write_null_bits(null_fd, null_bits, row, cellhd.cols, 0))
                G_fatal_error(_("Error writing null file at row [%d]"), row);
        }
        G_percent(row, cellhd.rows, 1);

        close(null_fd);
        G_free(null_bits);
    }

    /* handle delete null file */
    if (parm.delete_null->answer)
    {
        int null_fd;
        char path[400];

        G_message(_("Removing null file for [%s]..."), parm.raster->answer);

        sprintf(element, "cell_misc/%s", parm.raster->answer);
        null_fd = G_open_new(element, "null");
        G__file_name(path, element, "null", mapset);
        unlink(path);
        close(null_fd);
    }

    return EXIT_SUCCESS;
}

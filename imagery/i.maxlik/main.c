
/****************************************************************************
 *
 * MODULE:       i.maxlik
 * AUTHOR(S):    Michael Shapiro (USACERL) & Tao Wen (UIUC)
 *               (original contributors)
 *               Markus Neteler <neteler itc.it>,
 *               Roberto Flor <flor itc.it>, 
 *               Bernhard Reiter <bernhard intevation.de>, 
 *               Brad Douglas <rez touchofmadness.com>, 
 *               Glynn Clements <glynn gclements.plus.com>, 
 *               Jan-Oliver Wagner <jan intevation.de>
 * PURPOSE:      maximum likelihood classification of image groups
 * COPYRIGHT:    (C) 1999-2008 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <stdlib.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "global.h"
#include "local_proto.h"

char *group;
char *subgroup;
char *sigfile;
struct Ref Ref;
struct Signature S;
DCELL **cell;
int *cellfd;
CELL *class_cell, *reject_cell;
int class_fd, reject_fd;
char *class_name, *reject_name;
double *B;
double *P;

int main(int argc, char *argv[])
{
    struct Categories cats;
    struct Colors colr;
    struct Ref group_ref;
    int nrows, ncols;
    int row;
    int band;
    int i;
    struct GModule *module;
    struct
    {
	struct Option *group, *subgroup, *sigfile, *class, *reject;
    } parm;
    struct
    {
	struct Flag *quiet;
    } flag;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("imagery, classification, MLC");
    module->label =
	_("Classifies the cell spectral reflectances in imagery data.");
    module->description =
	_("Classification is based on the spectral signature information "
	  "generated by either i.cluster, i.class, or i.gensig.");

    parm.group = G_define_standard_option(G_OPT_I_GROUP);

    parm.subgroup = G_define_standard_option(G_OPT_I_SUBGROUP);

    parm.sigfile = G_define_option();
    parm.sigfile->key = "sigfile";
    parm.sigfile->required = YES;
    parm.sigfile->type = TYPE_STRING;
    parm.sigfile->key_desc = "name";
    parm.sigfile->label = _("Name of file containing signatures");
    parm.sigfile->description = _("Generated by either i.cluster, i.class, or i.gensig");

    parm.class = G_define_standard_option(G_OPT_R_OUTPUT);
    parm.class->key = "class";
    parm.class->required = YES;
    parm.class->description = _("Name for raster map holding classification results");

    parm.reject = G_define_standard_option(G_OPT_R_OUTPUT);
    parm.reject->key = "reject";
    parm.reject->required = NO;
    parm.reject->description =
	_("Name for raster map holding reject threshold results");

    flag.quiet = G_define_flag();
    flag.quiet->key = 'q';
    flag.quiet->description = _("Run quietly");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    class_name = parm.class->answer;
    reject_name = parm.reject->answer;
    group = parm.group->answer;
    subgroup = parm.subgroup->answer;
    sigfile = parm.sigfile->answer;

    open_files();

    nrows = G_window_rows();
    ncols = G_window_cols();

    for (row = 0; row < nrows; row++) {
	G_percent(row, nrows, 2);

	for (band = 0; band < Ref.nfiles; band++)
	    if (G_get_d_raster_row(cellfd[band], cell[band], row) < 0)
		G_fatal_error(_("Unable to read raster map row %d"),
			      row);
	
	classify(class_cell, reject_cell, ncols);
	G_put_raster_row(class_fd, class_cell, CELL_TYPE);
	if (reject_fd > 0)
	    G_put_raster_row(reject_fd, reject_cell, CELL_TYPE);
    }
    G_percent(nrows, nrows, 2);

    G_close_cell(class_fd);
    if (reject_fd > 0)
	G_close_cell(reject_fd);

    G_init_cats((CELL) S.nsigs, "Maximum Likelihood Classification", &cats);
    for (i = 0; i < S.nsigs; i++) {
	if (*S.sig[i].desc)
	    G_set_cat((CELL) (i + 1), S.sig[i].desc, &cats);
    }
    G_write_cats(class_name, &cats);
    G_free_cats(&cats);

    if (reject_fd > 0) {
	char title[100];

	sprintf(title, "Rejection Probability for %s", class_name);

	G_init_cats((CELL) 17, title, &cats);
	G_set_cats_title(title, &cats);
	G_set_cat((CELL) 0, "no data", &cats);
	G_set_cat((CELL) 1, "0.1%", &cats);
	G_set_cat((CELL) 2, "0.5%", &cats);
	G_set_cat((CELL) 3, "1%", &cats);
	G_set_cat((CELL) 4, "2%", &cats);
	G_set_cat((CELL) 5, "5%", &cats);
	G_set_cat((CELL) 6, "10%", &cats);
	G_set_cat((CELL) 7, "20%", &cats);
	G_set_cat((CELL) 8, "30%", &cats);
	G_set_cat((CELL) 9, "50%", &cats);
	G_set_cat((CELL) 10, "70%", &cats);
	G_set_cat((CELL) 11, "80%", &cats);
	G_set_cat((CELL) 12, "90%", &cats);
	G_set_cat((CELL) 13, "95%", &cats);
	G_set_cat((CELL) 14, "98%", &cats);
	G_set_cat((CELL) 15, "99%", &cats);
	G_set_cat((CELL) 16, "100%", &cats);
	G_set_cat((CELL) 17, "bad", &cats);
	G_write_cats(reject_name, &cats);
	G_free_cats(&cats);

	G_make_grey_scale_colors(&colr, (CELL) 1, (CELL) 16);

	G_set_color((CELL) 0, 0, 255, 0, &colr);
	G_set_color((CELL) 17, 255, 0, 0, &colr);
	G_write_colors(reject_name, G_mapset(), &colr);
	G_free_colors(&colr);
    }

    /* associate the output files with the group */
    I_get_group_ref(group, &group_ref);
    I_add_file_to_group_ref(class_name, G_mapset(), &group_ref);
    if (reject_cell)
	I_add_file_to_group_ref(reject_name, G_mapset(), &group_ref);

    I_put_group_ref(group, &group_ref);
    make_history(class_name, group, subgroup, sigfile);

    exit(EXIT_SUCCESS);
}

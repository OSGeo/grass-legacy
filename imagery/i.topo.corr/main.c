
/****************************************************************************
 *
 * MODULE:       i.topo.corr
 *
 * AUTHOR(S):    E. Jorge Tizado - ej.tizado@unileon.es
 *
 * PURPOSE:      Topographic corrections
 *
 * COPYRIGHT:    (C) 2002, 2005, 2008 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>

#include "local_proto.h"

int full_open_old(Gfile * gf, char *fname)
{
    gf->fd = -1;

    snprintf(gf->name, 127, "%s", fname);

    gf->mapset = G_find_cell2(gf->name, "");
    if (gf->mapset == NULL)
	G_fatal_error(_("Raster map <%s> not found"), gf->name);

    gf->fd = G_open_cell_old(gf->name, gf->mapset);
    if (gf->fd < 0)
	G_fatal_error(_("Unable to open raster map <%s@%s>"), gf->name,
		      gf->mapset);

    gf->type = G_raster_map_type(gf->name, gf->mapset);

    return gf->fd;
}

int full_open_new(Gfile * gf, char *fname, RASTER_MAP_TYPE ftype)
{
    gf->fd = -1;

    snprintf(gf->name, 127, "%s", fname);
    if (G_legal_filename(gf->name) < 0)
	G_fatal_error(_("<%s> is an illegal name"), gf->name);

    gf->type = ftype;
    gf->mapset = G_mapset();
    gf->fd = G_open_raster_new(gf->name, gf->type);
    if (gf->fd < 0)
	G_fatal_error(_("Unable to create raster map <%s>"), gf->name);
    
    return gf->fd;
}

int main(int argc, char *argv[])
{
    struct History history;
    struct GModule *module;
    struct Cell_head hd_band, hd_dem, window;

    char bufname[128];		/* TODO: use GNAME_MAX? */

    int i;
    struct Option *base, *output, *input, *zeni, *azim, *metho;
    struct Flag *ilum;

    Gfile dem, out, band;
    double zenith, azimuth;
    int method = COSINE;

    /* initialize GIS environment */
    G_gisinit(argv[0]);

    /* initialize module */
    module = G_define_module();
    module->description = _("Computes topographic correction of reflectance.");
    module->keywords =
	_("imagery, terrain, topographic correction");
    
    /* It defines the different parameters */

    input = G_define_standard_option(G_OPT_R_INPUTS);
    input->required = NO;
    input->multiple = YES;
    input->description =
	_("Name of reflectance raster maps to be corrected topographically");

    output = G_define_standard_option(G_OPT_R_OUTPUT);
    output->description =
	_("Name (flag -i) or prefix for output raster maps");

    base = G_define_standard_option(G_OPT_R_MAP);
    base->key = "basemap";
    base->description = _("Name of input base raster map (elevation or illumination)");

    zeni = G_define_option();
    zeni->key = "zenith";
    zeni->type = TYPE_DOUBLE;
    zeni->required = YES;
    zeni->description = _("Solar zenith in degrees");

    azim = G_define_option();
    azim->key = "azimuth";
    azim->type = TYPE_DOUBLE;
    azim->required = NO;
    azim->description = _("Solar azimuth in degrees (only if flag -i)");

    metho = G_define_option();
    metho->key = "method";
    metho->type = TYPE_STRING;
    metho->required = NO;
    metho->options = "cosine,minnaert,c-factor,percent";
    metho->description = _("Topographic correction method");
    metho->answer = "c-factor";

    ilum = G_define_flag();
    ilum->key = 'i';
    ilum->description = _("Output sun illumination terrain model");
    
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    if (ilum->answer && azim->answer == NULL)
	G_fatal_error(_("Solar azimuth is necessary to calculate illumination terrain model"));

    if (!ilum->answer && input->answer == NULL)
	G_fatal_error(_("Reflectance maps are necessary to make topographic correction"));

    zenith = atof(zeni->answer);

    /* Evaluate only cos_i raster file */
    /* i.topo.corr -i out=cosi.on07 base=SRTM_v2 zenith=33.3631 azimuth=59.8897 */
    if (ilum->answer) {
	azimuth = atof(azim->answer);
	/* Warning: make buffers and output after set window */
	full_open_old(&dem, base->answer);
	/* Set window to DEM file */
	G_get_window(&window);
	G_get_cellhd(dem.name, dem.mapset, &hd_dem);
	G_align_window(&window, &hd_dem);
	G_set_window(&window);
	/* Open and buffer of the output file */
	full_open_new(&out, output->answer, DCELL_TYPE);
	out.rast = G_allocate_raster_buf(out.type);
	/* Open and buffer of the elevation file */
	if (dem.type == CELL_TYPE) {
	    dem.rast = G_allocate_raster_buf(CELL_TYPE);
	    eval_c_cosi(&out, &dem, zenith, azimuth);
	}
	else if (dem.type == FCELL_TYPE) {
	    dem.rast = G_allocate_raster_buf(FCELL_TYPE);
	    eval_f_cosi(&out, &dem, zenith, azimuth);
	}
	else if (dem.type == DCELL_TYPE) {
	    dem.rast = G_allocate_raster_buf(DCELL_TYPE);
	    eval_d_cosi(&out, &dem, zenith, azimuth);
	}
	else {
	    G_fatal_error(_("Elevation raster map of unknown type"));
	}
	/* Close files, buffers, and write history */
	G_free(dem.rast);
	G_close_cell(dem.fd);
	G_free(out.rast);
	G_close_cell(out.fd);
	G_short_history(out.name, "raster", &history);
	G_command_history(&history);
	G_write_history(out.name, &history);
    }
    /* Evaluate topographic correction for all bands */
    /* i.topo.corr input=on07.toar.1 out=tcor base=cosi.on07 zenith=33.3631 method=c-factor */
    else {
	/*              if (G_strcasecmp(metho->answer, "cosine") == 0)        method = COSINE;
	 *               else if (G_strcasecmp(metho->answer, "percent") == 0)  method = PERCENT;
	 *               else if (G_strcasecmp(metho->answer, "minnaert") == 0) method = MINNAERT;
	 *               else if (G_strcasecmp(metho->answer, "c-factor") == 0) method = C_CORRECT;
	 *               else G_fatal_error(_("Invalid method: %s"), metho->answer);
	 */

	if (metho->answer[1] == 'o')
	    method = COSINE;
	else if (metho->answer[1] == 'e')
	    method = PERCENT;
	else if (metho->answer[1] == 'i')
	    method = MINNAERT;
	else if (metho->answer[1] == '-')
	    method = C_CORRECT;
	else
	    G_fatal_error(_("Invalid method: %s"), metho->answer);

	full_open_old(&dem, base->answer);
	if (dem.type == CELL_TYPE)
	    G_fatal_error(_("Illumination model is of CELL type"));

	for (i = 0; input->answers[i] != NULL; i++) {
	    G_message("Band %s: ", input->answers[i]);
	    /* Abre fichero de bandas y el de salida */
	    full_open_old(&band, input->answers[i]);
	    if (band.type != DCELL_TYPE) {
		G_warning(_("Reflectance of raster map <%s> is not of DCELL type - ignored"),
			  input->answers[i]);
		G_close_cell(band.fd);
		continue;
	    }
	    G_get_cellhd(band.name, band.mapset, &hd_band);
	    G_set_window(&hd_band);	/* Antes de out_open y allocate para mismo tamaño */
	    /* ----- */
	    snprintf(bufname, 127, "%s.%s", output->answer,
		     input->answers[i]);
	    full_open_new(&out, bufname, DCELL_TYPE);
	    out.rast = G_allocate_raster_buf(out.type);
	    band.rast = G_allocate_raster_buf(band.type);
	    dem.rast = G_allocate_raster_buf(dem.type);
	    /* ----- */
	    eval_tcor(method, &out, &dem, &band, zenith);
	    /* ----- */
	    G_free(dem.rast);
	    G_free(band.rast);
	    G_close_cell(band.fd);
	    G_free(out.rast);
	    G_close_cell(out.fd);
	    G_short_history(out.name, "raster", &history);
	    G_command_history(&history);
	    G_write_history(out.name, &history);

	    char command[300];

	    /* TODO: better avoid system() */
	    sprintf(command, "r.colors map=%s color=grey", out.name);
	    system(command);

/* new but not functional:
	    {
		struct FPRange range;
		DCELL min, max;
		struct Colors grey;
		G_read_fp_range(out.name, G_mapset(), &range);
		G_get_fp_range_min_max(&range, &min, &max);
		G_make_grey_scale_colors(&grey, min, max);
		G_write_colors(out.name, G_mapset(), &grey);
	    }
*/

	}
	G_close_cell(dem.fd);
    }

    exit(EXIT_SUCCESS);
}

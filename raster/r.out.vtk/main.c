
/****************************************************************************
*
* MODULE:       r.out.vtk  
*   	    	
* AUTHOR(S):    Original author 
*               Soeren Gebbert soerengebbert@gmx.de
* 		08 23 2005 Berlin
* PURPOSE:      Converts raster maps into the VTK-Ascii format  
*
* COPYRIGHT:    (C) 2005 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "globaldefs.h"
#include "writeascii.h"

#define MAIN
#include "parameters.h"



/* ************************************************************************* */
/* MAIN ******************************************************************** */
/* ************************************************************************* */
int main(int argc, char *argv[])
{
    struct Cell_head region;
    FILE *fp = NULL;
    struct GModule *module;
    int i = 0, polytype = 0;
    char *null_value, *mapset;
    int out_type;
    int fd;			/*Normale maps ;) */
    int rgbfd[3];
    int celltype[3] = { 0, 0, 0 };
    int headertype;

    /* Initialize GRASS */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description = _("Converts raster maps into the VTK-Ascii format");

    /* Get parameters from user */
    SetParameters();

    /* Have GRASS get inputs */
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    /*open the output */
    if (param.output->answer) {
	fp = fopen(param.output->answer, "w");
	if (fp == NULL) {
	    perror(param.output->answer);
	    G_usage();
	    exit(EXIT_FAILURE);
	}
    }
    else
	fp = stdout;

    /* Figure out the region from the map */
    if (G__get_window(&region, "", "WIND", G_mapset()) != NULL) {
	G_get_default_window(&region);
	G_put_window(&region);
    }

    /*Set the null Value, maybe i have to check this? */
    null_value = param.null_val->answer;

  /********************* WRITE ELEVATION *************************************/
    if (param.elevationmap->answer) {
	/*If the elevation is set, write the correct Header */
	if (param.usestruct->answer) {
	    writeVTKStructuredElevationHeader(fp, region);
	}
	else {
	    writeVTKPolygonalElevationHeader(fp, region);
	}

	G_debug(3, _("Open Raster file %s"), param.elevationmap->answer);

	mapset = G_find_cell2(param.elevationmap->answer, "");
	out_type = G_raster_map_type(param.elevationmap->answer, mapset);

	if (mapset == NULL) {
	    G_fatal_error(_("Cell file [%s] not found\n"),
			  param.elevationmap->answer);
	    exit(EXIT_FAILURE);
	}

	/* open raster file */
	fd = G_open_cell_old(param.elevationmap->answer, mapset);
	if (fd < 0) {
	    G_fatal_error(_("Could not open map %s\n"),
			  param.elevationmap->answer);
	    exit(EXIT_FAILURE);
	}



	/*The write the Coordinates */
	if (param.usestruct->answer) {
	    writeVTKStructuredCoordinates(fd, fp, param.elevationmap->answer,
					  region, out_type, null_value,
					  atof(param.elevscale->answer));
	}
	else {
	    polytype = QUADS;	/*The default */

	    if (param.usetriangle->answer)
		polytype = TRIANGLE_STRIPS;

	    if (param.usevertices->answer)
		polytype = VERTICES;

	    writeVTKPolygonalCoordinates(fd, fp, param.elevationmap->answer,
					 region, out_type, null_value,
					 atof(param.elevscale->answer),
					 polytype);
	}
	/*We have Pointdata */
	writeVTKPointDataHeader(fp, region);
	G_close_cell(fd);
    }
    else {
	/*Should pointdata or celldata be written */
	if (param.point->answer)
	    headertype = 1;
	else
	    headertype = 0;

	/*If no elevation is given, write the normal Header */
	if (param.origin->answer)
	    writeVTKNormalHeader(fp, region,
				 atof(param.elevscale->answer) *
				 (atof(param.elev->answer)), headertype);
	else
	    writeVTKNormalHeader(fp, region, atof(param.elev->answer),
				 headertype);

	if (param.point->answer)
	    writeVTKPointDataHeader(fp, region);
	else
	    writeVTKCellDataHeader(fp, region);
    }


  /********************** WRITE NORMAL DATA; CELL OR POINT *******************/
    /*Loop over all input maps! */
    for (i = 0; param.input->answers[i] != NULL; i++) {

	G_debug(3, _("Open Raster file %s"), param.input->answers[i]);

	mapset = NULL;

	mapset = G_find_cell2(param.input->answers[i], "");

	if (mapset == NULL) {
	    G_fatal_error(_("Cell file [%s] not found\n"),
			  param.input->answers[i]);
	    exit(EXIT_FAILURE);
	}

	out_type = G_raster_map_type(param.input->answers[i], mapset);

	/* open raster file */
	fd = G_open_cell_old(param.input->answers[i], mapset);
	if (fd < 0) {
	    G_fatal_error(_("Could not open map %s\n"),
			  param.input->answers[i]);
	    exit(EXIT_FAILURE);
	}

	/*Now write the data */
	writeVTKData(fd, fp, param.input->answers[i], region, out_type,
		     null_value);
	G_close_cell(fd);
    }

  /********************** WRITE RGB IMAGE DATA; CELL OR POINT ****************/
    if (param.rgbmaps->answers != NULL) {

	if (param.rgbmaps->answers[0] != NULL &&
	    param.rgbmaps->answers[1] != NULL &&
	    param.rgbmaps->answers[2] != NULL) {

	    /*Loop over all input maps! */
	    for (i = 0; i < 3; i++) {
		G_debug(3, _("Open Raster file %s"), param.rgbmaps->answers[i]);

		mapset = NULL;

		mapset = G_find_cell2(param.rgbmaps->answers[i], "");
		celltype[i] =
		    G_raster_map_type(param.rgbmaps->answers[i], mapset);

		if (mapset == NULL) {
		    G_fatal_error(_("Cell file [%s] not found\n"),
				  param.input->answers[i]);
		    exit(EXIT_FAILURE);
		}


		/* open raster file */
		rgbfd[i] = G_open_cell_old(param.rgbmaps->answers[i], mapset);
		if (rgbfd[i] < 0) {
		    G_fatal_error(_("Could not open map %s\n"),
				  param.rgbmaps->answers[i]);
		    exit(EXIT_FAILURE);
		}
	    }

	    if (celltype[0] == celltype[1] && celltype[0] == celltype[2]) {
		G_debug(3, _("Writing VTK ImageData\n"));

		out_type = celltype[0];
		/*Now write the data */
		writeVTKRGBImageData(rgbfd[0], rgbfd[1], rgbfd[2], fp,
				     "RGB_Image", region, out_type);
	    }
	    else {
		G_warning(_
			  ("Wrong RGB maps. Maps should be the same type! RGB output not added!"));
		/*do nothing */
	    }

	    for (i = 0; i < 3; i++)
		G_close_cell(rgbfd[i]);

	}
	else {
	    G_warning(_("Wrong RGB maps. RGB output not added!"));
	    /*do nothing */
	}
    }

    if (param.output->answer && fp != NULL)
	if (fclose(fp)) {
	    G_fatal_error(_("Error closing VTK-ASCII file"));
	    exit(EXIT_FAILURE);
	}

    return 0;
}

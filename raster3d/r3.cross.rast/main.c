
/****************************************************************************
*
* MODULE:       r3.cross.rast 
*   	    	
* AUTHOR(S):    Original author 
*               Soeren Gebbert soerengebbert at gmx de
* 		23 Feb 2006 Berlin
* PURPOSE:      Creates a cross section 2D map from one G3D raster map based on a 2D elevation map  
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
#include <grass/G3d.h>
#include <grass/glocale.h>


/*- Parameter and global variables -----------------------------------------*/
typedef struct
{
    struct Option *input, *output, *elevation;
    struct Flag *mask;
} ParameterType;

ParameterType Parameter;	/*Parameter */
int globalElevMapType;


/*- prototypes --------------------------------------------------------------*/
void FatalError(void *map, int elevfd, int outfd, char *errorMsg);	/*Simple Error message */
void SetParameter();		/*Fill the ParameterType structure */
void G3dCrossRaster(void *map, G3D_Region region, int elevfd, int outfd);	/*Write the raster */
void CloseOutputMap(int fd);	/*close the map */



/* ************************************************************************* */
/* Error handling ********************************************************** */
/* ************************************************************************* */
void FatalError(void *map, int elevfd, int outfd, char *errorMsg)
{

    /* Close files and exit */

    if (map != NULL) {
	if (!G3d_closeCell(map))
	    G3d_fatalError(_("Could not close G3D map"));
    }

    /*unopen the output map */
    if (outfd != -1)
	G_unopen_cell(outfd);

    if (elevfd != -1)
	CloseOutputMap(elevfd);

    G3d_fatalError(errorMsg);
    exit(EXIT_FAILURE);

}


/* ************************************************************************* */
/* Set up the arguments we are expecting *********************************** */
/* ************************************************************************* */
void SetParameter()
{
    Parameter.input = G_define_option();
    Parameter.input->key = "input";
    Parameter.input->type = TYPE_STRING;
    Parameter.input->required = YES;
    Parameter.input->gisprompt = "old,grid3,3d-raster";
    Parameter.input->description = _("Input 3D raster map for cross section.");

    Parameter.elevation = G_define_option();
    Parameter.elevation->key = "elevation";
    Parameter.elevation->type = TYPE_STRING;
    Parameter.elevation->required = YES;
    Parameter.elevation->description =
	_("2D elevation map used to create the cross section map");
    Parameter.elevation->gisprompt = "old,cell,raster";

    Parameter.output = G_define_option();
    Parameter.output->key = "output";
    Parameter.output->type = TYPE_STRING;
    Parameter.output->required = YES;
    Parameter.output->description = _("Resulting cross section 2D raster map");
    Parameter.output->gisprompt = "new,cell,raster";

    Parameter.mask = G_define_flag();
    Parameter.mask->key = 'm';
    Parameter.mask->description = _("Use g3d mask (if exists) with input map");
}



/* ************************************************************************* */
/* Calulates the resulting raster map ************************************** */
/* ************************************************************************* */
void G3dCrossRaster(void *map, G3D_Region region, int elevfd, int outfd)
{
    double d1 = 0, f1 = 0;
    int x, y, z, check = 0;
    int rows, cols, depths, typeIntern;
    FCELL *fcell = NULL;
    DCELL *dcell = NULL;
    void *elevrast;
    void *ptr;
    int intvalue;
    float fvalue;
    double dvalue;
    int isnull;
    double elevation = 0;
    double top, tbres, bottom;

    /*shoter names ;) */
    rows = region.rows;
    cols = region.cols;
    depths = region.depths;
    top = region.top;
    bottom = region.bottom;

    /*Calculate the top-bottom resolution */
    tbres = (top - bottom) / depths;

    /*Typ of the G3D Tile */
    typeIntern = G3d_tileTypeMap(map);

    /*Allocate mem for the output maps row */
    if (typeIntern == G3D_FLOAT)
	fcell = G_allocate_f_raster_buf();
    else if (typeIntern == G3D_DOUBLE)
	dcell = G_allocate_d_raster_buf();

    /*Mem for the input map row */
    elevrast = G_allocate_raster_buf(globalElevMapType);

    for (y = 0; y < rows; y++) {
	G_percent(y, rows - 1, 10);

	/*Read the input map row */
	if (!G_get_raster_row(elevfd, elevrast, y, globalElevMapType))
	    FatalError(map, elevfd, outfd,
		       _("Could not get elevation raster row \n"));

	for (x = 0, ptr = elevrast; x < cols; x++, ptr =
	     G_incr_void_ptr(ptr, G_raster_size(globalElevMapType))) {

	    /*we guess the elevation input map has no null values */
	    isnull = 0;

	    if (G_is_null_value(ptr, globalElevMapType)) {
		isnull = 1;	/*input map has nulls */
	    }

	    /*Read the elevation value */
	    if (globalElevMapType == CELL_TYPE) {
		intvalue = *(CELL *) ptr;
		elevation = intvalue;
	    }
	    else if (globalElevMapType == FCELL_TYPE) {
		fvalue = *(FCELL *) ptr;
		elevation = fvalue;
	    }
	    else if (globalElevMapType == DCELL_TYPE) {
		dvalue = *(DCELL *) ptr;
		elevation = dvalue;
	    }

	    /*Check if the elevation is located in the g3d map */
	    if (isnull == 0) {
		/*we guess, we have no hit */
		isnull = 1;
		for (z = 0; z < depths; z++) {	/*From the bottom to the top */
		    if (elevation >= z * tbres + bottom && elevation <= (z + 1) * tbres + bottom) {	/*if at the border, choose the value from the top */
			/*Read the value and put it in the output map row */
			if (typeIntern == G3D_FLOAT) {
			    G3d_getValue(map, x, y, z, &f1, typeIntern);
			    if (G3d_isNullValueNum(&f1, G3D_FLOAT))
				G_set_null_value(&fcell[x], 1, FCELL_TYPE);
			    else
				fcell[x] = (FCELL) f1;
			}
			else {
			    G3d_getValue(map, x, y, z, &d1, typeIntern);
			    if (G3d_isNullValueNum(&d1, G3D_DOUBLE))
				G_set_null_value(&dcell[x], 1, DCELL_TYPE);
			    else
				dcell[x] = (DCELL) d1;

			}
			/*no NULL value should be set */
			isnull = 0;
		    }
		}
	    }

	    /*Set the NULL values */
	    if (isnull == 1) {
		if (typeIntern == G3D_FLOAT)
		    G_set_null_value(&fcell[x], 1, FCELL_TYPE);
		else if (typeIntern == G3D_DOUBLE)
		    G_set_null_value(&dcell[x], 1, DCELL_TYPE);
	    }
	}

	/*Write the data to the output map */
	if (typeIntern == G3D_FLOAT) {
	    check = G_put_f_raster_row(outfd, fcell);
	    if (check != 1)
		FatalError(map, elevfd, outfd, _("Could not write raster row"));
	}

	if (typeIntern == G3D_DOUBLE) {
	    check = G_put_d_raster_row(outfd, dcell);
	    if (check != 1)
		FatalError(map, elevfd, outfd, _("Could not write raster row"));
	}
    }
    G_debug(3, _("\nDone\n"));

    /*Free the mem */
    if (elevrast)
	G_free(elevrast);
    if (dcell)
	G_free(dcell);
    if (fcell)
	G_free(fcell);
}


/* ************************************************************************* */
/* Main function, open the G3D map and create the cross section map ******** */
/* ************************************************************************* */
int main(int argc, char *argv[])
{
    G3D_Region region;
    struct Cell_head window2d;
    struct GModule *module;
    void *map = NULL;		/*The 3D Rastermap */
    int changemask = 0;
    int elevfd = -1, outfd = -1;	/*file descriptors */
    int output_type, cols, rows;
    char *mapset = NULL;

    /* Initialize GRASS */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
	_
	("Creates cross section 2D raster map from g3d raster volume map based on 2D elevation map");

    /* Get Parametereters from user */
    SetParameter();

    /* Have GRASS get inputs */
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    G_debug(3, _("Open 3D raster file %s"), Parameter.input->answer);

    if (NULL == G_find_grid3(Parameter.input->answer, ""))
	G3d_fatalError(_("Requested g3d file not found <%s>"), Parameter.input->answer);

    /* Figure out the region from the map */
    G3d_initDefaults();
    G3d_getWindow(&region);

    /*Check if the g3d-region is equal to the 2d rows and cols */
    rows = G_window_rows();
    cols = G_window_cols();

    /*If not equal, set the 2D windows correct */
    if (rows != region.rows || cols != region.cols) {
	G_message
	    (_("The 2d and 3d region settings are different. I will use the g3d settings to adjust the 2d region."));
	G_get_set_window(&window2d);
	window2d.ns_res = region.ns_res;
	window2d.ew_res = region.ew_res;
	window2d.rows = region.rows;
	window2d.cols = region.cols;
	G_set_window(&window2d);
    }


    /*******************/
    /*Open the g3d map */

    /*******************/
    map = G3d_openCellOld(Parameter.input->answer,
			  G_find_grid3(Parameter.input->answer, ""),
			  &region, G3D_TILE_SAME_AS_FILE,
			  G3D_USE_CACHE_DEFAULT);

    if (map == NULL)
	G3d_fatalError(_("Error opening g3d file <%s>"), Parameter.input->answer);

    /*Get the output type */
    output_type = G3d_fileTypeMap(map);

    if (output_type == G3D_FLOAT || output_type == G3D_DOUBLE) {

	/********************************/
	/*Open the elevation raster map */

	/********************************/
	mapset = G_find_cell2(Parameter.elevation->answer, "");

	if (mapset == NULL) {
	    FatalError(map, -1, -1, _("Elevation map not found\n"));
	}

	elevfd = G_open_cell_old(Parameter.elevation->answer, mapset);
	if (elevfd <= 0)
	    FatalError(map, -1, -1, _("Could not open elevation map\n"));

	globalElevMapType =
	    G_raster_map_type(Parameter.elevation->answer, mapset);

	/**********************/
	/*Open the Outputmap */

	/**********************/

	/*Filename check for output map */
	if (G_legal_filename(Parameter.output->answer) < 0)
	    FatalError(map, elevfd, -1, _("Illegal output file name"));

	if (G_find_cell2(Parameter.output->answer, ""))
	    G_message(_("Output map already exists. Will be overwritten!\n"));

	if (output_type == G3D_FLOAT) {
	    outfd = G_open_raster_new(Parameter.output->answer, FCELL_TYPE);
	    if (outfd < 0)
		FatalError(map, elevfd, -1, _("Could not open output map\n"));
	}
	else if (output_type == G3D_DOUBLE) {
	    outfd = G_open_raster_new(Parameter.output->answer, DCELL_TYPE);
	    if (outfd < 0)
		FatalError(map, elevfd, -1, _("Could not open output map\n"));
	}

	/*if requested set the Mask on */
	if (Parameter.mask->answer) {
	    if (G3d_maskFileExists()) {
		changemask = 0;
		if (G3d_maskIsOff(map)) {
		    G3d_maskOn(map);
		    changemask = 1;
		}
	    }
	}

	/************************/
	/*Create the Rastermaps */

	/************************/
	G3dCrossRaster(map, region, elevfd, outfd);

	/*We set the Mask off, if it was off before */
	if (Parameter.mask->answer) {
	    if (G3d_maskFileExists())
		if (G3d_maskIsOn(map) && changemask)
		    G3d_maskOff(map);
	}

	if (G_close_cell(outfd) < 0)
	    FatalError(map, elevfd, -1, _("unable to close output map"));
	if (G_close_cell(elevfd) < 0)
	    FatalError(map, -1, -1, _("unable to close elevation map"));

    }
    else {
	FatalError(map, -1, -1,
		   _("Wrong G3D Datatype! Cannot create raster map."));
    }

    /* Close files and exit */
    if (!G3d_closeCell(map))
	G3d_fatalError(_("Could not close G3D map <%s>"), Parameter.input->answer);

    return (EXIT_SUCCESS);
}


/* ************************************************************************* */
/* Close the raster map ********************************************* */
/* ************************************************************************* */
void CloseOutputMap(int fd)
{
    if (G_close_cell(fd) < 0)
	G_fatal_error(_("unable to close output map"));
}


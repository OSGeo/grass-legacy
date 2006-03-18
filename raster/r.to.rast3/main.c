/****************************************************************************
*
* MODULE:       r.to.rast3 
*   	    	
* AUTHOR(S):    Original author 
*               Soeren Gebbert soerengebbert@gmx.de
* 		08 01 2005 Berlin
* PURPOSE:      Converts 2D raster map slices to one 3D raster map  
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


/*- Parameters and global variables -----------------------------------------*/
typedef struct
{
    struct Option *input, *output;
} ParameterType;

ParameterType Parameter;		/*Parameters */
int globalRastMapType;
int globalG3dMapType;


/*- prototypes --------------------------------------------------------------*/
void FatalError(void *map, int *fd, int depths, char *errorMsg);	/*Simple Error message */
void SetParameters();		/*Fill the ParameterType structure */
void RasterToG3D(void *map, G3D_Region region, int *fd);	/*Write the raster */
int  OpenInputMap(char *name, char *mapset);	/*opens the outputmap */
void CloseInputMap(int fd);	/*close the map */



/* ************************************************************************* */
/* Error handling ********************************************************** */
/* ************************************************************************* */
void FatalError(void *map, int *fd, int depths, char *errorMsg)
{
    int i;

    /* Close files and exit */
    if (map != NULL) {
	/* should unopen map here! but this functionality is not jet implemented */
	if (!G3d_closeCell(map))
	    G3d_fatalError(_("Could not close the map"));
    }

    if (fd != NULL) {
	for (i = 0; i < depths; i++)
	    CloseInputMap(fd[i]);
    }

    G3d_fatalError(errorMsg);
    exit(EXIT_FAILURE);

}

/* ************************************************************************* */
/* Setg up the arguments we are expexting ********************************** */
/* ************************************************************************* */
void SetParameters()
{
    Parameter.input = G_define_option();
    Parameter.input->key = "input";
    Parameter.input->type = TYPE_STRING;
    Parameter.input->required = YES;
    Parameter.input->description = _("2d raster maps which represent the slices");
    Parameter.input->gisprompt = "old,cell,raster";
    Parameter.input->multiple = YES;

    Parameter.output = G_define_option();
    Parameter.output->key = "output";
    Parameter.output->type = TYPE_STRING;
    Parameter.output->required = YES;
    Parameter.output->gisprompt = "new,grid3,3d-raster";
    Parameter.output->description =
	_("output 3dcell map which will be filled with the 2D raster slices");
}



/* ************************************************************************* */
/* Write the raster maps into one G3D map ********************************** */
/* ************************************************************************* */
void RasterToG3D(void *map, G3D_Region region, int *fd)
{
    int x, y, z;
    int rows, cols, depths;
    void *rast;
    void *ptr;
    FCELL fvalue;
    DCELL dvalue;

    rows = region.rows;
    cols = region.cols;
    depths = region.depths;

    rast = G_allocate_raster_buf(globalRastMapType);

    G_debug(3, _("RasterToG3D: Writing %i raster maps with rows %i cols."),
	    depths, rows, cols);

    /*Every Rastermap */
    for (z = 0; z < depths; z++) {	/*From the bottom to the top */
	G_debug(2, _("Writing g3d slice %i\n"), z + 1);
	for (y = 0; y < rows; y++) {
	    G_percent(y, rows - 1, 10);

	    if (!G_get_raster_row(fd[z], rast, y, globalRastMapType))
		FatalError(map, fd, depths, _("Cold not get raster row \n"));

	    for (x = 0, ptr = rast; x < cols; x++,
		 ptr = G_incr_void_ptr(ptr, G_raster_size(globalRastMapType))) {
		if (globalRastMapType == CELL_TYPE) {
		    if (G_is_null_value(ptr, globalRastMapType)) {
			G3d_setNullValue(&dvalue, 1, G3D_DOUBLE);
		    }
		    else {
			dvalue = *(CELL *) ptr;
		    }
		if (G3d_putValue(map, x, y, z, (char *) &dvalue, G3D_DOUBLE) < 0)
				FatalError(map, fd, depths, "error writing double data");		    
		}
		else if (globalRastMapType == FCELL_TYPE) {
		    if (G_is_null_value(ptr, globalRastMapType)) {
			G3d_setNullValue(&fvalue, 1, G3D_FLOAT);
		    }
		    else {
			fvalue = *(FCELL *) ptr;
		    }
		if (G3d_putValue(map, x, y, z, (char *) &fvalue, G3D_FLOAT) < 0)
				FatalError(map, fd, depths, "error writing float data");		    

		}
		else if (globalRastMapType == DCELL_TYPE) {
		    if (G_is_null_value(ptr, globalRastMapType)) {
			G3d_setNullValue(&dvalue, 1, G3D_DOUBLE);
		    }
		    else {
			dvalue = *(DCELL *) ptr;
		    }
		if (G3d_putValue(map, x, y, z, (char *) &dvalue, G3D_DOUBLE) < 0)
				FatalError(map, fd, depths, "error writing double data");		    

		}

	    }
	}
	G_debug(2, _("\nDone\n"));
    }


    if (rast)
	G_free(rast);

}


/* ************************************************************************* */
/* Main function, open the raster maps and create the G3D raster maps ****** */
/* ************************************************************************* */
int main(int argc, char *argv[])
{
    G3D_Region region;
    struct Cell_head window2d;
    struct GModule *module;
    void *map = NULL;		/*The 3D Rastermap */
    int i = 0;
    int *fd = NULL;		/*The filehanlder array for the 2D inputmaps */
    int cols, rows, opencells;
    char *name;
    char *mapset;
    int maptype_tmp, nofile = 0;

    /* Initialize GRASS */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
	_("Converts 2D raster map slices to one 3D raster map");

    /* Get Parametereters from user */
    SetParameters();

    /* Have GRASS get inputs */
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    /*Check ob Input */
    if (Parameter.output->answer == NULL)
	G3d_fatalError(_("No output map"));

    /* Figure out the region from the map */
    G3d_initDefaults();
    G3d_getWindow(&region);

    /*Check if the g3d-region is equal to the 2d rows and cols */
    rows = G_window_rows();
    cols = G_window_cols();

    /*If not equal, set the 2D windows correct*/
    if (rows != region.rows || cols != region.cols) {
	G_message("The 2d and 3d region settings are different. I will use the g3d settings to adjust the 2d region.");
	G_get_set_window(&window2d);
	window2d.ns_res = region.ns_res;
	window2d.ew_res = region.ew_res;
	window2d.rows = region.rows;
	window2d.cols = region.cols;
	G_set_window(&window2d);
    }


    /*prepare the filehandler */
    fd = (int *)G_malloc(region.depths * sizeof(int));

    if (fd == NULL)
	FatalError(map, NULL, 0, _("out of memory!"));

    if (G_legal_filename(Parameter.output->answer) < 0)
	FatalError(map, NULL, 0, _("Illegal output file name"));


    mapset = NULL;
    name = NULL;

    globalRastMapType = DCELL_TYPE;
    globalG3dMapType = G3D_DOUBLE;
    maptype_tmp = DCELL_TYPE;

    opencells = 0;		/*Number of opened maps */
    /*Loop over all output maps! open */
    for (i = 0; i < region.depths; i++) {
	/*Open only existing maps */
	if (Parameter.input->answers[i] != NULL && nofile == 0) {
	    mapset = NULL;
	    name = NULL;
	    name = Parameter.input->answers[i];
	    mapset = G_find_cell2(name, "");

	    if (mapset == NULL) {
		FatalError(map, fd, opencells, _("Cell file not found\n"));
	    }
	}
	else {
	    nofile = 1;
	}

	/*if only one map is given, open it depths - times */
	G_message(_("Open raster map %s - one time for each depth (%d/%d)"), name, i+1, region.depths );
	fd[i] = OpenInputMap(name, mapset);
	opencells++;

	/*maptype */
	if (i == 0)
	    globalRastMapType = G_raster_map_type(name, mapset);

	maptype_tmp = G_raster_map_type(name, mapset);

	if (maptype_tmp != globalRastMapType) {
	    FatalError(map, fd, opencells,
		       _
		       ("Input maps have to be from the same type. CELL, FCELL or DCELL!"));
	}
    }

    G_message(_("Creating 3D raster map\n"));
    map = NULL;

    if (globalRastMapType == CELL_TYPE) {
	map =
	    G3d_openCellNew(Parameter.output->answer, G3D_DOUBLE,
			    G3D_USE_CACHE_DEFAULT, &region);
	globalG3dMapType = G3D_DOUBLE;
    }
    else if (globalRastMapType == FCELL_TYPE) {
	map =
	    G3d_openCellNew(Parameter.output->answer, G3D_FLOAT,
			    G3D_USE_CACHE_DEFAULT, &region);
	globalG3dMapType = G3D_FLOAT;
    }
    else if (globalRastMapType == DCELL_TYPE) {
	map =
	    G3d_openCellNew(Parameter.output->answer, G3D_DOUBLE,
			    G3D_USE_CACHE_DEFAULT, &region);
	globalG3dMapType = G3D_DOUBLE;
    }

    if (map == NULL)
	FatalError(map, fd, opencells, _("error opening g3d file"));

    /*Create the G3D Rastermap */
    RasterToG3D(map, region, fd);

    /*Loop over all output maps! close */
    for (i = 0; i < region.depths; i++)
	CloseInputMap(fd[i]);

    if (fd)
	G_free(fd);

    /* Close files and exit */
    if (!G3d_closeCell(map))
	G3d_fatalError(_("Error closing g3d file"));

    map = NULL;

    return (EXIT_SUCCESS);
}



/* ************************************************************************* */
/* Open the raster input map *********************************************** */
/* ************************************************************************* */
int OpenInputMap(char *name, char *mapset)
{
    int fd;

    G_debug(3, "Open Raster file %s in Mapset %s", name, mapset);


    /* open raster file */
    fd = G_open_cell_old(name, mapset);

    if (fd < 0)
	G_fatal_error(_("Could not open map %s"), name);


    return fd;
}

/* ************************************************************************* */
/* Close the raster input map ********************************************** */
/* ************************************************************************* */
void CloseInputMap(int fd)
{
    if (G_close_cell(fd) < 0)
	G_fatal_error(_("unable to close input map"));
}


/****************************************************************************
*
* MODULE:       r3.in.rast 
*   	    	
* AUTHOR(S):    Original author 
*               Soeren Gebbert soerengebbert@gmx.de
* 		08 01 2005 Berlin
* PURPOSE:      Converts 3D raster maps to 2D raster maps  
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
} paramType;

paramType param;		/*Parameters */
int global_rastMapType;
int global_g3dMapType;


/*- prototypes --------------------------------------------------------------*/
void fatalError(void *map, int *fd, int depths, char *errorMsg);	/*Simple Error message */
void setParams();		/*Fill the paramType structure */
void RasterToG3D(void *map, G3D_Region region, int *fd);	/*Write the raster */
int open_input_map(char *name, char *mapset);	/*opens the outputmap */
void close_input_map(int fd);	/*close the map */



/* ************************************************************************* */
/* Error handling ********************************************************** */
/* ************************************************************************* */
void fatalError(void *map, int *fd, int depths, char *errorMsg)
{
    int i;

    /* Close files and exit */
    if (map != NULL) {
	/* should unopen map here! but this functionaöity is not jet implementet */
	if (!G3d_closeCell(map))
	    G3d_fatalError(_("Could not close the map"));
    }

    if (fd != NULL) {
	for (i = 0; i < depths; i++)
	    close_input_map(fd[i]);
    }

    G3d_fatalError(errorMsg);
    exit(EXIT_FAILURE);

}

/* ************************************************************************* */
/* Setg up the arguments we are expexting ********************************** */
/* ************************************************************************* */
void setParams()
{
    param.input = G_define_option();
    param.input->key = "input";
    param.input->type = TYPE_STRING;
    param.input->required = YES;
    param.input->description = _("2d raster maps which represent the slices");
    param.input->gisprompt = "old,cell,raster";
    param.input->multiple = YES;

    param.output = G_define_option();
    param.output->key = "output";
    param.output->type = TYPE_STRING;
    param.output->required = YES;
    param.output->gisprompt = "new,grid3,3d-raster";
    param.output->description =
	_("output 3dcell map which will be filled with the 2D raster slices");
}



/* ************************************************************************* */
/* Write the raster maps into one G3D map ********************************** */
/* ************************************************************************* */
void RasterToG3D(void *map, G3D_Region region, int *fd)
{
    int x, y, z;
    int rows, cols, depths, pos = 0;
    void *rast;
    void *ptr;
    float fvalue;
    double dvalue;

    rows = region.rows;
    cols = region.cols;
    depths = region.depths;

    rast = G_allocate_raster_buf(global_rastMapType);

    G_debug(3, _("RasterToG3D: Writing %i raster maps with rows %i cols."),
	    depths, rows, cols);

    /*Every Rastermap */
    for (pos = 0, z = depths - 1; z >= 0; pos++, z--) {	/*From the bottom to the top */
	G_debug(2, _("Writing g3d slice %i\n"), z + 1);
	for (y = 0; y < rows; y++) {
	    G_percent(y, rows - 1, 10);

	    if (!G_get_raster_row(fd[pos], rast, y, global_rastMapType))
		fatalError(map, fd, depths, _("Cold not get raster row \n"));

	    for (x = 0, ptr = rast; x < cols; x++,
		 ptr = G_incr_void_ptr(ptr, G_raster_size(global_rastMapType)))
	    {
		if (global_rastMapType == CELL_TYPE) {
		    if (G_is_null_value(ptr, global_rastMapType)) {
			G3d_setNullValue(&fvalue, 1, G3D_FLOAT);
		    }
		    else {
			fvalue = *(CELL *) ptr;
		    }
		    G3d_putFloat(map, x, y, z, fvalue);
		}
		else if (global_rastMapType == FCELL_TYPE) {
		    if (G_is_null_value(ptr, global_rastMapType)) {
			G3d_setNullValue(&fvalue, 1, G3D_FLOAT);
		    }
		    else {
			fvalue = *(FCELL *) ptr;
		    }
		    G3d_putFloat(map, x, y, z, fvalue);
		}
		else if (global_rastMapType == DCELL_TYPE) {
		    if (G_is_null_value(ptr, global_rastMapType)) {
			G3d_setNullValue(&dvalue, 1, G3D_DOUBLE);
		    }
		    else {
			dvalue = *(DCELL *) ptr;
		    }
		    G3d_putDouble(map, x, y, z, dvalue);
		}

	    }
	}
	G_debug(2, _("\nDone\n"));
    }


    if (rast)
	G_free(rast);

}


/* ************************************************************************* */
/* Main function, open the G3D map and create the raster maps ************** */
/* ************************************************************************* */
int main(int argc, char *argv[])
{
    G3D_Region region;
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

    /* Get parameters from user */
    setParams();

    /* Have GRASS get inputs */
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    /*Check ob Input */
    if (param.output->answer == NULL)
	G3d_fatalError(_("No output map"));

    /* Figure out the region from the map */
    G3d_initDefaults();
    G3d_getWindow(&region);

    /*Check if the g3d-region is equal to the 2d rows and cols */
    rows = G_window_rows();
    cols = G_window_cols();

    if (rows != region.rows || cols != region.cols) {
	fatalError(map, NULL, 0,
		   _
		   ("The resolution of raster and raster3d maps should be equal!"));
    }


    /*prepare the filehandler */
    fd = (int *)G_malloc(region.depths * sizeof(int));

    if (fd == NULL)
	fatalError(map, NULL, 0, _("out of memory!"));

    if (G_legal_filename(param.output->answer) < 0)
	fatalError(map, NULL, 0, _("Illegal output file name"));


    mapset = NULL;
    name = NULL;

    global_rastMapType = DCELL_TYPE;
    global_g3dMapType = G3D_DOUBLE;
    maptype_tmp = DCELL_TYPE;

    opencells = 0;
    /*Loop over all output maps! open */
    for (i = 0; i < region.depths; i++) {
	/*Open only existing maps */
	if (param.input->answers[i] != NULL && nofile == 0) {
	    mapset = NULL;
	    name = NULL;
	    name = param.input->answers[i];
	    mapset = G_find_cell2(name, "");

	    if (mapset == NULL) {
		fatalError(map, fd, opencells, _("Cell file not found\n"));
	    }
	}
	else {
	    nofile = 1;
	}

	/*if only one map is given, open it depths - times */
	G_message(_("Open raster map %s\n"), name);
	fd[i] = open_input_map(name, mapset);
	opencells++;

	/*maptype */
	if (i == 0)
	    global_rastMapType = G_raster_map_type(name, mapset);

	maptype_tmp = G_raster_map_type(name, mapset);

	if (maptype_tmp != global_rastMapType) {
	    fatalError(map, fd, opencells,
		       _
		       ("Input maps have to be from the same type. CELL, FCELL or DCELL!"));
	}
    }

    G_message(_("Creating 3D raster map\n"));
    map = NULL;

    if (global_rastMapType == CELL_TYPE) {
	map =
	    G3d_openCellNew(param.output->answer, G3D_FLOAT,
			    G3D_USE_CACHE_DEFAULT, &region);
	global_g3dMapType = G3D_FLOAT;
    }
    else if (global_rastMapType == FCELL_TYPE) {
	map =
	    G3d_openCellNew(param.output->answer, G3D_FLOAT,
			    G3D_USE_CACHE_DEFAULT, &region);
	global_g3dMapType = G3D_DOUBLE;
    }
    else if (global_rastMapType == DCELL_TYPE) {
	map =
	    G3d_openCellNew(param.output->answer, G3D_DOUBLE,
			    G3D_USE_CACHE_DEFAULT, &region);
	global_g3dMapType = G3D_DOUBLE;
    }

    if (map == NULL)
	fatalError(map, fd, opencells, _("error opening g3d file"));

    /*Create the G3D Rastermap */
    RasterToG3D(map, region, fd);

    /*Loop over all output maps! close */
    for (i = 0; i < region.depths; i++)
	close_input_map(fd[i]);

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
int open_input_map(char *name, char *mapset)
{
    int fd;

    G_debug(3, _("Open Raster file %s in Mapset %s\n"), name, mapset);


    /* open raster file */
    fd = G_open_cell_old(name, mapset);

    if (fd < 0) {
	G_fatal_error(_("Could not open map %s\n"), name);
	exit(EXIT_FAILURE);
    }


    return fd;
}

/* ************************************************************************* */
/* Close the raster output map ********************************************* */
/* ************************************************************************* */
void close_input_map(int fd)
{
    if (G_close_cell(fd) < 0)
	G_fatal_error(_("unable to close input map"));
}

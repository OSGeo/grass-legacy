
/****************************************************************************
*
* MODULE:       r.elev.to.rast3 
*   	    	
* AUTHOR(S):    Original author 
*               Soeren Gebbert soerengebbert@gmx.de
* 		07 08 2006 Berlin
* PURPOSE:      Creates a 3D volume map based on a 2D elevation and value raster map
*
* COPYRIGHT:    (C) 2006 by the GRASS Development Team
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
    struct Option *input, *elev, *output, *upper, *lower;
    struct Flag *fillup, *filllow;
} ParameterType;

/*Data to be used */
typedef struct
{
    void *map;			/*The 3d voxel output map */
    int input;			/*The raster value map */
    int elev;			/*The raster elevation map */
    int inputmaptype;
    int elevmaptype;
    double upper;		/*The upper value */
    double lower;		/*The lower value */
    int useUpperMapVal;		/*0 = use upper value, 1 = use map value to fill upper cells */
    int useLowerMapVal;		/*0 = use lower value, 1 = use map value to fill lower cells */
} Database;

ParameterType Parameter;	/*Parameters */

/*- prototypes --------------------------------------------------------------*/
void FatalError(Database db, char *errorMsg);	/*Simple Error message */
void SetParameters();		/*Fill the ParameterType structure */
void ElevRasterToG3D(Database db, G3D_Region region);	/*Write the raster */
int OpenInputMap(char *name, char *mapset);	/*opens the outputmap */
void CloseInputMap(int fd);	/*close the map */
double GetRasterValueAsDouble(int maptype, void *ptr, double nullval);


/* ************************************************************************* */
/* Get the value of the current raster pointer as double ******************* */
/* ************************************************************************* */
double GetRasterValueAsDouble(int MapType, void *ptr, double nullval)
{
    double val = nullval;

    if (MapType == CELL_TYPE) {
	if (G_is_null_value(ptr, MapType)) {
	    val = nullval;
	}
	else {
	    val = *(CELL *) ptr;
	}
    }
    if (MapType == FCELL_TYPE) {
	if (G_is_null_value(ptr, MapType)) {
	    val = nullval;
	}
	else {
	    val = *(FCELL *) ptr;
	}
    }
    if (MapType == DCELL_TYPE) {
	if (G_is_null_value(ptr, MapType)) {
	    val = nullval;
	}
	else {
	    val = *(DCELL *) ptr;
	}
    }

    return val;
}

/* ************************************************************************* */
/* Open the raster input map *********************************************** */
/* ************************************************************************* */
int OpenInputMap(char *name, char *mapset)
{
    int fd;

    G_debug(3, _("Open Raster file %s in Mapset %s"), name, mapset);

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

/* ************************************************************************* */
/* Error handling ********************************************************** */
/* ************************************************************************* */
void FatalError(Database db, char *errorMsg)
{
    /* Close files and exit */
    if (db.map != NULL) {
	/* should unopen map here! but this functionality is not jet implemented */
	if (!G3d_closeCell(db.map))
	    G3d_fatalError(_("Could not close the map"));
    }

    if (db.input)
	CloseInputMap(db.input);

    if (db.elev)
	CloseInputMap(db.elev);

    G3d_fatalError(errorMsg);
    exit(EXIT_FAILURE);
}

/* ************************************************************************* */
/* Set up the arguments **************************************************** */
/* ************************************************************************* */
void SetParameters()
{
    Parameter.input = G_define_option();
    Parameter.input->key = "input";
    Parameter.input->type = TYPE_STRING;
    Parameter.input->required = YES;
    Parameter.input->description =
	_("2d raster maps which represent the values");
    Parameter.input->gisprompt = "old,cell,raster";
    Parameter.input->multiple = NO;

    Parameter.elev = G_define_option();
    Parameter.elev->key = "elev";
    Parameter.elev->type = TYPE_STRING;
    Parameter.elev->required = YES;
    Parameter.elev->description =
	_("2d raster maps which represent the elevation");
    Parameter.elev->gisprompt = "old,cell,raster";
    Parameter.elev->multiple = NO;

    Parameter.output = G_define_option();
    Parameter.output->key = "output";
    Parameter.output->type = TYPE_STRING;
    Parameter.output->required = YES;
    Parameter.output->gisprompt = "new,grid3,3d-raster";
    Parameter.output->description = _("output 3dcell map");

    Parameter.upper = G_define_option();
    Parameter.upper->key = "upper";
    Parameter.upper->type = TYPE_DOUBLE;
    Parameter.upper->required = NO;
    Parameter.upper->description =
	_("The value to fill the upper cells, default is null");

    Parameter.lower = G_define_option();
    Parameter.lower->key = "lower";
    Parameter.lower->type = TYPE_DOUBLE;
    Parameter.lower->required = NO;
    Parameter.lower->description =
	_("The value to fill the lower cells, default is null");

    Parameter.fillup = G_define_flag();
    Parameter.fillup->key = 'u';
    Parameter.fillup->description =
	_("Use the input map values to fill the upper cells");

    Parameter.filllow = G_define_flag();
    Parameter.filllow->key = 'l';
    Parameter.filllow->description =
	_("Use the input map values to fill the lower cells");

    return;
}

/* ************************************************************************* */
/* Write the raster map into the G3D map *********************************** */
/* ************************************************************************* */
void ElevRasterToG3D(Database db, G3D_Region region)
{
    int x, y, z;
    int rows, cols, depths;
    void *input_rast;
    void *input_ptr;
    void *elev_rast;
    void *elev_ptr;
    double inval, value, null;
    double hight, top, bottom, tbres;

    rows = region.rows;
    cols = region.cols;
    depths = region.depths;
    top = region.top;
    bottom = region.bottom;

    /*Calculate the top-bottom resolution */
    tbres = (top - bottom) / depths;

    /*memory */
    input_rast = G_allocate_raster_buf(db.inputmaptype);
    elev_rast = G_allocate_raster_buf(db.elevmaptype);

    G3d_setNullValue(&null, 1, G3D_DOUBLE);


    G_debug(3,
	    _
	    ("ElevRasterToG3D: Writing 3D raster maps with depths %i rows %i cols %i."),
	    depths, rows, cols);

    /*The mainloop */
    for (y = 0; y < rows; y++) {
	G_percent(y, rows - 1, 10);

	if (!G_get_raster_row(db.input, input_rast, y, db.inputmaptype))
	    FatalError(db, _("Cold not get raster row from input map\n"));
	if (!G_get_raster_row(db.elev, elev_rast, y, db.elevmaptype))
	    FatalError(db, _("Cold not get raster row from elev map\n"));

	for (x = 0, input_ptr = input_rast, elev_ptr = elev_rast; x < cols; x++,
	     input_ptr =
	     G_incr_void_ptr(input_ptr, G_raster_size(db.inputmaptype)),
	     elev_ptr =
	     G_incr_void_ptr(elev_ptr, G_raster_size(db.elevmaptype))) {

	    /*Get the elevation and the input map value */
	    inval = GetRasterValueAsDouble(db.inputmaptype, input_ptr, null);
	    hight = GetRasterValueAsDouble(db.elevmaptype, elev_ptr, null);

	    /*Calculate if the G3D cell is lower or upper the elevation map */
	    for (z = 0; z < depths; z++) {

		/*Upper cells */
		if (hight < (z * tbres + bottom)) {
		    if (db.useUpperMapVal)
			value = inval;	/*Input map value */
		    else
			value = db.upper;
		}
		/*lower cells */
		if (hight > ((z + 1) * tbres + bottom)) {
		    if (db.useLowerMapVal)
			value = inval;	/*Input map value */
		    else
			value = db.lower;
		}
		/*If exactly at the border, fill upper AND lower cell */
		if (hight >= (z * tbres + bottom) &&
		    hight <= ((z + 1) * tbres + bottom))
		    value = inval;
		/*If the elevation is null, set the G3D value null */
		if (G3d_isNullValueNum(&hight, G3D_DOUBLE))
		    value = null;

		/*Write the value to the 3D map */
		if (G3d_putValue(db.map, x, y, z, (char *)&value, G3D_DOUBLE) <
		    0)
		    FatalError(db, _("error writing G3D double data"));
	    }
	}
    }
    G_debug(2, _("\nDone\n"));

    if (input_rast)
	G_free(input_rast);
    if (elev_rast)
	G_free(elev_rast);

    return;
}

/* ************************************************************************* */
/* Main function, open the raster maps and create the G3D raster maps ****** */
/* ************************************************************************* */
int main(int argc, char *argv[])
{
    G3D_Region region;
    struct Cell_head window2d;
    struct GModule *module;
    int cols, rows;
    char *name = NULL;
    char *mapset = NULL;
    Database db;

    /*Initiate the database structure */
    db.map = NULL;
    db.input = 0;
    db.elev = 0;
    db.useUpperMapVal = 0;
    db.useLowerMapVal = 0;

    /* Initialize GRASS */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
	_
	("Creates a 3D volume map based on a 2D elevation and value raster map");

    /* Get Parametereters from user */
    SetParameters();

    /* Have GRASS get inputs */
    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    /*Use the input map value to fill the upper cells */
    if (Parameter.fillup->answer) {
	db.useUpperMapVal = 1;
    }

    /*Use the input map value to fill the lower cells */
    if (Parameter.filllow->answer) {
	db.useLowerMapVal = 1;
    }

    /*Set the upper value */
    if (Parameter.upper->answer) {
	if (sscanf(Parameter.upper->answer, "%lf", &db.upper))
	    db.useUpperMapVal = 0;
	else
	    G_fatal_error(_("The upper value is not valid"));
    }
    else {
	G3d_setNullValue(&db.upper, 1, G3D_DOUBLE);
    }

    /*Set the lower value */
    if (Parameter.lower->answer) {
	if (sscanf(Parameter.lower->answer, "%lf", &db.lower))
	    db.useLowerMapVal = 0;
	else
	    G_fatal_error(_("The lower value is not valid"));
    }
    else {
	G3d_setNullValue(&db.lower, 1, G3D_DOUBLE);
    }

    /* Figure out the region from the map */
    G3d_initDefaults();
    G3d_getWindow(&region);

    /*Check if the g3d-region is equal to the 2d rows and cols */
    rows = G_window_rows();
    cols = G_window_cols();

    /*If not equal, set the 2D windows correct */
    if (rows != region.rows || cols != region.cols) {
	G_message
	    (_
	     ("The 2d and 3d region settings are different. I will use the g3d settings to adjust the 2d region."));
	G_get_set_window(&window2d);
	window2d.ns_res = region.ns_res;
	window2d.ew_res = region.ew_res;
	window2d.rows = region.rows;
	window2d.cols = region.cols;
	G_set_window(&window2d);
    }

    if (G_legal_filename(Parameter.output->answer) < 0)
	FatalError(db, _("Illegal output file name"));

    /*Open input map */
    if (Parameter.input->answer != NULL) {
	mapset = NULL;
	name = NULL;
	name = Parameter.input->answer;
	mapset = G_find_cell2(name, "");
    }
    if (mapset == NULL) {
	FatalError(db, _("input cell file not found\n"));
    }
    db.input = OpenInputMap(name, mapset);
    db.inputmaptype = G_raster_map_type(name, mapset);

    /*Open elev map */
    if (Parameter.elev->answer != NULL) {
	mapset = NULL;
	name = NULL;
	name = Parameter.elev->answer;
	mapset = G_find_cell2(name, "");
    }

    if (mapset == NULL) {
	FatalError(db, _("elev cell file not found\n"));
    }
    db.elev = OpenInputMap(name, mapset);
    db.elevmaptype = G_raster_map_type(name, mapset);

    /*G3D output map */
    G_message(_("Creating 3D raster map\n"));
    db.map = NULL;
    db.map =
	G3d_openCellNew(Parameter.output->answer, G3D_DOUBLE,
			G3D_USE_CACHE_DEFAULT, &region);

    if (db.map == NULL)
	FatalError(db, _("error opening g3d file"));

    /***************************/
    /*Create the G3D Rastermap */
    ElevRasterToG3D(db, region);

    /***************************/

    /* Close files and exit */
    CloseInputMap(db.input);
    CloseInputMap(db.elev);
    if (!G3d_closeCell(db.map))
	G3d_fatalError(_("Error closing g3d file"));

    return (EXIT_SUCCESS);
}

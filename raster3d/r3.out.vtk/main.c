
/****************************************************************************
*
* MODULE:       r3.out.vtk  
*   	    	
* AUTHOR(S):    Original author 
*               Soeren Gebbert soerengebbert@gmx.de
* 		08 01 2005 Berlin
* PURPOSE:      Converts 3D raster maps (G3D) into the VTK-Ascii format  
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


/** Parameters and global variables ******************************************/
typedef struct
{
    struct Option *input, *output, *null_val, *elevscale;
    struct Flag *mask, *point, *origin;
    /*struct Flag *xml; *//*maybe xml support in the future */
} paramType;

void *map = NULL;		/*The 3D Rastermap */
paramType param;		/*Parameters */


/** prototypes ***************************************************************/
void fatalError(char *errorMsg);	/*Simple Error message */
void setParams();		/*Fill the paramType structure */
void writeVTKHeader(FILE * fp, char *vtkFile, G3D_Region region);	/*write the vtk-header */
void G3dTovtk(FILE * fp, G3D_Region region, char *varname);	/*Write the outputdata */


/* ************************************************************************* */
/* Error handling ********************************************************** */
/* ************************************************************************* */
void fatalError(char *errorMsg)
{
    /* Close files and exit */
    if (map != NULL) {
	/* should unopen map here! */
	if (!G3d_closeCell(map))
	    fatalError(_("Error closing g3d file"));

    }

    G3d_fatalError(errorMsg);
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
    param.input->gisprompt = "old,grid3,3d-raster";
    param.input->multiple = YES;	/* is this correct ?? ... Yes, you can put serveral maps into
					 * one vtk file */
    param.input->description =
	_("3dcell map(s) to be converted to VTK-ASCII data format");

    param.output = G_define_option();
    param.output->key = "output";
    param.output->type = TYPE_STRING;
    param.output->gisprompt = "file,file,file";
    param.output->required = NO;
    param.output->description = _("Name for VTK-ASCII output file");

    param.null_val = G_define_option();
    param.null_val->key = "null";
    param.null_val->type = TYPE_DOUBLE;
    param.null_val->required = NO;
    param.null_val->description = _("Float value to represent no data cell");
    param.null_val->answer = "-99999.99";

    param.elevscale = G_define_option();
    param.elevscale->key = "elevscale";
    param.elevscale->type = TYPE_DOUBLE;
    param.elevscale->required = NO;
    param.elevscale->description = _("Scale factor for elevation");
    param.elevscale->answer = "1.0";

    param.mask = G_define_flag();
    param.mask->key = 'm';
    param.mask->description = _("Use G3D mask (if exists) with input maps");

    param.point = G_define_flag();
    param.point->key = 'p';
    param.point->description =
	_("Create VTK pointdata instead of VTK celldata (celldata is default)");

    param.origin = G_define_flag();
    param.origin->key = 'o';
    param.origin->description = _("Scalefactor effects the origin");



    /* Maybe needed in the future
     * param.xml = G_define_flag ();
     * param.xml->key = 'x';
     * param.xml->description = "Write XML-VTK-format";
     */
}

/* ************************************************************************* */
/* Writes the Header ******************************************************* */
/* ************************************************************************* */
void writeVTKHeader(FILE * fp, char *vtkFile, G3D_Region region)
{
    double scale;

    scale = atof(param.elevscale->answer);
    G_debug(3, _("writeVTKHeader: Writing VTK-Header"));

    /*Simple vtk ASCII header */

    fprintf(fp, "# vtk DataFile Version 3.0\n");
    fprintf(fp, "GRASS 6 Export\n");
    fprintf(fp, "ASCII\n");
    fprintf(fp, "DATASET STRUCTURED_POINTS\n");	/*We are using the structured point dataset. */

    if (param.point->answer)
	fprintf(fp, "DIMENSIONS %i %i %i\n", region.cols, region.rows,
		region.depths);
    else
	fprintf(fp, "DIMENSIONS %i %i %i\n", region.cols + 1, region.rows + 1,
		region.depths + 1);

    fprintf(fp, "SPACING %g %g %g\n", region.ew_res, region.ns_res,
	    (region.tb_res * scale));

    if (param.point->answer) {
	if (param.origin->answer)
	    fprintf(fp, "ORIGIN %g %g %g\n", region.west + region.ew_res / 2,
		    region.south + region.ns_res / 2,
		    region.bottom * scale + (region.tb_res * scale) / 2);
	else
	    fprintf(fp, "ORIGIN %g %g %g\n", region.west + region.ew_res / 2,
		    region.south + region.ns_res / 2,
		    region.bottom + (region.tb_res * scale) / 2);
    }
    else {
	if (param.origin->answer)
	    fprintf(fp, "ORIGIN %g %g %g\n", region.west, region.south,
		    region.bottom * scale);
	else
	    fprintf(fp, "ORIGIN %g %g %g\n", region.west, region.south,
		    region.bottom);
    }

    if (param.point->answer)
	fprintf(fp, "POINT_DATA %i\n", region.cols * region.rows * region.depths);	/*We have pointdata */
    else
	fprintf(fp, "CELL_DATA %i\n", region.cols * region.rows * region.depths);	/*We have celldata */
}

/* ************************************************************************* */
/* This function does all the work. **************************************** */
/* ************************************************************************* */
void G3dTovtk(FILE * fp, G3D_Region region, char *varname)
{
    double d1 = 0;
    double *d1p;
    float *f1p;
    int x, y, z, status;
    int rows, cols, depths, typeIntern;

    rows = region.rows;
    cols = region.cols;
    depths = region.depths;


    G_debug(3,
	    _
	    ("G3dTovtk: Writing Celldata %s with rows %i cols %i depths %i to vtk-ascii file"),
	    varname, rows, cols, depths);

    fprintf(fp, "SCALARS %s float 1\n", varname);
    fprintf(fp, "LOOKUP_TABLE default\n");

    typeIntern = G3d_tileTypeMap(map);

    d1p = &d1;
    f1p = (float *)&d1;

    status = 0;

    for (z = depths - 1; z >= 0; z--) {	/*From the bottom to the top */
	for (y = rows - 1; y >= 0; y--) {	/*From south to the north */
	    G_percent(status, (rows * depths - 1), 10);
	    status++;

	    for (x = 0; x < cols; x++) {
		G3d_getValue(map, x, y, z, d1p, typeIntern);
		if (typeIntern == G3D_FLOAT) {
		    if (G3d_isNullValueNum(f1p, G3D_FLOAT))
			fprintf(fp, "%s ", param.null_val->answer);
		    else
			fprintf(fp, "%g ", *f1p);
		}
		else {
		    if (G3d_isNullValueNum(&d1, G3D_DOUBLE))
			fprintf(fp, "%s ", param.null_val->answer);
		    else
			fprintf(fp, "%g ", d1);
		}
	    }
	    fprintf(fp, "\n");
	}
    }
}

/* ************************************************************************* */
/* Main function, opens the input and output files, then call G3dtovtk. **** */
/* ************************************************************************* */
int main(int argc, char *argv[])
{
    char *output = NULL;
    G3D_Region region;
    FILE *fp = NULL;
    struct GModule *module;
    int i, changemask = 0;

    /* Initialize GRASS */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
	_("Converts 3D raster maps (G3D) into the VTK-Ascii format");

    /* Get parameters from user */
    setParams();

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


    /*Loop over all input maps! */
    for (i = 0; param.input->answers[i] != NULL; i++) {

	G_debug(3, _("Open 3DRaster file %s"), param.input->answers[i]);

	if (NULL == G_find_grid3(param.input->answers[i], ""))
	    G3d_fatalError(_("Requested g3d file not found"));


	/*Open the map */
	map =
	    G3d_openCellOld(param.input->answers[i],
			    G_find_grid3(param.input->answers[i], ""),
			    G3D_DEFAULT_WINDOW, G3D_TILE_SAME_AS_FILE,
			    G3D_USE_CACHE_DEFAULT);
	if (map == NULL)
	    G3d_fatalError(_("Error opening g3d file"));

	/*if requested set the Mask on */
	if (param.mask->answer) {
	    if (G3d_maskFileExists()) {
		changemask = 0;
		if (G3d_maskIsOff(map)) {
		    G3d_maskOn(map);
		    changemask = 1;
		}
	    }
	}

	/* Figure out the region from the map */
	if (i == 0)
	    G3d_getWindow(&region);

	/* Write the vtk-header */
	if (i == 0)
	    writeVTKHeader(fp, output, region);

	/* Now barf out the contents of the map in vtk form */
	G3dTovtk(fp, region, param.input->answers[i]);

	/*We set the Mask off, if it was off before */
	if (param.mask->answer) {
	    if (G3d_maskFileExists())
		if (G3d_maskIsOn(map) && changemask)
		    G3d_maskOff(map);
	}

	/* Close files and exit */
	if (!G3d_closeCell(map))
	    fatalError(_("Error closing g3d file"));

	map = NULL;
    }


    if (param.output->answer && fp != NULL)
	if (fclose(fp))
	    fatalError(_("Error closing VTK-ASCII file"));

    return 0;
}

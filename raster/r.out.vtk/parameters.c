
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
#include <grass/gis.h>
#include <grass/glocale.h>
#include "parameters.h"



/* ************************************************************************* */
/* PARAMETERS ************************************************************** */
/* ************************************************************************* */

void SetParameters()
{
    param.input = G_define_option();
    param.input->key = "input";
    param.input->type = TYPE_STRING;
    param.input->required = NO;
    param.input->gisprompt = "old,cell,raster";
    param.input->multiple = YES;
    param.input->description =
	_("Raster map(s) to be converted to VTK-ASCII data format");

    param.elevationmap = G_define_option();
    param.elevationmap->key = "elevation";
    param.elevationmap->type = TYPE_STRING;
    param.elevationmap->required = NO;
    param.elevationmap->gisprompt = "old,cell,raster";
    param.elevationmap->multiple = NO;
    param.elevationmap->description =
	_
	("Raster map that represents the elevation, used for the 3D information");

    param.rgbmaps = G_define_option();
    param.rgbmaps->key = "rgbmaps";
    param.rgbmaps->type = TYPE_STRING;
    param.rgbmaps->required = NO;
    param.rgbmaps->gisprompt = "old,cell,raster";
    param.rgbmaps->multiple = YES;
    param.rgbmaps->description =
	_
	("3 raster maps (r,g,b) which are used to create rgb values [redmap,greenmap,bluemap]");


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
    param.null_val->description = _("Value to represent no data cell");
    param.null_val->answer = "-10.0";

    param.elevscale = G_define_option();
    param.elevscale->key = "elevscale";
    param.elevscale->type = TYPE_DOUBLE;
    param.elevscale->required = NO;
    param.elevscale->description = _("Scale factor for elevation");
    param.elevscale->answer = "1.0";

    param.elev = G_define_option();
    param.elev->key = "elevation2d";
    param.elev->type = TYPE_DOUBLE;
    param.elev->required = NO;
    param.elev->description = _("Elevation (if no elevation map is given)");
    param.elev->answer = "0.0";

    param.usestruct = G_define_flag();
    param.usestruct->key = 's';
    param.usestruct->description =
	_("Use structured grid for elevation (not recommended)");

    param.usetriangle = G_define_flag();
    param.usetriangle->key = 't';
    param.usetriangle->description =
	_("Use polydata-trianglestrips for elevation grid creation");

    param.usevertices = G_define_flag();
    param.usevertices->key = 'v';
    param.usevertices->description =
	_
	("Use polydata-vertices for elevation grid creation (to use with vtkDelauny2D)");

    param.origin = G_define_flag();
    param.origin->key = 'o';
    param.origin->description =
	_("Scale factor effects the origin (if no elevation map is given)");

    param.point = G_define_flag();
    param.point->key = 'p';
    param.point->description =
	_
	("Create VTK point data instead of VTK cell data (if no elevation map is given)");


    /* 
     * param.mask = G_define_flag ();
     * param.mask->key = 'm';
     * param.mask->description = _("Use mask (if exists) with input maps");
     * 
     * Maybe needed in the future
     * param.xml = G_define_flag ();
     * param.xml->key = 'x';
     * param.xml->description = "Write XML-VTK-format";
     */
}

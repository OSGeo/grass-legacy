
/****************************************************************************
*
* MODULE:       r3.out.vtk  
*   	    	
* AUTHOR(S):    Original author 
*               Soeren Gebbert soerengebbert at gmx de
* 		27 Feb 2006 Berlin
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
#include "globalDefs.h"
#include "errorHandling.h"


/* ************************************************************************* */
/* Error handling ********************************************************** */
/* ************************************************************************* */
void fatalError(char *errorMsg, inputMaps * in)
{
    G_warning("%s\n", errorMsg);

    /*close all open maps and free memory */
    ReleaseInputMapsStruct(in);

    G3d_fatalError("Break because of errors.");
}

/* ************************************************************************* */
/* Close the raster input map ********************************************** */
/* ************************************************************************* */
int CloseInputRasterMap(int fd)
{
    if (fd != -1)
	if (G_close_cell(fd) < 0) {
	    G_warning(_("unable to close input raster map\n"));
	    return 1;
	}

    return 0;

}

/* ************************************************************************* */
/* Close the raster g3d input map ****************************************** */
/* ************************************************************************* */
int CloseInputRaster3dMap(void *map)
{
    if (map != NULL) {
	if (!G3d_closeCell(map)) {
	    G_warning(_("unable to close input g3d raster map\n"));
	    return 1;
	}
    }
    map = NULL;

    return 0;

}

/* ************************************************************************* */
/* Close alls open raster and g3d maps and free memory ********************* */
/* ************************************************************************* */
void ReleaseInputMapsStruct(inputMaps * in)
{
    int error = 0;		/*0 == true, 1 = false */
    int i;

    error += CloseInputRaster3dMap(in->map);
    error += CloseInputRaster3dMap(in->map_r);
    error += CloseInputRaster3dMap(in->map_g);
    error += CloseInputRaster3dMap(in->map_b);
    error += CloseInputRaster3dMap(in->map_x);
    error += CloseInputRaster3dMap(in->map_y);
    error += CloseInputRaster3dMap(in->map_z);

    error += CloseInputRasterMap(in->top);
    error += CloseInputRasterMap(in->bottom);

    for (i = 0; i < in->numelevmaps; i++) {
	if (in->elevmaps && in->elevmaps[i])
	    error += CloseInputRasterMap(in->elevmaps[i]);
    }

    if (in->elevmaps)
	free(in->elevmaps);

    free(in);

    if (error > 0)
	G3d_fatalError("Error while closing the input maps");

    return;
}

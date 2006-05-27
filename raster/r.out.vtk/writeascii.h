
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

#ifndef __R_OUT_VTK_WRITEASCII_H__
#define __R_OUT_VTK_WRITEASCII_H__

struct Cell_head;		/*Definition needed here */

void writeVTKNormalHeader(FILE * fp, struct Cell_head region, double elevation,
			  int type);
void writeVTKStructuredElevationHeader(FILE * fp, struct Cell_head region);
void writeVTKPolygonalElevationHeader(FILE * fp, struct Cell_head region);
void writeVTKCellDataHeader(FILE * fp, struct Cell_head region);
void writeVTKPointDataHeader(FILE * fp, struct Cell_head region);
void writeVTKData(int fd, FILE * fp, char *varname, struct Cell_head region,
		  int out_type, char *null_value);
void writeVTKRGBImageData(int redfd, int greenfd, int bluefd, FILE * fp,
			  const char *varname, struct Cell_head region,
			  int out_type);
void writeVTKVectorData(int xfd, int yfd, int zfd, FILE * fp,
			const char *varname, struct Cell_head region,
			int out_type);
void writeVTKStructuredCoordinates(int fd, FILE * fp, char *varname,
				   struct Cell_head region, int out_type,
				   char *null_value, double scale);
void writeVTKPolygonalCoordinates(int fd, FILE * fp, char *varname,
				  struct Cell_head region, int out_type,
				  char *null_value, double scale, int polytype);
#endif


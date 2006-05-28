
 /***************************************************************************
 *
 * MODULE:     v.out.vtk  
 * AUTHOR(S):  Soeren Gebbert
 *
 * PURPOSE:    v.out.vtk: writes ASCII VTK file
 *             this module is based on v.out.ascii
 * COPYRIGHT:  (C) 2000 by the GRASS Development Team
 *
 *             This program is free software under the GNU General Public
 *              License (>=v2). Read the file COPYING that comes with GRASS
 *              for details.
 *
 ****************************************************************************/

#ifndef __V_OUT_VTK_WRITE_VTK__
#define __V_OUT_VTK_WRITE_VTK__

/*the information for every vector type */
typedef struct
{
    int numpoints;		/*The number of points */
    int pointoffset;
    int numvertices;		/*the number of vertices */
    int verticesoffset;
    int numlines;		/*The number of lines */
    int lineoffset;
    int numpolygons;		/*The number of polygons */
    int polygonoffset;
    int generatedata;		/*If 0 the categorie data is not complete */
} VTKTypeInfo;

/*essential vtk file information */
typedef struct
{
    int maxnumpoints;		/*The max number of points */
    int maxnumvertices;		/*the max number of vertices */
    int maxnumlines;		/*The max number of lines */
    int maxnumlinepoints;	/*The max number of line points */
    int maxnumpolygons;		/*The max number of polygons */
    int maxnumpolygonpoints;	/*The max number of polygon points */
    VTKTypeInfo **typeinfo;	/*The info struct for every vector type */
} VTKInfo;



/*Writes the point cooridanets for every vector type */
int writeVTKPoints(FILE * ascii, struct Map_info *Map, VTKInfo * info,
		   int *type, int typenum, int dp);
/*Writes the polydata cells for every vector type */
int writeVTKCells(FILE * ascii, struct Map_info *Map, VTKInfo * info,
		  int *type, int typenum);
/*Write the category (the first available) as cell or point data */
int writeVTKCatData(FILE * ascii, struct Map_info *Map, VTKInfo * info,
		    int layer, int *type, int typenum, int dp);
/*If a database connection is given, write the db data as cell or point data */
int writeVTKDBData(FILE * ascii, struct Map_info *Map, VTKInfo * info,
		   int layer, int *type, int typenum, int dp);
#endif

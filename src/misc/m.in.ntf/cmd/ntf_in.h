/************************************************************************/
/*** 								      ***/
/***			       ntf_in.h			   	      ***/
/***  Header file for use with m.in.ntf - 			      ***/
/***  Jo Wood, ASSIST, Dept of Geography, University of Leicester     ***/
/***  V1.1  - 19th May, 1993				     	      ***/
/***								      ***/
/************************************************************************/

#include "gis.h" 	/* This MUST be included in all GRASS programs	*/
			/* It sets up the necessary prototypes for all	*/
			/* GRASS library calls.				*/

#include "Vect.h"       /* Must be included in programs that manipulate */
                        /* vectors.                                     */

#include "ntf.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRUE 1
#define FALSE 0
#define NO_ATTRIB -1

#define MODE_READ  0
#define MODE_WRITE 1
#define MODE_RW    2

#define TEXT_CODE 9999


/* Map Types */

#define CONTOUR 1
#define DEM 2
#define LANDLINE 3
#define BOUNDARYLINE 4
#define OSCAR 5
#define ONE250 6
#define ONE625 7
#define STRATEGI 8
#define TEXT_LABEL 9999
#define DONT_KNOW -1


/* ------ Global variables ------ */

#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

char		*ntf_in_name,	/* Name of the raster file to process.	*/
    		*file_out_name,	/* Name of the raster output file.	*/
		*mapset_out,	/* GRASS mapset holding output file.	*/
		conversion_log,	/* Determines if conversion log printed.*/
		outfile,	/* Determines if an outfile is created.	*/
		nodes,		/* Determines if nodes are transferred.	*/
		O_raster,	/* Determines if raster is already open	*/
		O_vector,	/* Determines if vector is already open	*/
		O_temp,
		O_spot,		/* Flags that determine whether files	*/
		O_cont,		/*   are already open.			*/
		O_lake,		/*   Include:	Spot Heights, Contours,	*/
		O_break,	/* 	    	Lakes, Breaklines,	*/
		O_coast,	/*	    	Coastlines, Ridgelines,	*/
				/*	    	and Form Lines.		*/
		O_form,
		O_other,	/* Any other type of vector coverage.	*/
	        text[128],	/* Stores a line (record) of NTF file.	*/
		H_organ[30],	/* Organisation supplying data.		*/
		H_ddate[11],	/* Digitisation date			*/
		H_mname[40],	/* Title of map.			*/
		H_mdate[11],	/* Date of map survey.			*/
		V_name[40],	/* Vector object name.			*/
		Write_vect;	/* Flag to defermine if vector written	*/

#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

float		XY_mult,	/* X, Y, and Z Scaling factors.		*/
		Z_mult;


#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

FILE		*ntf_fptr,	/* File descriptor for input and	*/
		*out_fptr,	/* output raster files.			*/
		*new_fptr,	/* New converted NTF 2.0 file.		*/

		*fptr_spot,	/* File descriptor for vector files.	*/
		*fptr_spot_att,
		*fptr_cont,
		*fptr_cont_att,
		*fptr_lake,
		*fptr_lake_att,
		*fptr_break,
		*fptr_break_att,
		*fptr_coast,
		*fptr_coast_att,
		*fptr_ridge,
		*fptr_ridge_att,
		*fptr_form,
		*fptr_form_att,
		*fptr_other,
		*fptr_other_att;


#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

int		line_number,	/* Current line of NTF file being read.	*/
		fd_out,		/* Raster output file descriptor.	*/
		num_rlines,	/* Number of raster lines read.		*/
		X_min,Y_min,	/* Boundaries of vector map.		*/
		X_max,Y_max,
		X_origin,	/* SW corner of vector map.		*/
		Y_origin,
		V_featcode,	/* Feature code of vector map.		*/
		H_scale,	/* Original scale of map data.		*/
		geom_type;	/* Point line or area type of vector	*/



#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

struct Map_info  vect_info;	/* Structure to hold vector information	*/

#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

struct line_pnts *points;	/* Holds x,y coordinates of vector.	*/

#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

struct Categories cats,		/* Stores vector categories.		*/
		  feature_desc;	/* Temporary structure for storing	*/
				/* feature descriptions.		*/


#ifndef MAIN
    extern			/* Externally defined if not main()	*/
#endif

CELL		raster[401][401];
				/* Array holding entire raster tile.	*/

   

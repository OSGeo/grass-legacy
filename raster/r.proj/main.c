/*
* Name
* 	r.proj -- convert a map to a new geographic projection.
*
* Usage:
*	r.proj input=name location=name [output=name] [mapset=name]
*		  [dbase=name] [method=name]
*
*
* Description:
*	r.proj converts a map to a new geographic projection. It reads a
*	map from a different location, projects it and write it out
*	to the current location. The projected data is resampled with
*	one of three different methods: nearest neighbor, bilinear and
*	cubic convolution.
*
*
* Parameters:
*
*	input   	input raster map
*	location   	location of input map
*	output		output raster map
*	mapset		mapset of input map
*	dbase		database of input map
*	method		interpolation method to use
*
*
*
* Author:
*		 Martin Schroeder
*		 University of Heidelberg
*		 Dept. of Geography
*		 emes@geo0.geog.uni-heidelberg.de
*
* 		 (With the help of a lot of existing GRASS sources, in 
*		  particular v.proj) 
*
* Changes
*		 Morten Hulden <morten@ngb.se>, Aug 2000:
*		 - aborts if input map is outside current location.
*		 - can handle projections (conic, azimuthal etc) where 
*		 part of the map may fall into areas where south is 
*		 upward and east is leftward.
*		 - avoids passing location edge coordinates to PROJ
*		 (they may be invalid in some projections).
*		 - output map will be clipped to borders of the current region.
*		 - output map cell edges and centers will coinside with those 
*		 of the current region.
*		 - output map resolution (unless changed explicitly) will
*		 match (exactly) the resolution of the current region.
*		 - if the input map is smaller than the current region, the 
*		 output map will only cover the overlapping area.
*                - if the input map is larger than the current region, only the
*		 needed amount of memory will be allocated for the projection
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "projects.h"
#include "r.proj.h"

int main (int argc, char **argv)
{
	char     *mapname,		 /* ptr to name of output layer	 */
	         *setname,		 /* ptr to name of input mapset	 */
	         *ipolname,		 /* name of interpolation method */
	          errbuf[256];		 /* buffer for error messages	 */

	int       fdi,			 /* input map file descriptor	 */
	          fdo,			 /* output map file descriptor	 */
	          method,		 /* position of method in table	 */
	          permissions,		 /* mapset permissions		 */
	          cell_type,		 /* output celltype		 */
	          cell_size,		 /* size of a cell in bytes	 */
	          row, col, i,		 /* counters			 */
		  irows, icols,		 /* original rows, cols		 */
		  orows, ocols;

	void     *obuffer,		 /* buffer that holds one output row	 */
	         *obufptr;		 /* column ptr in output buffer	 */
	FCELL   **ibuffer;		 /* buffer that holds the input map	 */
	func      interpolate;		 /* interpolation routine	 */

	double    xcoord1, xcoord2,	 /* temporary x coordinates 	 */
	          ycoord1, ycoord2,	 /* temporary y coordinates	 */
	          col_idx,		 /* column index in input matrix */
	          row_idx,		 /* row index in input matrix	 */
		  onorth, osouth,	 /* save original border coords  */
		  oeast, owest,
		  inorth, isouth,
		  ieast, iwest;

	struct pj_info iproj,		 /* input map proj parameters	 */
	          oproj;		 /* output map proj parameters	 */

	struct Key_Value *in_proj_info,	 /* projection information of 	 */
	         *in_unit_info,		 /* input and output mapsets	 */
	         *out_proj_info, 
	         *out_unit_info;

	struct GModule *module;

	struct Option *imapset,		 /* name of input mapset	 */
	         *inmap,		 /* name of input layer		 */
	         *outmap,		 /* name of output layer	 */
	         *inlocation,		 /* name of input location	 */
	         *indbase,		 /* name of input database	 */
	         *interpol,		 /* interpolation method:	 
 					    nearest neighbor, bilinear, cubic */
		 *res;			 /* resolution of target map     */
		  struct Cell_head incellhd,	 /* cell header of input map	 */
	          outcellhd;		 /* and output map		 */




	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Projects raster maps between two projections.";

	inmap = G_define_option();
	inmap->key = "input";
	inmap->type = TYPE_STRING;
	inmap->required = YES;
	inmap->description = "input raster map";

	outmap = G_define_option();
	outmap->key = "output";
	outmap->type = TYPE_STRING;
	outmap->required = NO;
	outmap->description = "output raster map";

	imapset = G_define_option();
	imapset->key = "mapset";
	imapset->type = TYPE_STRING;
	imapset->required = NO;
	imapset->description = "mapset of input map";

	inlocation = G_define_option();
	inlocation->key = "location";
	inlocation->type = TYPE_STRING;
	inlocation->required = YES;
	inlocation->description = "location of input map";

	indbase = G_define_option();
	indbase->key = "dbase";
	indbase->type = TYPE_STRING;
	indbase->required = NO;
	indbase->description = "database of input map";

	interpol = G_define_option();
	interpol->key = "method";
	interpol->type = TYPE_STRING;
	interpol->required = NO;
	interpol->answer = "nearest";
	interpol->options = ipolname = (char *) G_malloc(1024);
	
	for (i = 0; menu[i].name != 0; i++) {
		if (i)
			strcat(ipolname, ",");
		else
			*ipolname = 0;
		strcat(ipolname, menu[i].name);
	}
	interpol->description = "interpolation method to use";


	res = G_define_option();
	res->key = "resolution";
	res->type = TYPE_DOUBLE;
	res->required = NO;
	res->description = "resolution of output map";



	if (G_parser(argc, argv))
		exit(-1);

   /* get the method */
	for (method = 0; ipolname = menu[method].name; method++)
		if (strcmp(ipolname, interpol->answer) == 0)
			break;

	if (!ipolname) {
		fprintf(stderr, "<%s=%s> unknown %s\n",
			interpol->key, interpol->answer, interpol->key);
		G_usage();
		exit(1);
	}
	interpolate = menu[method].method;

	if (outmap->answer)
		mapname = outmap->answer;
	else
		mapname = inmap->answer;

	if (imapset->answer)
		setname = imapset->answer;
	else
		setname = G_store(G_mapset());

	if (strcmp(inlocation->answer, G_location()) == 0)
		G_fatal_error("You have to use a different location for input than the current");

	G_get_window(&outcellhd);


   /* Get projection info for output mapset */
	if ((out_proj_info = G_get_projinfo()) == NULL)
		G_fatal_error("Can't get projection info of output map");

	if ((out_unit_info = G_get_projunits()) == NULL)
		G_fatal_error("Can't get projection units of output map");

	if (pj_get_kv(&oproj, out_proj_info, out_unit_info) < 0)
		G_fatal_error("Can't get projection key values of output map");



   /* Change the location 		 */
	G__create_alt_env();
	G__setenv("GISDBASE", indbase->answer == NULL
		  ? G_gisdbase()
		  : indbase->answer);
	G__setenv("LOCATION_NAME", inlocation->answer);
	permissions = G__mapset_permissions(setname);

	if (permissions >= 0) {

		if (!G_find_cell(inmap->answer, setname)) {
			sprintf(errbuf, "Input map [%s] in mapset [%s] not found.",
				inmap->answer, setname);
			G_fatal_error(errbuf);
		}
   /* Get projection info for input mapset */
		if ((in_proj_info = G_get_projinfo()) == NULL)
			G_fatal_error("Can't get projection info of input map");

		if ((in_unit_info = G_get_projunits()) == NULL)
			G_fatal_error("Can't get projection units of input map");

		if (pj_get_kv(&iproj, in_proj_info, in_unit_info) < 0)
			G_fatal_error("Can't get projection key values of input map");


		G_get_cellhd(inmap->answer, setname, &incellhd);

		G_set_window(&incellhd);
		cell_type = G_raster_map_type(inmap->answer, setname);

		if (!G_projection())	/* XY data 		 */
			G_fatal_error("Can't work with xy data");

	} else {		/* can't access mapset 	 */

		sprintf(errbuf, "Mapset [%s] in input location [%s] - ",
			setname, inlocation->answer);

		strcat(errbuf, permissions == 0
		       ? "permission denied\n"
		       : "not found\n");
		G_fatal_error(errbuf);
	}

    /* Save default borders so we can show them later */

	inorth = incellhd.north;
	isouth = incellhd.south;
	ieast = incellhd.east;
	iwest = incellhd.west;
	irows = incellhd.rows;
	icols = incellhd.cols;

	onorth = outcellhd.north;
	osouth = outcellhd.south;
	oeast = outcellhd.east;
	owest = outcellhd.west;
	orows = outcellhd.rows;
	ocols = outcellhd.cols;

    /* Cut non-overlapping parts of input map */

	if (bordwalk(&outcellhd, &incellhd, &oproj, &iproj, errbuf) < 0)
	    G_fatal_error(errbuf);

    /* Add 2 cells on each side for bilinear/cubic & future interpolation methods */
    /* (should probably be a factor based on input and output resolution) */
        
	    incellhd.north+=2*incellhd.ns_res;
	    incellhd.east+=2*incellhd.ew_res;
	    incellhd.south-=2*incellhd.ns_res;
	    incellhd.west-=2*incellhd.ew_res;
	    if (incellhd.north > inorth) incellhd.north=inorth;  
	    if (incellhd.east > ieast) incellhd.east=ieast;  
	    if (incellhd.south < isouth) incellhd.south=isouth;  
	    if (incellhd.west < iwest) incellhd.west=iwest;  

	G_set_window(&incellhd);

   /* And switch back to original location */

	G__switch_env();

    /* Adjust borders of output map */

	if (bordwalk(&incellhd, &outcellhd, &iproj, &oproj, errbuf) < 0)
	    G_fatal_error(errbuf);

	if (res->answer != NULL)   /* set user defined resolution		*/
		outcellhd.ns_res = outcellhd.ew_res = atof(res->answer);

	G_adjust_Cell_head(&outcellhd, 0, 0);
	G_set_window(&outcellhd);

	fprintf(stderr, "Input:\n");
	fprintf(stderr, "Cols:	%d (%d)\nRows:	%d (%d)\n", incellhd.cols, icols, incellhd.rows, irows);
	fprintf(stderr, "North: %f (%f)\n", incellhd.north, inorth);
	fprintf(stderr, "South: %f (%f)\n", incellhd.south, isouth);
	fprintf(stderr, "West:  %f (%f)\n", incellhd.west, iwest);
	fprintf(stderr, "East:  %f (%f)\n", incellhd.east, ieast);
	fprintf(stderr, "ew-res:	%f\nns-res	%f\n\n", incellhd.ew_res, incellhd.ns_res);

	fprintf(stderr, "Output:\n");
	fprintf(stderr, "Cols:	%d (%d)\nRows:	%d (%d)\n", outcellhd.cols, ocols, outcellhd.rows, orows);
	fprintf(stderr, "North: %f (%f)\n", outcellhd.north, onorth);
	fprintf(stderr, "South: %f (%f)\n", outcellhd.south, osouth);
	fprintf(stderr, "West:  %f (%f)\n", outcellhd.west, owest);
	fprintf(stderr, "East:  %f (%f)\n", outcellhd.east, oeast);
	fprintf(stderr, "ew-res:	%f\nns-res	%f\n", outcellhd.ew_res, outcellhd.ns_res);

	G__switch_env();
	
   /* open and read the relevant parts of the input map and close it */
	
   	G_set_window(&incellhd);
		fdi = G_open_cell_old(inmap->answer, setname);
		ibuffer = (FCELL **) readcell(fdi);
		G_close_cell(fdi);
	G__switch_env();
	G_set_window(&outcellhd);
		
	if (strcmp(interpol->answer, "nearest") == 0) {
		fdo = G_open_raster_new(mapname, cell_type);
		obuffer = (CELL *) G_allocate_raster_buf(cell_type);
	} else {
		fdo = G_open_fp_cell_new(mapname);
		cell_type = FCELL_TYPE;
		obuffer = (FCELL *) G_allocate_raster_buf(cell_type);
	}

	cell_size = G_raster_size(cell_type);

	xcoord1 = xcoord2 = outcellhd.west + (outcellhd.ew_res / 2);	/**/
	ycoord1 = ycoord2 = outcellhd.north - (outcellhd.ns_res / 2);	/**/



	fprintf(stderr, "Projecting... ");
	G_percent(0, outcellhd.rows, 2);
	for (row = 0; row < outcellhd.rows; row++) {

		obufptr = obuffer;

		for (col = 0; col < outcellhd.cols; col++) {
   /* project coordinates in output matrix to		 */
   /* coordinates in input matrix			 */
			if (pj_do_proj(&xcoord1, &ycoord1, &oproj, &iproj) < 0)
				G_fatal_error("Error in pj_do_proj\n");

   /* convert to row/column indices of input matrix	 */
			col_idx = (xcoord1 - incellhd.west) / incellhd.ew_res;
			row_idx = (incellhd.north - ycoord1) / incellhd.ns_res;

   /* and resample data point				 */

			interpolate(ibuffer, obufptr, cell_type,
				    &col_idx, &row_idx, &incellhd);

			obufptr = G_incr_void_ptr(obufptr, cell_size);
			xcoord2 += outcellhd.ew_res;
			xcoord1 = xcoord2;
			ycoord1 = ycoord2;
		}
		if (G_put_raster_row(fdo, obuffer, cell_type) < 0)
			exit(-1);

		xcoord1 = xcoord2 = outcellhd.west;
		ycoord2 -= outcellhd.ns_res;
		ycoord1 = ycoord2;
		G_percent(row, outcellhd.rows - 1, 2);
	}
	G_close_cell(fdo);
	return(0);		/* OK */
}

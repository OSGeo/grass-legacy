/*
 * $Id$
 */

/* -*-c-basic-offset: 4;-*- */
/************************************************************************ 
*                                                                       *
*      This is the main program for tracing out the path that a         *
*      drop of water would take if released at a certain location       *
*      on an input elevation map.  The program was written by           *
*      Kewan Q. Khawaja                                                 *
*      kewan@techlogix.com                                              *
*                                                                       *
* update to FP (2000): Pierre de Mouveaux <pmx@audiovu.com><pmx@free.fr>*
* bugfix in FCELL, DCELL: Markus Neteler 12/2000                        *
*************************************************************************/

/* uncomment this to get debug messages */
/*
#define DEBUG
*/

#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "segment.h"
#include "gis.h"
#define MAIN
#include "stash.h"

#define POINT           struct point
#define PRES_PT_SLOPE   PRES_PT->slope
#define PRES_PT_ROW     PRES_PT->row
#define PRES_PT_COL     PRES_PT->column
#define NEXT_PT         PRES_PT->next
#define FIRST_PT_SLOPE  head->slope
#define SECOND_PT       head->next
#define NEW_ROW         NEW_START_PT->row
#define NEW_COL         NEW_START_PT->column
#define NEW_NEXT        NEW_START_PT->next
#define NEXT_START_PT   PRESENT_PT->next

#include "drain_cmd.h"

struct metrics {
	double ew_res, ns_res, diag_res;
};



struct Cell_head window;
int nrows, ncols;
SEGMENT in_seg, out_seg;
int data_type, data_type2;
int exclude_nulls = 1;
double null_value = 0.0;
int mode=0;
int cum = 0;
float fcum =0.0;
double dcum = 0.0;
double count = 0.0 ;
int count2 = 0;

struct metrics* m = NULL;

int 
main (int argc, char *argv[])
{
	int n ;
	int col, row, 
	len, len2, flag,
	srows, scols,
	elevation_fd, drain_path_fd,
	in_fd, out_fd;
	char *current_mapset,
	*search_mapset,
	*drain_path_mapset, 
	*elevation_mapset,
	*in_file, *out_file,
	buf[400];
	void *cell;
	POINT *PRES_PT=NULL, *NEW_START_PT, *PRESENT_PT=NULL;
	double east, north;
	struct GModule *module;
	struct Option *opt1, *opt2, *opt3, *opt4, *opt5;
	struct Flag *flag1, *flag2, *flag3;

	module = G_define_module();
	module->description =
		"Traces a flow through an elevation model on a raster map layer.";

	opt2 = G_define_option() ;
	opt2->key        = "input" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = YES ;
	opt2->gisprompt  = "old,cell,raster" ;
	opt2->description= "Name of raster map containing cost elevation information";

	opt1 = G_define_option() ;
	opt1->key        = "output" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "any,cell,raster" ;
	opt1->description= "Name of output raster map" ;

	opt3 = G_define_option() ;
	opt3->key        = "coordinate" ;
	opt3->type       = TYPE_STRING ;
	opt3->multiple   = YES;
	opt3->key_desc   = "x,y" ;
	opt3->description= "The map E and N grid coordinates of a starting point";

	opt4 = G_define_option() ;
	opt4->key        = "sites" ;
	opt4->type       = TYPE_STRING;
	opt4->gisprompt       = "old,site_list,sites";
	opt4->required   = NO;
	opt4->description= "Starting points site file";

	opt5 = G_define_option() ;
	opt5->key        = "null_value" ;
	opt5->type       = TYPE_DOUBLE;
	opt5->key_desc   = "null cells value" ;
	opt5->required   = NO;
	opt5->multiple   = NO;
/*  	opt5->answer     = ""; */
	opt5->description= "Value assigned to null cells. Null cells are excluded by default";

	flag1 = G_define_flag();
	flag1->key = 'c';
	flag1->description = "Copy input cell values on output";

	flag2 = G_define_flag();
	flag2->key = 'a';
	flag2->description = "Accumulate input values along the path";

	flag3 = G_define_flag();
	flag3->key = 'n';
	flag3->description = "Count cell numbers along the path";

	G_gisinit (argv[0]);

	current_mapset = G_mapset();
	in_file = G_tempfile();
	out_file = G_tempfile();

	/*  Get database window parameters      */
	if(G_get_window (&window) < 0)
	{
		sprintf (buf,"can't read current window parameters");
		G_fatal_error (buf);
		exit(1);
	}

	/*   Do command line parsing	*/

	if (G_parser(argc, argv))
		exit(-1);
	
	if ((opt5->answer != NULL) && (sscanf(opt5->answer, "%lf", &null_value) == 1))	{
		exclude_nulls = 0;
		printf("\nnull %f\n",null_value);
	} 

	if (opt3->answer) 
	{  
	    for(n=0; opt3->answers[n] != NULL; n+=2)
	    {
		G_scan_easting  (opt3->answers[n  ], &east, G_projection()) ;
		G_scan_northing (opt3->answers[n+1], &north, G_projection()) ;
		col = (int)G_easting_to_col(east, &window);
		row = (int)G_northing_to_row(north, &window );

		NEW_START_PT = (POINT *) (malloc (sizeof (POINT)));

		NEW_ROW = row;
		NEW_COL = col;
		NEW_NEXT= NULL;

		if (head_start_pt == NULL)
		{
			head_start_pt = NEW_START_PT;
			PRESENT_PT = head_start_pt;
		}
	    	else 
		{
			NEXT_START_PT = NEW_START_PT;
			PRESENT_PT = NEXT_START_PT ;
			/*return(0); quote this out to accept multi-starters,
			 -modified by Jianping Xu*/
	    	}
	    }
	}

	if (opt4->answer) 
	{  
#if 1
		FILE* fp;
		Site *site = NULL;               /* pointer to Site */
		search_mapset = "";
 
		search_mapset = G_find_file ("site_lists", opt4->answer, "");

		fp = G_fopen_sites_old ( opt4->answer, search_mapset);

		site = G_site_new_struct (-1, 2, 0, 0);

	    for (; (G_site_get(fp,site) != EOF);) {
			if (!G_site_in_region (site, &window))
				continue;
			
/*  		G_scan_easting  (opt3->answers[n  ], &east, G_projection()) ; */
/*  		G_scan_northing (opt3->answers[n+1], &north, G_projection()) ; */
			col = (int)G_easting_to_col(site->east, &window);
			row = (int)G_northing_to_row(site->north, &window );

			NEW_START_PT = (POINT *) (malloc (sizeof (POINT)));

			NEW_ROW = row;
			NEW_COL = col;
			NEW_NEXT= NULL;

			if (head_start_pt == NULL)
			{
				head_start_pt = NEW_START_PT;
				PRESENT_PT = head_start_pt;
			}
			else 
			{
				NEXT_START_PT = NEW_START_PT;
				PRESENT_PT = NEXT_START_PT ;
				/*return(0); quote this out to accept multi-starters,
				  -modified by Jianping Xu*/
		    	}
		}
		G_site_free_struct(site);	
		fclose(fp);
#endif
	}

	if (flag1->answer)
		mode = 1;
	if (flag2->answer) {
		if (mode)
			G_warning("Both -c and -a flags specified! r.drain will use the -a flag");
		mode = 2;
	}
	if (flag3->answer && flag2->answer && flag1->answer) {
		sprintf(buf, "Don't specify -n with other flags!");
		G_fatal_error (buf);
	}
	if (flag3->answer && (flag2->answer || flag1->answer)) {
		sprintf(buf, "Don't specify -n with other flags!");
		G_fatal_error (buf);
	}
	if (flag3->answer) {
		mode = 3;
	}


/*  Check if elevation layer exists in data base  */
	search_mapset = "";

	strcpy(elevation_layer, opt2->answer);

	elevation_mapset = G_find_cell 
	    (elevation_layer, search_mapset);

	if (elevation_mapset == NULL)
	{
		sprintf(buf, "%s - not found", elevation_layer);
		G_fatal_error (buf);
		exit(1);
	}

	search_mapset = "";

	strcpy (drain_path_layer, opt1->answer);

	drain_path_mapset = G_find_cell 
	    (drain_path_layer, search_mapset);

	/*  Set flag according to input */
	if (drain_path_mapset != NULL)
	{
		if (head_start_pt == NULL)
			/*output layer exists and start pts are not given on cmd line*/
			flag = 1;

			/* output layer exists and starting pts are given on cmd line*/
		else flag = 2;
	}
	else
		flag = 3;    /* output layer does not previously exist */


	/*  Check if specified output layer name is legal   */
	if (flag == 3)
	{
		if (G_legal_filename (drain_path_layer) < 0)
		{
			sprintf(buf, "%s - illegal name", drain_path_layer);
			G_fatal_error (buf);
			exit(1);
		}
	}

	/*  find number of rows and columns in window    */
	nrows = G_window_rows();
	ncols = G_window_cols();

	m = (struct metrics*) malloc(nrows*sizeof(struct metrics));
	
	if (m==NULL)
		G_fatal_error("Metrics allocation");

	G_begin_distance_calculations();
	{
		double e1,n1,e2,n2;
		e1=window.east;
		n1=window.north;
		e2=e1+window.ew_res;
		n2=n1-window.ns_res;
		for (n=0;n<nrows;n++) {
			m[n].ew_res = G_distance(e1,n1,e2,n1);
			m[n].ns_res = G_distance(e1,n1,e1,n2);
			m[n].diag_res = G_distance(e1,n1,e2,n2);
			e2=e1+window.ew_res;
			n2=n1-window.ns_res;
		}
	}

	/*  Open elevation cell layer for reading  */
	elevation_fd = G_open_cell_old
	    (elevation_layer, elevation_mapset);

	if (elevation_fd < 0)
	{
		sprintf (buf, "%s - can't open raster file", elevation_layer);
		G_fatal_error (buf);
		exit(1);
	}

	/* input */
	data_type = G_raster_map_type(elevation_layer, elevation_mapset);
	cell = G_allocate_raster_buf(data_type); 
	len = G_raster_size(data_type);

#ifdef DEBUG
fprintf(stderr,"Mode type: %i\n", mode);
#endif
	/* output */
        data_type2 = data_type;  /* would be better to store CELL_TYPE when mode == 3 */
	len2 = G_raster_size(data_type2);

/*   Parameters for map submatrices   */
#ifdef DEBUG
	if (1) {
		switch (data_type) {
			case (CELL_TYPE):
				fprintf(stderr,"Source map is: Integer cell type,");
			break;
			case (FCELL_TYPE):
				fprintf(stderr,"Source map is: Floating point (float) cell type,");
			break;
			case (DCELL_TYPE):
				fprintf(stderr,"Source map is: Floating point (double) cell type,");
			break;
		}
			fprintf(stderr," %d rows, %d cols.\n", nrows, ncols);
	}
#endif
	/*   Parameters for map submatrices   */

	srows = nrows/4 + 1;
	scols = ncols/4 + 1;

	/* Create segmented files for elev and output layers  */
	in_fd = creat(in_file,0666);
	segment_format(in_fd, nrows, ncols, srows, scols, len);
	close(in_fd);

	out_fd = creat(out_file,0666);
	segment_format(out_fd, nrows, ncols, srows, scols, len2);
	close(out_fd);

	/*   Open initialize and segment all files  */
	in_fd = open(in_file,2);
	segment_init(&in_seg,in_fd,4);

	out_fd = open(out_file,2);
	segment_init(&out_seg,out_fd,4);

	/*   Write the elevation layer in the segmented file  */
	for( row=0 ; row<nrows ; row++ )
	{
		if(G_get_raster_row(elevation_fd, cell, row,data_type)<0)
			exit(1);
		segment_put_row(&in_seg, cell, row);
	}
	segment_flush(&in_seg);

/*	G_close_cell(elevation_fd); */

	G_free(cell);

	cell = G_allocate_raster_buf(data_type2); 
	G_set_null_value(cell,ncols,data_type2);
	for( row=0 ; row<nrows ; row++ )
	{
		segment_put_row(&out_seg, cell, row);
	}
	segment_flush(&out_seg);

	/* If the output layer containing the marked starting positions*/
	/* already exists, create a linked list of starting locations  */
	if (flag == 1) {
		int data_type3;
		int data_size3;
		void* cell3;

		drain_path_fd = G_open_cell_old (drain_path_layer,
		    drain_path_mapset);
		data_type3 = G_raster_map_type(drain_path_layer,drain_path_mapset);
		data_size3 = G_raster_size(data_type3);

		if (drain_path_fd < 0)
		{
			sprintf (buf, "%s -can't open raster file", drain_path_layer);
			G_fatal_error (buf);
			exit(1);
		}

		cell3 = G_allocate_raster_buf(data_type3); 

		/*  Search for the marked starting pts and make list	*/
		for(row = 0; row < nrows; row++)
		{
			if(G_get_raster_row(drain_path_fd,cell,row, data_type3) < 0)
				exit(1);

			for(col = 0; col < ncols; col++)
			{
				if(!G_is_null_value(cell3,data_type3)) {
					POINT *new;
					G_incr_void_ptr(cell3,data_size3);
					new = make_point((POINT *)NULL,row,col,0.0);
					if(head_start_pt == NULL)
						head_start_pt = new;
					else
						NEXT_PT = new;
					PRES_PT = new;
				}
			}	/* loop over cols */
		}	/* loop over rows */

		printf("free--------------\n");
		G_free(cell3);
		printf("free--------------\n");
		G_close_cell(drain_path_fd);
	}

	/*  check if all the starting pts are inside database window */
	if(flag != 1)
	{
		PRES_PT = head_start_pt;
		while (PRES_PT != NULL)
		{
			if (PRES_PT_ROW < 0 || PRES_PT_ROW >= nrows
			    || PRES_PT_COL < 0 || PRES_PT_COL >= ncols)
			{
				sprintf(buf," Starting location outside window");
				G_fatal_error(buf);
			}
			PRES_PT = NEXT_PT;
		}
	}

	/* loop over the starting points to find the drainage paths  */
	PRES_PT = head_start_pt;
	while(PRES_PT != NULL)
	{
		if (mode == 2 || mode == 3) {
			cum = 0;
			fcum= 0.0;
			dcum = 0.0;
		}
		drain_path_finder (PRES_PT);
		PRES_PT = NEXT_PT;
	}


	/*  Open output layer for writing   */
	drain_path_fd = G_open_raster_new(drain_path_layer,data_type2);

	/*  Write pending updates by segment_put() to outputmap   */
	segment_flush(&in_seg);
	segment_flush(&out_seg);

	for ( row=0 ; row<nrows; row++ )
	{
		segment_get_row(&out_seg,cell,row);

		if (G_put_raster_row(drain_path_fd,cell,data_type2)<0)
			exit(1);
	}

	segment_release(&in_seg);   /* release memory  */
	segment_release(&out_seg);

	close(in_fd);               /* close all files */
	close(out_fd);

  	G_close_cell(drain_path_fd); 
	G_close_cell(elevation_fd);

	unlink(in_file);       /* remove submatrix files  */
	unlink(out_file);

	exit(0);
}

/*************** END OF "MAIN" PROGRAM *************************/



/***************************************************************

	This recursive function traces the drainage path
	of a drop of water on an elevation map

****************************************************************/

int drain_path_finder ( POINT *PRES_PT)

{
	POINT *head = NULL, *make_neighbors_list();

	{ /* start a new block to minimize variable use in recursion */
		int row, col, val, val2;
		double p_elev, dval, fdata, ddummy;
		float f, fval, f_elev, fdummy;


		switch (data_type) {
			case (CELL_TYPE):
				segment_get(&out_seg, &val2, PRES_PT_ROW, PRES_PT_COL);
				if(!G_is_c_null_value(&val2)) 
					return 0;		/* already traversed	*/
				segment_get(&in_seg, &val, PRES_PT_ROW, PRES_PT_COL);
				p_elev = val;
#ifdef DEBUG
fprintf(stderr, "p_elev: %g\n", p_elev);
#endif
				/* mode: flag */
				switch (mode){
					case 0: {
						val2 = 1;
						segment_put(&out_seg, &val2, PRES_PT_ROW, PRES_PT_COL); /* (pmx - for Markus 20 april 2000 */
						break;
					}
					case 1: {
						segment_put(&out_seg, &val, PRES_PT_ROW, PRES_PT_COL); /* (pmx - for Markus 20 april 2000 */
						break;
					}
					case 2: {
						cum+=val;
						segment_put(&out_seg, &cum, PRES_PT_ROW, PRES_PT_COL); /* (pmx - for Markus 20 april 2000 */
						break;
					}
					case 3: {                  /* added MN 12/2000 */
						count2+=1;
						segment_put(&out_seg, &count2, PRES_PT_ROW, PRES_PT_COL);
						break;
					}
				}						
					
				/* check the elevations of neighbouring pts to determine the	*/
				/* next pt(s) for the drop to flow				*/
				for (row = PRES_PT_ROW -1;
					 row <= (PRES_PT_ROW +1) && row < nrows; row++)
				{
					if (row < 0) continue;

					for (col = PRES_PT_COL -1;
						 col <= (PRES_PT_COL +1)  && col < ncols; col++)
					{
						if (col < 0) continue;
						if (row == PRES_PT_ROW && col == PRES_PT_COL) continue;

						segment_get(&in_seg, &val, row, col);
						if (G_is_c_null_value(&val)) {
							if (exclude_nulls) {
								continue;
							} else {
								fdata = null_value;
							} 
						} else {
							fdata = val;
						}
				/* elev of neighbor is higher. i.e. no chance of flow	*/
						if (fdata > p_elev)
								continue;

						/* if elev of neighbor is equal or lower consider for	*/
						/* addition to the list of pts where water will flow	*/
						head = make_neighbors_list(head, row, col, fdata,
												   PRES_PT_ROW, PRES_PT_COL, p_elev);

					}	/* end of "col" loop */
				}	/* end of "row" loop */
				break;
			case (FCELL_TYPE):
				segment_get(&out_seg, &fval, PRES_PT_ROW, PRES_PT_COL);
#ifdef DEBUG
fprintf(stderr, "fval: %g\n", fval);
#endif
				if(!G_is_f_null_value(&fval) ) 
					return 0;		/* already traversed	*/
				segment_get(&in_seg, &f, PRES_PT_ROW, PRES_PT_COL);
				f_elev = f;
#ifdef DEBUG
fprintf(stderr, "PRES_PT_ROW: %i - PRES_PT_COL: %i - ", PRES_PT_ROW, PRES_PT_COL);
fprintf(stderr, "f: %g\n", f);
#endif
				
				/* check the elevations of neighbouring pts to determine the	*/
				/* next pt(s) for the drop to flow				*/
				switch (mode){
					case 0:
						dval = 1;
						segment_put(&out_seg, &fval, PRES_PT_ROW, PRES_PT_COL); /* (pmx - for Markus 20 april 2000 */
						break;
					case 1:
						segment_put(&out_seg, &f, PRES_PT_ROW, PRES_PT_COL); /* (pmx - for Markus 20 april 2000 */
						break;
					case 2:
						dcum+=f;
						segment_put(&out_seg, &fcum, PRES_PT_ROW, PRES_PT_COL); /* (pmx - for Markus 20 april 2000 */
						break;
					case 3: {                  /* added MN 12/2000 */
						count+=1;
						segment_put(&out_seg, &count, PRES_PT_ROW, PRES_PT_COL);
						break;
					}

				}						
				for (row = PRES_PT_ROW -1;
					 row <= (PRES_PT_ROW +1) && row < nrows; row++)
				{
					if (row < 0) continue;

					for (col = PRES_PT_COL -1;
						 col <= (PRES_PT_COL +1)  && col < ncols; col++)
					{
						if (col < 0) continue;
						if (row == PRES_PT_ROW && col == PRES_PT_COL) continue;

						segment_get(&in_seg, &f, row, col);
						if (G_is_f_null_value(&f)) {
							if (exclude_nulls) {
								continue;
							} else {
								fdummy = null_value;
							} 
						}  else {
							 fdummy = f;
						}

#ifdef DEBUG
fprintf(stderr, "PRES_PT_ROW: %i - PRES_PT_COL: %i - ", PRES_PT_ROW, PRES_PT_COL);
fprintf(stderr, "fdummy: %g - f_elev: %g\n", fdummy, f_elev);
#endif
							/* elev of neighbor is higher. i.e. no chance of flow	*/
							if(fdummy > f_elev) continue;

						/* if elev of neighbor is equal or lower consider for	*/
						/* addition to the list of pts where water will flow	*/
						head = make_neighbors_list(head, row, col, fdummy,
												   PRES_PT_ROW, PRES_PT_COL, p_elev);

					}	/* end of "col" loop */
				}	/* end of "row" loop */
				break;
			case (DCELL_TYPE):
				segment_get(&out_seg, &dval, PRES_PT_ROW, PRES_PT_COL);
#ifdef DEBUG
fprintf(stderr, "dval: %g\n", dval);
#endif
				if(!G_is_d_null_value(&dval) ) 
					return 0;		/* already traversed	*/
				segment_get(&in_seg, &fdata, PRES_PT_ROW, PRES_PT_COL);
				p_elev = fdata;
#ifdef DEBUG
fprintf(stderr, "fdata: %g\n", fdata);
#endif
				
				/* check the elevations of neighbouring pts to determine the	*/
				/* next pt(s) for the drop to flow				*/
				switch (mode){
					case 0:
						dval = 1;
						segment_put(&out_seg, &dval, PRES_PT_ROW, PRES_PT_COL); /* (pmx - for Markus 20 april 2000 */
						break;
					case 1:
						segment_put(&out_seg, &fdata, PRES_PT_ROW, PRES_PT_COL); /* (pmx - for Markus 20 april 2000 */
						break;
					case 2:
						dcum+=fdata;
						segment_put(&out_seg, &dcum, PRES_PT_ROW, PRES_PT_COL); /* (pmx - for Markus 20 april 2000 */
						break;
					case 3: {                  /* added MN 12/2000 */
						count+=1;
						segment_put(&out_seg, &count, PRES_PT_ROW, PRES_PT_COL);
						break;
					}

				}						
				for (row = PRES_PT_ROW -1;
					 row <= (PRES_PT_ROW +1) && row < nrows; row++)
				{
					if (row < 0) continue;

					for (col = PRES_PT_COL -1;
						 col <= (PRES_PT_COL +1)  && col < ncols; col++)
					{
						if (col < 0) continue;
						if (row == PRES_PT_ROW && col == PRES_PT_COL) continue;

						segment_get(&in_seg, &fdata, row, col);
						if (G_is_d_null_value(&fdata)) {
							if (exclude_nulls) {
								continue;
							} else {
								ddummy = null_value;
							} 
						}  else {
							 ddummy = fdata;
						}

#ifdef DEBUG
fprintf(stderr, "PRES_PT_ROW: %i - PRES_PT_COL: %i - ", PRES_PT_ROW, PRES_PT_COL); 
fprintf(stderr, "ddummy: %g - p_elev: %g\n", ddummy, p_elev);
#endif							/* elev of neighbor is higher. i.e. no chance of flow	*/
							if(ddummy > p_elev) continue;

						/* if elev of neighbor is equal or lower consider for	*/
						/* addition to the list of pts where water will flow	*/
						head = make_neighbors_list(head, row, col, ddummy,
												   PRES_PT_ROW, PRES_PT_COL, p_elev);

					}	/* end of "col" loop */
				}	/* end of "row" loop */
				break;
		}



		if(head == NULL) return 0;	/* lowest pt reached */
	}


	/* process the list of neighboring pts where the water will flow*/
	PRES_PT = head;
	while(PRES_PT != NULL)
	{
		drain_path_finder(PRES_PT);
		PRES_PT = NEXT_PT;
	}

	free_list(head);	/* free list of neighboring pts*/

	return 0;
}

/********* END OF FUNCTION "DRAIN_PATH_FINDER" ******************/



/****************************************************************

	This function makes a list of pts neighboring to the
 	one under consideration where the water	drop would flow
	next.

****************************************************************/

POINT *
make_neighbors_list (POINT *head, int row, int col, double data, int p_row, int p_col, double p_elev)
{
	POINT *make_point();
	double dist, slope, sqrt(), atan();
#if 0	

	dist = sqrt((((row - p_row) * window.ns_res) *
	    ((row - p_row) * window.ns_res))
	    + (((col - p_col) * window.ew_res) *
	    ((col - p_col) * window.ew_res)));
#else								/* (pmx) true distance , relevant whith lat-lon projections and large maps */
	if ((p_row!=row)&&(p_col!=col))
		dist = m[row].diag_res;
	else if (p_row==row)
		dist = m[row].ew_res;
	else
		dist = m[row].ns_res;
#endif

	slope = atan((data - p_elev)/dist);

	if(head == NULL)	/* first pt	*/
	{
		head = make_point(head,row, col, slope);
	}
	else	/* otherwise	*/
	{	/*----------------------------------------------*/
		if(slope < FIRST_PT_SLOPE)
		/* this pt is at a steeper slope than those in list	*/
		{
			/* start new list				*/
			free_list(head);
			head = make_point((POINT *) NULL,row, col, 
			    slope);
		}

		/* if pt at same slope as those already in list, add to list */
		else if(slope == FIRST_PT_SLOPE)
		{
			POINT *PRES_PT;
			PRES_PT = make_point((POINT *) NULL, row, col, slope);
			NEXT_PT = head;
			head = PRES_PT;
		}
	}	/*----------------------------------------------*/

	return(head);

}

/************ END OF FUNCTION "MAKE_NEIGHBORS_LIST"**************/



/****************************************************************

	This function creates a new point data structure using
	memory from the malloc pool and initializes all fields

****************************************************************/

POINT *
make_point (POINT *PRES_PT, int row, int col, double slope)
{
	if (PRES_PT == NULL)
		PRES_PT = (POINT *) malloc(sizeof (POINT));

	PRES_PT_ROW = row;
	PRES_PT_COL = col;
	PRES_PT_SLOPE = slope;
	NEXT_PT = NULL;

	return(PRES_PT);
}

/**************** END OF FUNCTION "MAKE_POINT" ******************/



/****************************************************************

	This function frees the memory allocated to a pt list

*****************************************************************/

int 
free_list (POINT *head)
{
	POINT *PRES_PT;

	PRES_PT = head;
	while(PRES_PT != NULL)
	{
		PRES_PT = SECOND_PT;
		free(head);
		head = PRES_PT;
	}

	return 0;
}


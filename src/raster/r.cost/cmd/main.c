/*********************************************************************
 *
 *     This is the main program for the minimum path cost analysis.
 *     It generates a cumulative cost map (output) from an elevation
 *     or cost map (input) with repsect to starting locations (coor).
 *
 *     It takes as input the following:
 *     1) Cost of traversing each grid cell as given by a cost map
 *        cell (input).
 *     2) If starting points are not specified on the command line
 *        then the ouput map must exist and contain the starting locations
 *
 *        Otherwise the ouput map need not exist and the coor points
 *        from the command line are used.
 *
 *********************************************************************/

/* 08 april 2000 - Pierre de Mouveaux. pmx@audiovu.com
   Updated to use the Grass 5.0 floating point raster cell format.
   TODO: convert floats to double. Done ;)
*/

#define MAIN

#define SEGCOLSIZE 	256

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "gis.h"
#include "site.h"
#include "segment.h"
#include "cost.h"
#include "stash.h"
#include "local_proto.h"


struct Cell_head window;

int main (int argc, char *argv[])
{
	void *cell, *cell2;
	SEGMENT in_seg, out_seg;
	char *cost_mapset ;
	char *cum_cost_mapset ;
	char *current_mapset ;
	char *in_file, *out_file ;
	char *search_mapset ;
	double* value;
	char buf[400];
	extern struct Cell_head window;
	double NS_fac,EW_fac,DIAG_fac,H_DIAG_fac,V_DIAG_fac ;
	double fcost ;
	double min_cost,old_min_cost ;
	double neg = -2.0 ;
	double zero = 0.0 ;
	int at_percent = 0;
	int col, row, nrows, ncols ;
	int maxcost ;
	double cost ;
	int cost_fd, cum_fd ;
	int have_start_points ;
	int have_stop_points ;
	int in_fd, out_fd ;
	double my_cost ;
	double null_cost;
	int srows, scols ;
	int total_reviewed ;
	int verbose = 1 ;
	int keep_nulls = 1 ;
	int neighbor ;
	int segments_in_memory ;
	long n_processed = 0;
	long total_cells ;
	struct GModule *module;
	struct Flag *flag1, *flag2, *flag3;
	struct Option *opt1, *opt2, *opt3, *opt4, *opt5, *opt6, *opt7, *opt8;
	struct cost *pres_cell, *new_cell;
	struct start_pt *pres_start_pt = NULL ;
	struct start_pt *pres_stop_pt = NULL;

	void* ptr2;
	RASTER_MAP_TYPE data_type, data_type2;
	double peak = 0.0;
	int dsize;

	module = G_define_module();
	module->description =
		"Outputs a raster map layer showing the "
		"cumulative cost of moving between different "
		"geographic locations on an input raster map "
		"layer whose cell category values represent cost.";
							
	opt2 = G_define_option() ;
	opt2->key        = "input" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = YES ;
	opt2->gisprompt  = "old,cell,raster" ;
	opt2->description= "Name of raster map containing grid cell cost information" ;

	opt1 = G_define_option() ;
	opt1->key        = "output" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "any,cell,raster" ;
	opt1->description= "Name of raster map to contain results" ;

	opt7 = G_define_option() ;
	opt7->key        = "start_sites" ;
	opt7->type       = TYPE_STRING;
	opt7->gisprompt  = "old,site_lists,sites";
	opt7->required   = NO;
	opt7->description= "Starting points site file";

	opt8 = G_define_option() ;
	opt8->key        = "stop_sites" ;
	opt8->type       = TYPE_STRING;
	opt8->gisprompt  = "old,site_lists,sites";
	opt8->required   = NO;
	opt8->description= "Stop points site file";

	opt3 = G_define_option() ;
	opt3->key        = "coordinate" ;
	opt3->type       = TYPE_STRING ;
	opt3->key_desc   = "x,y" ;
	opt3->multiple   = YES;
	opt3->description= "The map E and N grid coordinates of a starting point,coor=E,N" ;

	opt4 = G_define_option() ;
	opt4->key        = "stop_coordinate" ;
	opt4->type       = TYPE_STRING ;
	opt4->key_desc   = "x,y" ;
	opt4->multiple   = YES;
	opt4->description= "The map E and N grid coordinates of a stoping point,coor=E,N" ;

	opt5 = G_define_option() ;
	opt5->key        = "max_cost" ;
	opt5->type       = TYPE_INTEGER;
	opt5->key_desc   = "cost" ;
	opt5->required   = NO;
	opt5->multiple   = NO;
	opt5->answer     = "0";
	opt5->description= "An optional maximum cumulative cost";

	opt6 = G_define_option() ;
	opt6->key        = "null_cost" ;
	opt6->type       = TYPE_DOUBLE;
	opt6->key_desc   = "null cost" ;
	opt6->required   = NO;
	opt6->multiple   = NO;
/*  	opt6->answer     = ""; */
	opt6->description= "Cost assigned to null cells. By defaults, null cells are excluded";

	flag1 = G_define_flag();
	flag1->key = 'v';
	flag1->description = "Run verbosly";

	flag2 = G_define_flag();
	flag2->key = 'k';
	flag2->description = "Use the 'Knight's move'; slower, but more accurate";

	flag3 = G_define_flag();
	flag3->key = 'n';
	flag3->description = "Keep null values in output map";

	/* initalize access to database and create temporary files */

	G_gisinit (argv[0]);

	in_file = G_tempfile();
	out_file = G_tempfile();

	/*  Get database window parameters      */

	if (G_get_window(&window) < 0)
	{
		sprintf (buf,"can't read current window parameters");
		G_fatal_error (buf);
		exit(1);
	}

	/*  Find north-south, east_west and diagonal factors */

	/*
	  NS_fac = window.ns_res*dist_fac;
	  EW_fac = window.ew_res*dist_fac;
	  DIAG_fac = (double)sqrt((double)(NS_fac*NS_fac + EW_fac*EW_fac));
	*/
	EW_fac = 1.0 ;
	NS_fac = window.ns_res/window.ew_res ;
	DIAG_fac = (double)sqrt((double)(NS_fac*NS_fac + EW_fac*EW_fac));
	V_DIAG_fac = (double)sqrt((double)(4*NS_fac*NS_fac + EW_fac*EW_fac)); 
	H_DIAG_fac = (double)sqrt((double)(NS_fac*NS_fac + 4*EW_fac*EW_fac)); 

	G_set_d_null_value(&null_cost,1);
	/*   Parse command line */

	if (G_parser(argc, argv))
		exit(-1);

	verbose = flag1->answer;
	if (flag2->answer)
		total_reviewed =  16 ;
	else
		total_reviewed =  8 ;

	keep_nulls = flag3->answer;


	have_start_points = process_answers(opt3->answers, &head_start_pt, &pres_start_pt) ;

	have_stop_points  = process_answers(opt4->answers, &head_end_pt, &pres_stop_pt) ;

	if (sscanf(opt5->answer, "%d", &maxcost) != 1 || maxcost < 0)
	{
		sprintf(buf, "Inappropriate maximum cost: %d", maxcost);
		G_fatal_error (buf) ;
		exit(1) ;
	}
	
 
	if ((opt6->answer == NULL) ||(sscanf(opt6->answer, "%lf", &null_cost) != 1))
	{
		if (verbose)
			fprintf(stderr,"Null cells excluded from cost evaluation.\n");
		G_set_d_null_value(&null_cost,1);
	} 
	else if (verbose && keep_nulls)
			fprintf(stderr,"Input null cell will be retained into output map\n");


	if(!G_is_d_null_value(&null_cost)) {
		if (null_cost <0.0) {
			printf("Warning: assigning negative cost to null cell. Null cells excluded.\n"); 
			G_set_d_null_value(&null_cost,1);
		}
	} else {
		keep_nulls = 0; /* handled automagically... */
	}

	strcpy (cum_cost_layer, opt1->answer);
	current_mapset = G_mapset();

	/*  Search for output layer (cum_cost_layer) in all mapsets */

	search_mapset = "";
	cum_cost_mapset = G_find_cell2 (cum_cost_layer, search_mapset);
	

	/*  Check if cost layer exists in data base  */

	strcpy(cost_layer, opt2->answer);
	cost_mapset = G_find_cell2 (cost_layer, search_mapset);

	if (cost_mapset == NULL)
	{
		sprintf(buf, "%s - not found", cost_layer);
		G_fatal_error (buf);
		exit(1);
	}

	/*  Check if specified output layer name is legal   */

	if (G_legal_filename (cum_cost_layer) < 0)
	{
		sprintf(buf, "%s - illegal name", cum_cost_layer);
		G_fatal_error (buf);
		exit(1);
	}

	/*  find number of rows and columns in window    */

	nrows = G_window_rows();
	ncols = G_window_cols();


	/*  Open cost cell layer for reading  */

	cost_fd = G_open_cell_old(cost_layer, cost_mapset);

	if (cost_fd < 0)
	{
		sprintf (buf, "%s - can't open raster file", cost_layer);
		G_fatal_error (buf);
		exit(1);
	}

	data_type = G_raster_map_type(cost_layer,cost_mapset);
/*  	printf("Map type: %d\n",data_type); */
/*    	cell = G_malloc(ncols* G_raster_size(data_type));  */
	cell = G_allocate_raster_buf(data_type); 
	/*   Parameters for map submatrices   */

	if (verbose) {
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

/*
  srows =  nrows/12 + 1;
  scols =  ncols/12 + 1;
*/
	srows = scols = SEGCOLSIZE ;
	segments_in_memory = 4 * (nrows/SEGCOLSIZE + ncols/SEGCOLSIZE + 2) ;

	/*   Create segmented format files for cost layer and output layer  */

	if (verbose)
	{
		fprintf (stderr, "Creating some temporary files ...");
		fflush (stderr);
	}

	in_fd = creat(in_file,0666);
	segment_format(in_fd, nrows, ncols, srows, scols, sizeof(double));
	close(in_fd);

	out_fd = creat(out_file,0666);
	segment_format(out_fd, nrows, ncols, srows, scols, sizeof(double));
	close(out_fd);

	/*   Open initialize and segment all files  */

	in_fd = open(in_file,2);
	segment_init(&in_seg,in_fd,segments_in_memory);


	out_fd = open(out_file,2);
	segment_init(&out_seg,out_fd,segments_in_memory);

	/*   Write the cost layer in the segmented file  */

	if (verbose)
	{
		fprintf (stderr, "\n");
		fprintf (stderr, "Reading %s ...", cost_layer);
	}

	{
		int i;
		double p;
		
		dsize = G_raster_size(data_type);
		p=0.0;
	
		for( row=0 ; row<nrows ; row++ )
		{
			if (verbose)
				G_percent (row, nrows, 2);
			if(G_get_raster_row(cost_fd, cell, row, data_type)<0)
				exit(1);
/*  		segment_put_row(&in_seg, cell, row); */

/* INPUT NULL VALUES: ??? */
			ptr2 = cell;
			switch (data_type) {
				case CELL_TYPE:		
					for(i=0;i<ncols;i++) {
						if (G_is_null_value(ptr2,data_type)) {
  							p = null_cost;
						} else {
							p=*(int*)ptr2;
						}
						segment_put(&in_seg, &p,row,i);
						ptr2 = G_incr_void_ptr(ptr2,dsize);
					}
					break;
				case FCELL_TYPE:
					for(i=0;i<ncols;i++) {
						if (G_is_null_value(ptr2,data_type)) {
  							p = null_cost;
						} else {
							p = *(float*)ptr2;
						}
						segment_put(&in_seg, &p,row,i);
						ptr2 = G_incr_void_ptr(ptr2,dsize);
					}
					break;
				
				case DCELL_TYPE:
					for(i=0;i<ncols;i++) {
						if (G_is_null_value(ptr2,data_type)) {
  							p = null_cost;
						} else {
							p = *(double*)ptr2;
						}
						segment_put(&in_seg, &p,row,i);
						ptr2 = G_incr_void_ptr(ptr2,dsize);
					}
					break;
			}				
/*    			segment_get(&in_seg,&p,row,20);  */
/*            	printf("<%d %d>\n",((int*)cell)[20],(int)p); */

		}
	}
	segment_flush(&in_seg);
	if (verbose)
		G_percent (row, nrows, 2);

	/* Initialize output map with NULL VALUES */

	/*   Initialize segmented output file  */
	if (verbose)
		fprintf (stderr, "Initializing output \n") ;
	{
		double *fbuff ;
		double returnval;
		void* p = &returnval;
		int i ;

		fbuff = (double *)G_malloc(ncols * sizeof(double)) ;

		if (fbuff == NULL) {
			printf("fbuff == NULL\n");
		}
		
/*		for(i=0;i<ncols;i++) 
		fbuff[i] = neg ;
*/
		G_set_d_null_value(fbuff,ncols);

		for( row=0 ; row<nrows ; row++ ) {
			if (verbose) {
				G_percent (row, nrows, 2);
			}
/*
  segment_put_row(&out_seg, fbuffp, row);
*/
			for(i=0;i<ncols;i++) {
				segment_put(&out_seg, &fbuff[i], row, i);
			}
			
/*  			segment_get(&out_seg,p,row,0); */
/*          	printf("<%lf %lf>\n",fbuff[0],returnval); */
		}
		segment_flush(&out_seg);
		if (verbose)
			G_percent (row, nrows, 2);
		G_free(fbuff) ;
	}

/*   Scan the existing cum_cost_layer searching for starting points.
 *   Create a btree of starting points ordered by increasing costs.
 */
	if (opt7->answer) 
	{  
#if 1
		FILE* fp;
		struct start_pt  *new_start_pt;
		Site *site = NULL;               /* pointer to Site */
		search_mapset = "";
 
		search_mapset = G_find_file ("site_lists", opt7->answer, "");

		fp = G_fopen_sites_old ( opt7->answer, search_mapset);

		site = G_site_new_struct (-1, 2, 0, 0);

	    for (; (G_site_get(fp,site) != EOF);) {
			if (!G_site_in_region (site, &window))
				continue;
			have_start_points = 1;

			col = (int)G_easting_to_col(site->east, &window);
			row = (int)G_northing_to_row(site->north, &window );

			new_start_pt = (struct start_pt *)(G_malloc(sizeof(struct start_pt)));

			new_start_pt->row = row;
			new_start_pt->col = col;
			new_start_pt->next = NULL;

			if(head_start_pt == NULL)
			{
				head_start_pt = new_start_pt;
				pres_start_pt = new_start_pt;
				new_start_pt->next = NULL;
			}
			else
			{
				pres_start_pt->next = new_start_pt ;
				pres_start_pt = new_start_pt ;
			}
		}

		G_site_free_struct(site);	
		fclose(fp);
#endif
	}

	if (opt8->answer) 
	{  
#if 1
		FILE* fp;
		struct start_pt  *new_start_pt;
		Site *site = NULL;               /* pointer to Site */
		search_mapset = "";
 
		search_mapset = G_find_file ("site_lists", opt8->answer, "");

		fp = G_fopen_sites_old ( opt8->answer, search_mapset);

		site = G_site_new_struct (-1, 2, 0, 0);

	    for (; (G_site_get(fp,site) != EOF);) {
			if (!G_site_in_region (site, &window))
				continue;
			have_stop_points = 1;

			col = (int)G_easting_to_col(site->east, &window);
			row = (int)G_northing_to_row(site->north, &window );

			new_start_pt = (struct start_pt *)(G_malloc(sizeof(struct start_pt)));

			new_start_pt->row = row;
			new_start_pt->col = col;
			new_start_pt->next = NULL;

			if(head_end_pt == NULL)
			{
				head_end_pt = new_start_pt;
				pres_stop_pt = new_start_pt;
				new_start_pt->next = NULL;
			}
			else
			{
				pres_stop_pt->next = new_start_pt ;
				pres_stop_pt = new_start_pt ;
			}
		}

		G_site_free_struct(site);	
		fclose(fp);
#endif
	}

	if (! have_start_points)
	{
		int dsize2;
		cum_fd = G_open_cell_old (cum_cost_layer, cum_cost_mapset);
		if (cum_fd < 0)
		{
			sprintf (buf, "%s -can't open raster file", cum_cost_layer);
			G_fatal_error (buf);
			exit(1);
		}

		data_type2 = G_raster_map_type(cum_cost_layer,cum_cost_mapset);

		dsize2 = G_raster_size(data_type2);

		cell2 = G_allocate_raster_buf(data_type2);
/*  		cell2 = G_malloc(ncols*dsize); */

		if (cell2 == NULL) {
			fprintf(stderr,"Memory error\n");
			exit(1);
		} 
		if (verbose)
			fprintf (stderr, "Reading %s ... ", cum_cost_layer);
		for ( row=0 ; row<nrows ; row++ )
		{
			if (verbose)
				G_percent (row, nrows, 2);
			if ( G_get_raster_row (cum_fd, cell2, row, data_type2) < 0)
				exit(1);
			ptr2 = cell2;
			for ( col=0 ; col<ncols ; col++ )
			{
/* Did I understand that concept of cummulative cost map? - (pmx) 12 april 2000 */
				if (!G_is_null_value(ptr2,data_type2))
				{
					value= &zero;
					new_cell = insert(zero, row, col);
					segment_put(&out_seg, value, row, col);
				}
				ptr2 = G_incr_void_ptr(ptr2, dsize2);
			}
		}
		if (verbose)
			G_percent (row, nrows, 2);

		G_close_cell(cum_fd);
		G_free(cell2);
	}


/*  If the starting points are given on the command line start a linked
 *  list of cells ordered by increasing costs
 */
	else
	{
		struct start_pt *top_start_pt = NULL ;
		top_start_pt = head_start_pt;
		while(top_start_pt != NULL)
		{
			value= &zero;
			if (top_start_pt->row <0 || top_start_pt->row >= nrows
				|| top_start_pt->col <0 || top_start_pt-> col >= ncols)
			{
				sprintf(buf,
						"specified starting location outside database window");
				G_fatal_error (buf);
				exit(1);
			}
			new_cell = insert(zero, top_start_pt->row, top_start_pt->col);
			segment_put(&out_seg,value,top_start_pt->row,top_start_pt->col);
			top_start_pt = top_start_pt->next;
		}
/*  		printf("--------+++++----------\n"); */
	}

/*  Loop through the btree and perform at each cell the following:
 *   1) If an adjacent cell has not already been assigned a value compute
 *      the min cost and assign it.
 *   2) Insert the adjacent cell in the btree.
 *   3) Free the memory allocated to the present cell.
 */


	if (verbose) {
			system("date");
		fprintf (stderr, "Finding cost path\n");
	}
	n_processed = 0;
	total_cells = nrows * ncols ;
	at_percent = 0;

	pres_cell = get_lowest();
	while ( pres_cell != NULL )
	{
		struct cost* ct;
		double N, NE, E, SE, S, SW, W, NW ;
		double NNE, ENE, ESE, SSE, SSW, WSW, WNW, NNW ;

/* If we have surpassed the user specified maximum cost, then quit */
		if(maxcost && ((double)maxcost < pres_cell->min_cost))
			break ;

/*  fprintf(stderr,"P: %d,%d:%f\n",pres_cell->row,pres_cell->col,pres_cell->min_cost) ; */

		if (verbose)
			G_percent (++n_processed, total_cells, 1);

		segment_get(&in_seg,&my_cost,pres_cell->row,pres_cell->col);

/*          9    10       Order in which neighbors 
 *       13 5  3  6 14    are visited.
 *          1     2
 *       16 8  4  7 15
 *         12    11
 */
		for( neighbor=1;neighbor<=total_reviewed;neighbor++ )
		{
			switch(neighbor)
			{
				case 1:
					row = pres_cell->row ;
					col = pres_cell->col - 1 ;
					break ;
				case 2:
					col = pres_cell->col + 1 ;
					break ;
				case 3:
					row = pres_cell->row - 1 ;
					col = pres_cell->col ;
					break ;
				case 4:
					row = pres_cell->row + 1 ;
					break ;
				case 5:
					row = pres_cell->row - 1 ;
					col = pres_cell->col - 1 ;
					break ;
				case 6:
					col = pres_cell->col + 1 ;
					break ;
				case 7:
					row = pres_cell->row + 1 ;
					break ;
				case 8:
					col = pres_cell->col - 1 ;
					break ;
				case 9:
					row = pres_cell->row - 2 ;
					col = pres_cell->col - 1 ;
					break ;
				case 10:
					col = pres_cell->col + 1 ;
					break ;
				case 11:
					row = pres_cell->row + 2 ;
					break ;
				case 12:
					col = pres_cell->col - 1 ;
					break ;
				case 13:
					row = pres_cell->row - 1 ;
					col = pres_cell->col - 2 ;
					break ;
				case 14:
					col = pres_cell->col + 2 ;
					break ;
				case 15:
					row = pres_cell->row + 1 ;
					break ;
				case 16:
					col = pres_cell->col - 2 ;
					break ;
			}
				
			if ( row < 0 || row >= nrows)
				continue ;
			if( col < 0 || col >= ncols)
				continue;

   			value = &cost;

			switch(neighbor)
			{
				case 1:
					value = &W ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(W + my_cost) / 2.0 ;
					min_cost = pres_cell->min_cost+fcost*EW_fac ;
					break ;
				case 2:
					value = &E ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(E + my_cost) / 2.0 ;
					min_cost = pres_cell->min_cost+fcost*EW_fac ;
					break ;
				case 3:
					value = &N ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(N + my_cost) / 2.0 ;
					min_cost = pres_cell->min_cost+fcost*NS_fac ;
					break ;
				case 4:
					value = &S ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(S + my_cost) / 2.0 ;
					min_cost = pres_cell->min_cost+fcost*NS_fac ;
					break ;
				case 5:
					value = &NW ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(NW + my_cost) / 2.0 ;
					min_cost = pres_cell->min_cost+fcost*DIAG_fac ;
					break ;
				case 6:
					value = &NE ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(NE + my_cost) / 2.0 ;
					min_cost = pres_cell->min_cost+fcost*DIAG_fac ;
					break ;
				case 7:
					value = &SE ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(SE + my_cost) / 2.0 ;
					min_cost = pres_cell->min_cost+fcost*DIAG_fac ;
					break ;
				case 8:
					value = &SW ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(SW + my_cost) / 2.0 ;
					min_cost = pres_cell->min_cost+fcost*DIAG_fac ;
					break ;
				case 9:
					value = &NNW ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(N + NW + NNW + my_cost) / 4.0 ;
					min_cost = pres_cell->min_cost+fcost*V_DIAG_fac ;
					break ;
				case 10:
					value = &NNE ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(N + NE + NNE + my_cost) / 4.0 ;
					min_cost = pres_cell->min_cost+fcost*V_DIAG_fac ;
					break ;
				case 11:
					value = &SSE ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(S + SE + SSE + my_cost) / 4.0 ;
					min_cost = pres_cell->min_cost+fcost*V_DIAG_fac ;
					break ;
				case 12:
					value = &SSW ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(S + SW + SSW + my_cost) / 4.0 ;
					min_cost = pres_cell->min_cost+fcost*V_DIAG_fac ;
					break ;
				case 13:
					value = &WNW ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(W + NW + WNW + my_cost) / 4.0 ;
					min_cost = pres_cell->min_cost+fcost*H_DIAG_fac ;
					break ;
				case 14:
					value = &ENE ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(E + NE + ENE + my_cost) / 4.0 ;
					min_cost = pres_cell->min_cost+fcost*H_DIAG_fac ;
					break ;
				case 15:
					value = &ESE ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(E + SE + ESE + my_cost) / 4.0 ;
					min_cost = pres_cell->min_cost+fcost*H_DIAG_fac ;
					break ;
				case 16:
					value = &WSW ;
					segment_get(&in_seg,value,row,col);
					fcost = (double)(W + SW + WSW + my_cost) / 4.0 ;
					min_cost = pres_cell->min_cost+fcost*H_DIAG_fac ;
					break ;
			}

			if (G_is_d_null_value(&min_cost))
				continue;

			segment_get(&out_seg,&old_min_cost,row,col);
	
			if ( G_is_d_null_value(&old_min_cost))
			{
/*  				printf("*"); */
/*  				printf(".%.3lf %d,%d\n",min_cost,row,col);   */
				segment_put(&out_seg, &min_cost,row,col);
				new_cell = insert(min_cost, row, col);
/*
  check_all("Insert: ") ;
  fprintf(stderr,"I: %d,%d:%f\n", row,col,min_cost) ;
  show_all() ;
*/
			}
			else
			{
				struct cost* ct;
				if ( old_min_cost > min_cost )
				{
					segment_put(&out_seg, &min_cost,row,col);
					ct = find(old_min_cost, row, col) ;
					if (ct)
						delete(ct);
					else {
						printf ("Null...\n");
/*  						printf("%.3lf %.3lf %d,%d\n",old_min_cost,min_cost,row,col);   */
/*  						segment_put(&out_seg, &zero,row,col); */
						goto OUT;
					}
							 
					new_cell = insert(min_cost, row, col);
				}
				else {
				}
			}
		}

		if (have_stop_points && time_to_stop(pres_cell->row, pres_cell->col))
			break ;

/*
  fprintf(stderr,"D: %d,%d:%f\n", pres_cell->row,pres_cell->col,pres_cell->min_cost) ;
  show_all() ;
*/
	ct = pres_cell;
		delete(pres_cell) ;
/*
  check_all("Delete: ") ;
  show_all() ;
*/

		pres_cell = get_lowest() ;
		if (pres_cell == NULL) {
			if (verbose)
					fprintf(stderr,"End of map!\n");
			goto OUT;
		}
		if (ct == pres_cell)
			printf("Error, ct == pres_cell\n");
	}
OUT:
	/*  Open cumulative cost layer for writing   */

	cum_fd = G_open_raster_new(cum_cost_layer,data_type);

	/*  Write pending updates by segment_put() to output map   */

	segment_flush(&out_seg);

	/*  Copy segmented map to output map  */
	if (verbose) {
		system("date");
		fprintf (stderr, "Writing %s ... ", cum_cost_layer);
	}

	if (keep_nulls) {
		if(verbose)
				fprintf(stderr,"Will copy input map null values into output map\n");
		cell2 =  G_allocate_raster_buf(data_type); 
	}

	if (data_type == CELL_TYPE) {
		int* p;
		int* p2;
		if(verbose) {
				fprintf(stderr,"Integer cell type.\n");
				fprintf(stderr,"Writing...");
		}
		for ( row=0 ; row<nrows; row++ )
		{
			if (verbose)
				G_percent (row, nrows, 2);

			if (keep_nulls) {
				if(G_get_raster_row(cost_fd, cell2, row, data_type)<0) {
					fprintf(stderr,"Error getting input null cells\n");
					exit(1);
				}
			}
			p = cell;
			p2 = cell2;
			for ( col=0 ; col<ncols; col++ )
			{
				if (keep_nulls) {
					if (G_is_null_value(p2++, data_type)) {
						G_set_null_value((p+col),1,data_type);
						continue;
					}
				}
				segment_get(&out_seg, &min_cost ,row,col);
				if (G_is_d_null_value(&min_cost)) {
					G_set_null_value((p+col),1,data_type);
				} else {
					if (min_cost > peak) peak = min_cost;
					*(p+col) = (int)(min_cost+.5);
				}
			}
			G_put_raster_row(cum_fd,cell, data_type);
		}
	} else if (data_type == FCELL_TYPE) {
		float* p;
		float* p2;
		if(verbose) {
				fprintf(stderr,"Float cell type.\n");
				fprintf(stderr,"Writing...");
		}
		for ( row=0 ; row<nrows; row++ )
		{
			if (verbose)
				G_percent (row, nrows, 2);
			if (keep_nulls) {
				if(G_get_raster_row(cost_fd, cell2, row, data_type)<0) {
					fprintf(stderr,"Error getting input null cells\n");
					exit(1);
				}
			}
			p = cell;
			p2 = cell2;
			for ( col=0 ; col<ncols; col++ )
			{
				if (keep_nulls) {
					if (G_is_null_value(p2++, data_type)) {
						G_set_null_value((p+col),1,data_type);
						continue;
					}
				}
				segment_get(&out_seg,&min_cost ,row,col);
				if (G_is_d_null_value(&min_cost)) {
					G_set_null_value((p+col),1,data_type);
				} else {
					if (min_cost > peak) peak = min_cost;
					*(p+col) = (float)(min_cost);
				}
			}
			G_put_raster_row(cum_fd,cell, data_type);
		}
	} else if (data_type == DCELL_TYPE) {
		double* p;	
		double* p2;	
		if(verbose) {
				fprintf(stderr,"Double cell type.\n");
				fprintf(stderr,"Writing...");
		}
		for ( row=0 ; row<nrows; row++ )
		{
			if (verbose)
				G_percent (row, nrows, 2);
			if (keep_nulls) {
				if(G_get_raster_row(cost_fd, cell2, row, data_type)<0) {
					fprintf(stderr,"Error getting input null cells\n");
					exit(1);
				}
			}
			p = cell;
			p2 = cell2;
			for ( col=0 ; col<ncols; col++ )
			{
				if (keep_nulls) {
					if (G_is_null_value(p2++, data_type)) {
						G_set_null_value((p+col),1,data_type);
						continue;
					}
				}
				segment_get(&out_seg,&min_cost ,row,col);
				if (G_is_d_null_value(&min_cost)) {
					G_set_null_value((p+col),1,data_type);
				} else {
					if (min_cost > peak) peak = min_cost;
					*(p+col) = min_cost;
				}
			}
			G_put_raster_row(cum_fd,cell,data_type);
		}
	}

	if (verbose)
		G_percent (row, nrows, 2);

	printf("Peak cost value: %f\n",peak);

	segment_release(&in_seg);   /* release memory  */
	segment_release(&out_seg);
	G_close_cell(cost_fd);
	G_close_cell(cum_fd);
	close(in_fd);               /* close all files */
	close(out_fd);
	unlink(in_file);       /* remove submatrix files  */
	unlink(out_file);

	/*  Create colours for output map    */

	/*
	  G_read_range (cum_cost_layer, current_mapset, &range);
	  G_get_range_min_max(&range, &min, &max);
	  G_make_color_wave(&colors,min, max);
	  G_write_colors (cum_cost_layer,current_mapset,&colors);
	*/
	exit(0);
}

int 
process_answers (char **answers, struct start_pt **points, struct start_pt **top_start_pt)
{
	int col, row, n ;
	double east, north;

	struct start_pt *new_start_pt;
	int got_one = 0 ;

	*points = NULL ;

	if (! answers)
		return(0) ;
	
	for(n=0; *answers != NULL; answers+=2)
	{
		if(!G_scan_easting(*answers, &east, G_projection()))
		{
			fprintf (stderr, "Illegal x coordinate <%s>\n",
					 *answers);
			G_usage();
			exit(1);
		}
		if(!G_scan_northing(*(answers+1), &north, G_projection()))
		{
			fprintf (stderr, "Illegal y coordinate <%s>\n",
					 *(answers+1));
			G_usage();
			exit(1);
		}

		if(east < window.west ||
		   east > window.east ||
		   north < window.south ||
		   north > window.north)
		{
			fprintf(stderr,"Warning, ignoring point outside window: \n") ;
			fprintf(stderr,"   %.4f,%.4f\n", east, north) ;
			continue ;
		}
		else
			got_one = 1 ;

		row = (window.north - north) / window.ns_res;
		col = (east - window.west) / window.ew_res;

		new_start_pt = (struct start_pt *)(G_malloc(sizeof(struct start_pt)));

		new_start_pt->row = row;
		new_start_pt->col = col;
		new_start_pt->next = NULL;

		if(*points == NULL)
		{
			*points = new_start_pt;
			*top_start_pt = new_start_pt;
			new_start_pt->next = NULL;
		}
		else
		{
			(*top_start_pt)->next = new_start_pt ;
			*top_start_pt = new_start_pt ;
		}
	}
	return(got_one) ;
}

int 
time_to_stop (int row, int col)
{
	static int total = 0 ;
	static int hits = 0 ;
	struct start_pt *points ;

	if (total == 0)
	{
		for(points = head_end_pt;
			points != NULL;
			points = points->next, total++) ;
	}

	for(points = head_end_pt;
		points != NULL;
		points = points->next)

		if (points->row == row && points->col == col)
		{
			hits++ ;
			if (hits == total)
				return(1) ;
		}
	
	return(0) ;
}



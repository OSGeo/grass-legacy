
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

#define MAIN

#include "segment.h"
#include "gis.h"
#include "cost.h"
#include "stash.h"
#include <math.h>

struct Cell_head window;

main(argc,argv)
int argc;
char *argv[];
{
	CELL *cell;
	CELL min, max;
	SEGMENT in_seg, out_seg;
	char *cost_mapset ;
	char *cum_cost_mapset ;
	char *current_mapset ;
	char *in_file, *out_file ;
	char *search_mapset ;
	char *value ;
	char buf[400];
	double sqrt();
	extern struct Cell_head window;
	float NS_fac,EW_fac,DIAG_fac,H_DIAG_fac,V_DIAG_fac ;
	float dist_fac= 0.01 ;
	float fcost ;
	float min_cost,old_min_cost ;
	float neg = -2.0 ;
	float zero = 0.0 ;
	float row_fac,col_fac,diag_fac ;
	int at_percent = 0;
	int col, row, nrows, ncols ;
	int cost ;
	int cost_fd, cum_fd ;
	int delete() ;
	int have_start_points ;
	int have_stop_points ;
	int in_fd, out_fd ;
	int my_cost ;
	int my_fcost ;
	int n ;
	int srows, scols ;
	int total_reviewed ;
	int verbose = 1 ;
	int neighbor ;
	int segments_in_memory ;
	long n_processed = 0;
	long total_cells ;
	struct Cell_head cost_cellhd;
	struct Colors colors;
	struct Flag *flag1, *flag2;
	struct Option *opt1, *opt2, *opt3, *opt4;
	struct Range range;
	struct cost *pres_cell, *new_cell, *get_lowest(), *insert(), *find();
	struct start_pt *pres_start_pt ;

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

	flag1 = G_define_flag();
	flag1->key = 'v';
	flag1->description = "Run verbosly";

	flag2 = G_define_flag();
	flag2->key = 'k';
	flag2->description = "Use the 'Knight's move'; slower, but more accurate";

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
	DIAG_fac = (float)sqrt((double)(NS_fac*NS_fac + EW_fac*EW_fac));
	*/
	EW_fac = 1.0 ;
	NS_fac = window.ns_res/window.ew_res ;
	DIAG_fac = (float)sqrt((double)(NS_fac*NS_fac + EW_fac*EW_fac));
	V_DIAG_fac = (float)sqrt((double)(4*NS_fac*NS_fac + EW_fac*EW_fac)); 
	H_DIAG_fac = (float)sqrt((double)(NS_fac*NS_fac + 4*EW_fac*EW_fac)); 

	/*   Parse command line */

	if (G_parser(argc, argv))
		exit(-1);

	verbose = flag1->answer;
	if (flag2->answer)
		total_reviewed =  16 ;
	else
		total_reviewed =  8 ;

	have_start_points = process_answers(opt3->answers, &head_start_pt) ;

	have_stop_points  = process_answers(opt4->answers, &head_end_pt) ;

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

	cell = G_allocate_cell_buf();

	/*  Open cost cell layer for reading  */

	cost_fd = G_open_cell_old(cost_layer, cost_mapset);

	if (cost_fd < 0)
	{
		sprintf (buf, "%s - can't open raster file", cost_layer);
		G_fatal_error (buf);
		exit(1);
	}

	/*   Parameters for map submatrices   */

/*
	srows =  nrows/12 + 1;
	scols =  ncols/12 + 1;
*/
	srows = scols = 10 ;
	segments_in_memory = 4 * (nrows/10 + ncols/10 + 2) ;

	/*   Create segmented format files for cost layer and output layer  */

	if (verbose)
	{
		fprintf (stderr, "Creating some temporary files ...");
		fflush (stderr);
	}

	in_fd = creat(in_file,0666);
	segment_format(in_fd, nrows, ncols, srows, scols, sizeof(int));
	close(in_fd);

	out_fd = creat(out_file,0666);
	segment_format(out_fd, nrows, ncols, srows, scols, sizeof(float));
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

	for( row=0 ; row<nrows ; row++ )
	{
		if (verbose)
			G_percent (row, nrows, 2);
		if( G_get_map_row(cost_fd, cell, row)<0)
			exit(1);
		segment_put_row(&in_seg, cell, row);
	}
	if (verbose)
		G_percent (row, nrows, 2);

	/* Initialize output map with -2 */
	/*   Initialize segmented output file  */
	if (verbose)
	    fprintf (stderr, "Initializing output ") ;
	{
		float *buff ;
		int i ;
		buff = (float *)G_malloc(ncols * sizeof(float)) ;
		for(i=0;i<ncols;i++) 
		    buff[i] = neg ;

		for( row=0 ; row<nrows ; row++ ) {
			if (verbose)
				G_percent (row, nrows, 2);
			segment_put_row(&out_seg, (char *)buff, row);
		}
		if (verbose)
			G_percent (row, nrows, 2);
		free(buff) ;
	}


/*   Scan the existing cum_cost_layer searching for starting points.
 *   Create a btree of starting points ordered by increasing costs.
 */
	if (! have_start_points)
	{
		cum_fd = G_open_cell_old (cum_cost_layer, cum_cost_mapset);

		if (cum_fd < 0)
		{
			sprintf (buf, "%s -can't open raster file", cum_cost_layer);
			G_fatal_error (buf);
			exit(1);
		}

		if (verbose)
			fprintf (stderr, "Reading %s ... ", cum_cost_layer);
		for ( row=0 ; row<nrows ; row++ )
		{
			if (verbose)
				G_percent (row, nrows, 2);
			if ( G_get_map_row (cum_fd, cell, row) < 0)
				exit(1);
			for ( col=0 ; col<ncols ; col++ )
			{
			    if (*(cell+col) > 0)
			    {
			       value= (char *)&zero;
			       new_cell = insert(zero, row, col);
			       segment_put(&out_seg, value, row, col);
		            }	
			}
		}
		if (verbose)
			G_percent (row, nrows, 2);
		G_close_cell(cum_fd);
	}


/*  If the starting points are given on the command line start a linked
 *  list of cells ordered by increasing costs
 */
	else
	{
		pres_start_pt = head_start_pt;
		while(pres_start_pt != NULL)
		{
			value= (char *)&zero;
			if (pres_start_pt->row <0 || pres_start_pt->row >= nrows
			    || pres_start_pt->col <0 || pres_start_pt-> col >= ncols)
			{
				sprintf(buf,
				    "specified starting location outside database window");
				G_fatal_error (buf);
				exit(1);
			}

			new_cell = insert(zero, pres_start_pt->row, pres_start_pt->col);
			segment_put(&out_seg,value,pres_start_pt->row,pres_start_pt->col);

			pres_start_pt = pres_start_pt->next;
		}
	}

/*  Loop through the btree and perform at each cell the following:
 *   1) If an adjacent cell has not already been assigned a value compute
 *      the min cost and assign it.
 *   2) Insert the adjacent cell in the btree.
 *   3) Free the memory allocated to the present cell.
 */

	if (verbose)
		fprintf (stderr, "Finding cost path\n");
	n_processed = 0;
	total_cells = nrows * ncols ;
	at_percent = 0;

	pres_cell = get_lowest();
	while ( pres_cell != NULL )
	{
	int N, NE, E, SE, S, SW, W, NW ;
	int NNE, ENE, ESE, SSE, SSW, WSW, WNW, NNW ;
/*
fprintf(stderr,"P: %d,%d:%f\n",pres_cell->row,pres_cell->col,pres_cell->min_cost) ;
*/
		if (verbose)
			G_percent (++n_processed, total_cells, 1);

		value = (char *)&my_cost;
		segment_get(&in_seg,value,pres_cell->row,pres_cell->col);

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

			value = (char *)&cost;
			switch(neighbor)
			{
			case 1:
				value = (char *)&W ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(W + my_cost) / 2.0 ;
				min_cost = pres_cell->min_cost+fcost*EW_fac ;
				break ;
			case 2:
				value = (char *)&E ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(E + my_cost) / 2.0 ;
				min_cost = pres_cell->min_cost+fcost*EW_fac ;
				break ;
			case 3:
				value = (char *)&N ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(N + my_cost) / 2.0 ;
				min_cost = pres_cell->min_cost+fcost*NS_fac ;
				break ;
			case 4:
				value = (char *)&S ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(S + my_cost) / 2.0 ;
				min_cost = pres_cell->min_cost+fcost*NS_fac ;
				break ;
			case 5:
				value = (char *)&NW ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(NW + my_cost) / 2.0 ;
				min_cost = pres_cell->min_cost+fcost*DIAG_fac ;
				break ;
			case 6:
				value = (char *)&NE ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(NE + my_cost) / 2.0 ;
				min_cost = pres_cell->min_cost+fcost*DIAG_fac ;
				break ;
			case 7:
				value = (char *)&SE ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(SE + my_cost) / 2.0 ;
				min_cost = pres_cell->min_cost+fcost*DIAG_fac ;
				break ;
			case 8:
				value = (char *)&SW ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(SW + my_cost) / 2.0 ;
				min_cost = pres_cell->min_cost+fcost*DIAG_fac ;
				break ;
			case 9:
				value = (char *)&NNW ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(N + NW + NNW + my_cost) / 4.0 ;
				min_cost = pres_cell->min_cost+fcost*V_DIAG_fac ;
				break ;
			case 10:
				value = (char *)&NNE ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(N + NE + NNE + my_cost) / 4.0 ;
				min_cost = pres_cell->min_cost+fcost*V_DIAG_fac ;
				break ;
			case 11:
				value = (char *)&SSE ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(S + SE + SSE + my_cost) / 4.0 ;
				min_cost = pres_cell->min_cost+fcost*V_DIAG_fac ;
				break ;
			case 12:
				value = (char *)&SSW ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(S + SW + SSW + my_cost) / 4.0 ;
				min_cost = pres_cell->min_cost+fcost*V_DIAG_fac ;
				break ;
			case 13:
				value = (char *)&WNW ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(W + NW + WNW + my_cost) / 4.0 ;
				min_cost = pres_cell->min_cost+fcost*H_DIAG_fac ;
				break ;
			case 14:
				value = (char *)&ENE ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(E + NE + ENE + my_cost) / 4.0 ;
				min_cost = pres_cell->min_cost+fcost*H_DIAG_fac ;
				break ;
			case 15:
				value = (char *)&ESE ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(E + SE + ESE + my_cost) / 4.0 ;
				min_cost = pres_cell->min_cost+fcost*H_DIAG_fac ;
				break ;
			case 16:
				value = (char *)&WSW ;
				segment_get(&in_seg,value,row,col);
				fcost = (float)(W + SW + WSW + my_cost) / 4.0 ;
				min_cost = pres_cell->min_cost+fcost*H_DIAG_fac ;
				break ;
			}

			value = (char *)&old_min_cost;
			segment_get(&out_seg,value,row,col);

			if ( old_min_cost < -1.0 )
			{
				value= (char *)&min_cost;
				segment_put(&out_seg,value,row,col);
				new_cell =insert(min_cost, row, col);
/*
check_all("Insert: ") ;
fprintf(stderr,"I: %d,%d:%f\n", row,col,min_cost) ;
show_all() ;
*/
			}
			else
			{
				if ( old_min_cost > min_cost )
				{
					value= (char *)&min_cost;
					segment_put(&out_seg,value,row,col);
/*
fprintf(stderr,"D: %d,%d:%f\n", row,col,old_min_cost) ;
show_all() ;
*/
					delete(find(old_min_cost, row, col)) ;
/*
check_all("Delete: ") ;
show_all() ;
*/
					new_cell = insert(min_cost, row, col);
/*
check_all("Insert: ") ;
fprintf(stderr,"I: %d,%d:%f\n", row,col,min_cost) ;
show_all() ;
*/
				}
			}
		}

		if (have_stop_points && time_to_stop(pres_cell->row, pres_cell->col))
			break ;

/*
fprintf(stderr,"D: %d,%d:%f\n", pres_cell->row,pres_cell->col,pres_cell->min_cost) ;
show_all() ;
*/
		delete(pres_cell) ;
/*
check_all("Delete: ") ;
show_all() ;
*/
		pres_cell = get_lowest() ;
	}

	/*  Open cumulative cost layer for writing   */

	cum_fd = G_open_cell_new(cum_cost_layer);

	/*  Write pending updates by segment_put() to output map   */

	segment_flush(&out_seg);

	/*  Copy segmented map to output map casting doubles into integers */
	if (verbose)
		fprintf (stderr, "Writing %s ... ", cum_cost_layer);
	for ( row=0 ; row<nrows; row++ )
	{
		if (verbose)
			G_percent (row, nrows, 2);
		for ( col=0 ; col<ncols; col++ )
		{
			value = (char *)&min_cost;
			segment_get(&out_seg,value ,row,col);
			if (min_cost < 0.) min_cost = 0. ;

			*(cell+col) = (int)(min_cost+.5);
		}
		G_put_map_row(cum_fd,cell,row);
	}
	if (verbose)
		G_percent (row, nrows, 2);

	segment_release(&in_seg);   /* release memory  */
	segment_release(&out_seg);

	close(in_fd);               /* close all files */
	close(out_fd);

	G_close_cell(cum_fd);
	G_close_cell(cost_fd);

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

process_answers(answers, points)
	char **answers ;
	struct start_pt **points ;
{
	int col, row, n ;
	double east, north;

	struct start_pt *pres_start_pt, *new_start_pt;
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
			fprintf(stderr,"   %.4lf,%.4lf\n", east, north) ;
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
			pres_start_pt = new_start_pt;
			new_start_pt->next = NULL;
		}
		else
		{
			pres_start_pt->next = new_start_pt ;
			pres_start_pt = new_start_pt ;
		}
	}
	return(got_one) ;
}

time_to_stop(row, col)
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

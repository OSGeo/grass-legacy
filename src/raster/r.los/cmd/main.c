/****************************************************************
 *      main.c
 *
 *      This is the main program for line-of-sight analysis.
 *      It takes a digital elevation map and identifies all
 *      the grid cells that are visible from a user specified
 *      observer location.  Other input parameters to the
 *      program include the height of the observer above the
 *      ground, a map marking areas of interest in which the
 *      analysis is desired, and a maximum range for the line
 *      of sight.
 *
 ****************************************************************/

#include <unistd.h>
#include <math.h>
#include <fcntl.h>
#include "gis.h"
#define MAIN
#include "segment.h"
#include "cmd_line.h"
#include "point.h"
#include "local_proto.h"

#define   COLOR_SHIFT      155.0
#define   COLOR_MAX        255.0

#define   SEARCH_PT_INCLINATION		SEARCH_PT->inclination
#define	  NEXT_SEARCH_PT		SEARCH_PT->next

struct Cell_head window;	/*	database window		*/
      
int 
main (int argc, char *argv[])
{
	int row_viewpt,col_viewpt,nrows,ncols,a,b,row,patt_flag;
	int  segment_no,flip,xmax,ymax,sign_on_y,sign_on_x;
	int submatrix_rows,submatrix_cols,lenth_data_item;
	int new,old,patt=0,in_fd,out_fd,patt_fd=0;
	double slope_1,slope_2,max_vert_angle=0.0,color_factor;
	char *old_mapset,*patt_mapset=NULL;
	CELL *value;
	char *search_mapset, *current_mapset;
	char *in_name, *out_name, *patt_name=NULL;
	struct Categories cats;
	struct Cell_head cellhd_elev, cellhd_patt;
	extern struct Cell_head window;
	char buf[1024];
	CELL *cell, data, viewpt_elev;
	SEGMENT seg_in, seg_out, seg_patt;
	struct point *heads[16],*SEARCH_PT;
	struct GModule *module;
	struct Option *opt1,*opt2,*opt3,*opt5,*opt6,*opt7;

	module = G_define_module();
	module->description =
		"Line-of-sight raster analysis program.";

	/* Define the different options */

	opt1 = G_define_option() ;
	opt1->key        = "input";
	opt1->type       = TYPE_STRING;
	opt1->required   = YES;
	opt1->gisprompt  = "old,cell,raster" ;
	opt1->description= "Raster map containing elevation data" ;
   
	opt7 = G_define_option() ;
	opt7->key        = "output";
	opt7->type       = TYPE_STRING;
	opt7->required   = YES;
	opt7->gisprompt  = "new,cell,raster" ;
	opt7->description= "Raster map name for storing results";

	opt3 = G_define_option() ;
	opt3->key        = "coordinate";
	opt3->type       = TYPE_STRING;
	opt3->required   = YES;
	opt3->key_desc   = "x,y";
	opt3->description= "Coordinate identifying the viewing location";

	opt2 = G_define_option() ;
	opt2->key        = "patt_map";
	opt2->type       = TYPE_STRING;
	opt2->required   = NO;
	opt2->description= "Binary (1/0) raster map";
	opt2->gisprompt  = "old,cell,raster" ;

	opt5 = G_define_option() ;
	opt5->key        = "obs_elev";
	opt5->type       = TYPE_DOUBLE;
	opt5->required   = NO;
	opt5->answer     = "1.75";
	opt5->description= "Height of the viewing location";

	opt6 = G_define_option() ;
	opt6->key        = "max_dist";
	opt6->type       = TYPE_DOUBLE;
	opt6->required   = NO;
	opt6->answer     = "100";
	opt6->options    = "0-99999" ;
	opt6->description= "Max distance from the viewing point (meters)" ;

	G_gisinit (argv[0]);

	if (G_parser(argc, argv))
		exit (-1);

	G_scan_easting  (opt3->answers[0], &east,  G_projection()) ;
	G_scan_northing (opt3->answers[1], &north, G_projection()) ;

	sscanf (opt5->answer, "%lf", &obs_elev);
	sscanf (opt6->answer, "%lf", &max_dist);
	elev_layer = opt1->answer;
	patt_layer = opt2->answer;
	out_layer = opt7->answer;

	G_get_window (&window);

	current_mapset= G_mapset();
    /* set flag to indicate presence of areas of interest   */
    if(patt_layer == NULL)
        patt_flag=0;
    else
        patt_flag=1;


    /* check if specified observer location inside window   */
    if(east<window.west || east>window.east
        || north>window.north || north<window.south)
    {
        sprintf (buf,
            "specified observer location outside database region");
        G_fatal_error (buf);
        exit(1);
    }

    search_mapset = "";
    old_mapset = G_find_cell2 (elev_layer, search_mapset);

    /*  check if elevation layer present in database    */
    if (old_mapset == NULL)
    {
        sprintf (buf, "%s -elev_layer not found", elev_layer);
        G_fatal_error (buf);
        exit(1);
    }

    /* if pattern layer used, check if present in database  */
    if(patt_flag == 1)
    {
        patt_mapset = G_find_cell (patt_layer, search_mapset);
        if(patt_mapset == NULL)
        {
            sprintf (buf, "%s - patt_layer not found", patt_layer);
            G_fatal_error (buf);
            exit(1);
        }
    }

    /* check if specified output layer name is legal    */
    if (G_legal_filename(out_layer) < 0)
    {
        sprintf (buf, "%s -the out_layer is illegal name", out_layer);
        G_fatal_error (buf);
        exit(1);
    }

    /*   check if specified output layer is unique      */
    if (G_find_cell (out_layer, current_mapset))
    {
        sprintf (buf,
            "%s - already exits. can't overwrite", out_layer);
        G_fatal_error (buf);
        exit(1);
    }

    /*  read header info for elevation layer        */
    if(G_get_cellhd(elev_layer,old_mapset,&cellhd_elev)<0)
    {
        sprintf (buf, "%s in %s - can't read raster header",
            elev_layer, old_mapset);
        G_fatal_error (buf);
        exit(1);
    }

    /*  if pattern layer present, read in its header info   */
    if(patt_flag == 1)
    {
        if(G_get_cellhd(patt_layer,patt_mapset,&cellhd_patt)<0)
        {
            sprintf (buf, "%s in %s -can't read raster header",
                patt_layer, patt_mapset);
            G_fatal_error (buf);
            exit(1);
        }
    }

	/*  find number of rows and columns in elevation map	*/
	nrows = G_window_rows();
	ncols = G_window_cols();
	/*  allocate buffer space for row-io to layer		*/
	cell = G_allocate_cell_buf();
	/*	open elevation overlay file for reading		*/
	old = G_open_cell_old (elev_layer, old_mapset);
	if (old < 0)
	{
		char buf[200];
		sprintf (buf, 
		    "%s in %s - can't open cell file",elev_layer,old_mapset);
		G_fatal_error (buf);
		exit(1);
	}
	/*	open cell layer for writing output 		*/
	new = G_open_cell_new (out_layer);
	if (new < 0)
	{
		sprintf(buf, "%s - can't create cell file", out_layer);
		G_fatal_error (buf);
		exit(1);
	}
	/* if pattern layer specified, open it for reading	*/
	if(patt_flag == 1)
	{
		patt = G_open_cell_old (patt_layer, patt_mapset);

		if (old < 0)
		{
			char buf[200];
			sprintf (buf, "%s in %s - can't open cell file", 
			    patt_layer, patt_mapset);
			G_fatal_error (buf);
			exit(1);
		}
	}

	/*	parameters for map submatrices			*/
	lenth_data_item = sizeof(CELL);
	submatrix_rows = nrows/4 + 1;
	submatrix_cols = ncols/4 + 1;
	/* create segmented format files for elevation layer,	*/
	/* output layer and pattern layer (if present)		*/
	in_name = G_tempfile();
	in_fd = creat(in_name, 0666);
	segment_format(in_fd,nrows,ncols,
	    submatrix_rows,submatrix_cols,lenth_data_item);
	close(in_fd);
	out_name = G_tempfile();
	out_fd = creat(out_name, 0666);
	segment_format(out_fd,nrows,ncols,
	    submatrix_rows,submatrix_cols,lenth_data_item);
	close(out_fd);
	if(patt_flag == 1)
	{
		patt_name = G_tempfile();
		patt_fd = creat(patt_name, 0666);
		segment_format(patt_fd,nrows,ncols,
		    submatrix_rows,submatrix_cols,lenth_data_item);
		close(patt_fd);
	}
	/*	open, initialize and segment all files		*/
	in_fd = open(in_name,2);
	segment_init(&seg_in,in_fd,4);
	out_fd = open(out_name,2);
	segment_init(&seg_out,out_fd,4);
	if(patt_flag == 1)
	{
		patt_fd = open(patt_name,2);
		segment_init(&seg_patt,patt_fd,4);
		for(row = 0; row < nrows; row++)
		{
			if (G_get_map_row (patt,cell,row) < 0)
				exit(1);
			segment_put_row(&seg_patt,cell,row);
		}
	}
	for(row = 0; row < nrows; row++)
	{
		if (G_get_map_row (old,cell,row) < 0)
			exit(1);
		segment_put_row(&seg_in,cell,row);
	}
	/* calc map array coordinates for viewing point		*/
	row_viewpt = (window.north - north)/window.ns_res;
	col_viewpt = (east - window.west)/window.ew_res;
	/*	 read elevation of viewing point		*/
	value = &viewpt_elev;
	segment_get(&seg_in,value,row_viewpt,col_viewpt);
	viewpt_elev += obs_elev;
	/*	DO LOS ANALYSIS FOR SIXTEEN SEGMENTS		*/
	for(segment_no=1;segment_no<=16;segment_no++){
		sign_on_y= 1- (segment_no-1)/8 * 2;
		if(segment_no>4 && segment_no<13)
			sign_on_x= -1; 
		else sign_on_x=1;
		/*	calc slopes for bounding rays of a segment	*/
		if(segment_no==1 || segment_no==4 || segment_no==5  ||
		    segment_no==8 || segment_no==9 || segment_no==12 || 
		    segment_no==13 || segment_no==16)
		{ 
			slope_1= 0.0;      
			slope_2= 0.5;
		}
		else { 
			slope_1= 0.5;      
			slope_2= 1.0;
		}
		if(segment_no==1 || segment_no==2 || segment_no==7 || 
		    segment_no==8 || segment_no==9  || segment_no==10 || 
		    segment_no==15 || segment_no==16)
			flip = 0;
		else flip = 1;
		/*	calculate max and min 'x' and 'y'		*/
		a= ((ncols-1)*(sign_on_x+1)/2 - sign_on_x * col_viewpt);
		b= (1-sign_on_y)/2*(nrows-1) + sign_on_y*row_viewpt;
		if(flip==0){ 
			xmax=a; 
			ymax=b;
		}
		else { 
			xmax=b; 
			ymax=a;
		}
		/*	perform analysis for every segment		*/
		heads[segment_no-1]= segment(segment_no,xmax,ymax,
		    slope_1,slope_2,flip,sign_on_y,sign_on_x,
		    viewpt_elev,&seg_in,&seg_out,&seg_patt,
		    row_viewpt,col_viewpt,patt_flag);
	}	/*	end of for-loop over segments		*/
	/* loop over all segment lists to find maximum vertical	*/
	/* angle of any point when viewed from observer location*/
	for(segment_no=1; segment_no <= 16; segment_no++)
	{
		SEARCH_PT = heads[segment_no-1];
		while(SEARCH_PT != NULL)
		{
			if(fabs(SEARCH_PT_INCLINATION) > max_vert_angle)
				max_vert_angle = fabs(SEARCH_PT_INCLINATION);
			SEARCH_PT = NEXT_SEARCH_PT;
		}
	}
	/* calculate factor to be multiplied to every vertical	*/
	/* angle for suitable color variation on output map	*/
	color_factor = decide_color_range(max_vert_angle*57.3,
	    COLOR_SHIFT,COLOR_MAX);
	color_factor = 1.0; /* to give true angle? */
	/* mark visible points for all segments on outputmap	*/
	for(segment_no=1; segment_no <= 16; segment_no++)
	{
		mark_visible_points(heads[segment_no-1],&seg_out,
		    row_viewpt,col_viewpt,color_factor,COLOR_SHIFT);
	}
	/*	mark viewpt on output map			*/
	data = 180;
	value = &data;
	segment_put(&seg_out,value,row_viewpt,col_viewpt);
	/* write pending updates by segment_put() to outputmap  */
	segment_flush(&seg_out);
	/* convert output submatrices to full cell overlay	*/
	for(row=0; row< nrows; row++)
	{
		int col ;
		segment_get_row(&seg_out,cell,row);
		for (col=0; col < ncols; col++)
		{
			if (cell[col] == 1) cell[col] = 0;
		}
		if(G_put_map_row(new,cell) < 0)
		{
			exit(1);
		}
	}
	segment_release(&seg_in);	/* release memory	*/
	segment_release(&seg_out);
	segment_release(&seg_patt);
	close(in_fd);			/* close all files	*/
	close(out_fd);
	unlink (in_name);               /* remove temp files as well */
	unlink (out_name);
	G_close_cell(old);
	G_close_cell(new);
	if(patt_flag == 1)
	{
		close(patt_fd);
		G_close_cell(patt);
	}
	/*      create category file for output map             */
	G_read_cats (out_layer, current_mapset, &cats);
	G_set_cats_fmt ("$1 degree$?s", 1.0, 0.0, 0.0, 0.0, &cats);
	G_write_cats (out_layer, &cats);

	exit(0);
}

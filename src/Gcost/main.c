/* %W% %G% */
/*********************************************************************
 *
 *     main.c          in             ~/src/Gcost
 *
 *     Usage: Gcost output=name input=name [coor=east,north] ...
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

#define USAGE "output=name input=name [coor=east,north] [coor=east,north] ..."
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
    int col, row, nrows, ncols,
        len, flag,
        cost,
        srows, scols,
        cost_fd, cum_fd,
        in_fd, out_fd, 
	delete(),
        stash_away();

  float min_cost,old_min_cost,
        dist_fac= 0.01,
	NS_fac,EW_fac,DIAG_fac,
	row_fac,col_fac,diag_fac,
	zero = 0.0, neg = -2.0;

 double sqrt();

   char *current_mapset,
        *search_mapset,
        *cum_cost_mapset, 
        *cost_mapset,
        *in_file, *out_file,
        *value,
        buf[400];

   CELL *cell;

   SEGMENT in_seg, out_seg;

   struct Cell_head cost_cellhd;
   struct Range range;

   struct cost *pres_cell, *new_cell, *old_cell, 
          *start_cell = NULL, *end_cell = NULL,
          *insert();

   extern struct Cell_head window;

   struct Colors colors;  

   struct start_pt *pres_start_pt;

/* initalize access to database and create temporary files */    

    G_gisinit ("Gcost"); 

    in_file = G_tempfile();
    out_file = G_tempfile();
   
/*  Get database window parameters      */
 
    if(G_get_window (&window) < 0) {
       sprintf (buf,"can't read current window parameters");
       G_fatal_error (buf);
       exit(1);
    }

/*  Find north-south, east_west and diagonal factors */

    NS_fac = window.ns_res*dist_fac;
    EW_fac = window.ew_res*dist_fac;
    DIAG_fac = (float)sqrt((double)(NS_fac*NS_fac + EW_fac*EW_fac));

    
/*   Check for correct no of command line arguments   */

     if(argc < 3) {
       fprintf(stderr,"usage: %s %s\n", argv[0], USAGE);
       exit(-1);
    }

/* Do parsing of the command line */

    if (D_parse_command(argc, argv, variables, n_variables, stash_away)){
        fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }

    current_mapset = G_mapset();

/*  Search for output layer (cum_cost_layer) in all mapsets */

    search_mapset = "";
    cum_cost_mapset = G_find_cell (cum_cost_layer, search_mapset);

/*  Set flag according to input */

    if (cum_cost_mapset != NULL) {
        if (head_start_pt == NULL)
            flag = 1; /* output layer exists and starting pts are not given  */
        else
            flag = 2; /* output layer exists and starting pts are given */
    }
    else
        flag = 3;     /* output layer does not exist */
 
/*  Check if cost layer exists in data base  */

    cost_mapset = G_find_cell (cost_layer, search_mapset);

    if (cost_mapset == NULL) { 
       sprintf(buf, "%s - not found", cost_layer); 
       G_fatal_error (buf); 
       exit(1); 
    }
    
/*  Check if specified output layer name is legal   */

    if (flag == 3); 
        if (G_legal_filename (cum_cost_layer) < 0) {
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

    if (cost_fd < 0) {
        sprintf (buf,
                 "%s in %s -can't open cell file",
                 cost_layer,
                 cost_mapset);
        G_fatal_error (buf);
        exit(1);
     }

/*   Parameters for map submatrices   */

     srows =  nrows/12 + 1;
     scols =  ncols/12 + 1;

/*   Create segmented format files for cost layer and output layer  */

     in_fd = creat(in_file,0666);
     segment_format(in_fd, nrows, ncols, srows, scols, sizeof(int));
     close(in_fd);

     out_fd = creat(out_file,0666);   
     segment_format(out_fd, nrows, ncols, srows, scols, sizeof(float));
     close(out_fd);

/*   Open initialize and segment all files  */

     in_fd = open(in_file,2);
     segment_init(&in_seg,in_fd,40);
 

     out_fd = open(out_file,2);
     segment_init(&out_seg,out_fd,40);

/*   Write the cost layer in the segmented file  */

     for( row=0 ; row<nrows ; row++ ) {
         if( G_get_map_row(cost_fd, cell, row)<0)
             exit(1);
             segment_put_row(&in_seg, cell, row);
         }

/*   Scan the existing cum_cost_layer searching for starting points.
 *   Create a linked list of starting points ordered by increasing costs.
 */ 
     if (flag == 1) {
         cum_fd = G_open_cell_old (cum_cost_layer, cum_cost_mapset);
    
         if (cum_fd < 0) {
             sprintf (buf,
                      "%s in %s -can't open cell file", 
                      cum_cost_layer,
                      cum_cost_mapset);
         G_fatal_error (buf);
         exit(1);  
         }
 

         for ( row=0 ; row<nrows ; row++ ) {
             if ( G_get_map_row (cum_fd, cell, row) < 0)
                 exit(1);
             for ( col=0 ; col<ncols ; col++ ) {
                 if (*(cell+col) > 0) {  
                     value = (char *)&cost;
                     segment_get(&in_seg,value,row,col);
                     new_cell = insert(&start_cell, &end_cell,(float )cost, row, col);
                     value = (char *)&zero; 
                 }   
                 else {
                     value = (char *)&neg;     
                 }
                 segment_put(&out_seg, value, row, col);
             } 
         } 
         G_close_cell(cum_fd);
     }   


/*  If the starting points are given on the command line start a linked
 *  list of cells ordered by increasing costs
 */
    if (flag == 2 || flag == 3) {
       for ( row=0 ; row<nrows ; row++ ) {
           for ( col=0 ; col<ncols ; col++ ) {
                value = (char *)&neg;
                segment_put(&out_seg, value, row, col);
           }
       }

       pres_start_pt = head_start_pt;
       while(pres_start_pt != NULL) {
           value = (char *)&cost;
           if (pres_start_pt->row <0 || pres_start_pt->row >= nrows
               || pres_start_pt->col <0 || pres_start_pt-> col >= ncols) {
               sprintf(buf,
                       "specified starting location outside database window");
               G_fatal_error (buf);
               exit(1);
           }
           segment_get(&in_seg,value,
                       pres_start_pt->row,pres_start_pt->col);

           new_cell = insert(&start_cell, &end_cell,(float )cost, pres_start_pt->row, pres_start_pt->col); 

           value = (char *)&zero;
           segment_put(&out_seg, value, pres_start_pt->row, pres_start_pt->col);
           pres_start_pt = pres_start_pt->next;
       }
   }
   
/*  Loop through the linked list and perform at each cell the following:
 *   1) If an adjacent cell has not already been assigned a value compute
 *      the min cost and assign it.
 *   2) Insert the adjacent cell in the linked list.
 *   3) Free the memory allocated to the present cell.
 */
         
    pres_cell = start_cell;
    while ( pres_cell != NULL ) {

	for( row=(pres_cell->row)-1;
             row<=(pres_cell->row)+1;
             row++ ) {
            if ( row < 0 || row >= nrows)
              continue; 

            row_fac = ( (pres_cell->row -row) != 0 ) ? 0 :NS_fac ;

            for(col=(pres_cell->col)-1;
	        col<=(pres_cell->col)+1;
 	        col++ ) {
                if( col < 0 || col >= ncols)
                    continue;

                col_fac = ( (pres_cell->col - col) != 0 ) ? 0 :EW_fac ;
		diag_fac = ( row_fac || col_fac ) ? 0 : DIAG_fac;

                value = (char *)&old_min_cost;
                segment_get(&out_seg,value,row,col);

                value = (char *)&cost;
                segment_get(&in_seg,value,row,col);

	        min_cost = pres_cell->min_cost+cost*(col_fac+row_fac+diag_fac);
		if ( old_min_cost < -1.0 ) {

                    value= (char *)&min_cost;
                    segment_put(&out_seg,value,row,col);

		    new_cell =insert(&start_cell, &end_cell, min_cost, row, col);
                }            
		else {
		       if ( old_min_cost > min_cost ){

                       value= (char *)&min_cost;
                       segment_put(&out_seg,value,row,col);

                       new_cell = insert(&start_cell, &end_cell, min_cost, row, col);
		       delete(&end_cell, row, col);
         
                    }
                    else {
			continue;
		    }	
                } 
            }
        }	    
        old_cell = pres_cell;
        pres_cell = pres_cell->next;
        free(old_cell);
    }  
 


/*  Open cumulative cost layer for writing   */

    cum_fd = G_open_cell_new(cum_cost_layer);

/*  Write pending updates by segment_put() to output map   */ 

    segment_flush(&out_seg);  

/*  Copy segmented map to output map casting doubles into integers */
    for ( row=0 ; row<nrows; row++ ) {
	for ( col=0 ; col<ncols; col++ ) {

     	   value = (char *)&min_cost;
           segment_get(&out_seg,value ,row,col);

           *(cell+col) = (int)min_cost;
        }
        G_put_map_row(cum_fd,cell,row);
    }

    segment_release(&in_seg);   /* release memory  */
    segment_release(&out_seg);

    close(in_fd);               /* close all files */
    close(out_fd);

    G_close_cell(cum_fd);
    G_close_cell(cost_fd);

    unlink(in_file);       /* remove submatrix files  */
    unlink(out_file);

/*  Create colours for output map    */

    G_read_range (cum_cost_layer, current_mapset, &range);
    
    G_make_color_wave(&colors,range.pmin, range.pmax);
    G_write_colors (cum_cost_layer,current_mapset,&colors);

}



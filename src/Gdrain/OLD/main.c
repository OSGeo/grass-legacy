/* %W% %G% */
/**********************************************************************/
/*                                                                    */
/*     main.c          in             ~/src/Gdrain                    */
/*                                                                    */
/*     Usage: Gdrain drain_path_layer=name elevation_layer=name	      */ 
/*	[start_pt=row,col] [start_pt=row,col]    		      */
/*                                                                    */
/*	This is the main program for tracing out the path that a      */
/*	drop of water would take if released at a certain location    */
/*	on an input elevation map.				      */
/*                                                                    */
/**********************************************************************/

#define USAGE "output=name input=name [coor=east,north] [coor=east,north] ..."

#include "segment.h"
#include "gis.h"
#define MAIN
#include "stash.h"

#define		POINT			struct point
#define		PRES_PT_ELEV		PRES_PT->elevation
#define		PRES_PT_ROW		PRES_PT->row
#define         PRES_PT_COL		PRES_PT->column
#define		NEXT_PT			PRES_PT->next
#define		FIRST_PT_ELEV		head->elevation
#define		SECOND_PT		head->next

struct Cell_head window;
char *value;
int nrows, ncols;
SEGMENT in_seg, out_seg;

main(argc,argv)
    int argc;
    char *argv[];
{
    int col, row, 
        len, flag,
        srows, scols,
        elevation_fd, drain_path_fd,
        in_fd, out_fd, 
        stash_away();
   char *current_mapset,
        *search_mapset,
        *drain_path_mapset, 
        *elevation_mapset,
        *in_file, *out_file,
        buf[400];
   CELL *cell;
   struct Colors colors;  
   struct Range range;  
   POINT *PRES_PT, *make_point();

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

/*  Make sure that the window has square grid cells   */
    if (window.ns_res != window.ew_res) 
	{
        sprintf (buf, "Gdrain requires square grid cells");
        G_fatal_error (buf);
        exit(1);
     	}
    
/*   Check for correct no of command line arguments   */
     if(argc < 3) 
	{
       fprintf(stderr,"usage: %s %s\n", argv[0], USAGE);
       exit(-1);
    	}

/*   Do command line parsing	*/
    if (D_parse_command(argc, argv, variables, 
	n_variables, stash_away))
	{
        fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    	}

/*  Check if elevation layer exists in data base  */         
	search_mapset = ""; 
	elevation_mapset = G_find_cell 
			(elevation_layer, search_mapset);
 
    if (elevation_mapset == NULL) 
	{
       sprintf(buf, "%s - not found", elevation_layer);
       G_fatal_error (buf);
       exit(1);
    	}  

    search_mapset = "";
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

    cell = G_allocate_cell_buf();
     
/*  Open elevation cell layer for reading  */
    elevation_fd = G_open_cell_old
			(elevation_layer, elevation_mapset);

    if (elevation_fd < 0) 
	{
        sprintf (buf,
                 "%s in %s -can't open cell file",
                 elevation_layer,
                 elevation_mapset);
        G_fatal_error (buf);
        exit(1);
     	}

/*   Parameters for map submatrices   */
     len = sizeof(CELL);

     srows = nrows/4 + 1;
     scols = ncols/4 + 1;

/* Create segmented files for elev and output layers  */
     in_fd = creat(in_file,0666);
     segment_format(in_fd, nrows, ncols, srows, scols, len);
     close(in_fd);

     out_fd = creat(out_file,0666);   
     segment_format(out_fd, nrows, ncols, srows, scols, len);
     close(out_fd);

/*   Open initialize and segment all files  */
     in_fd = open(in_file,2);
     segment_init(&in_seg,in_fd,4);

     out_fd = open(out_file,2);
     segment_init(&out_seg,out_fd,4);

/*   Write the elevation layer in the segmented file  */
     for( row=0 ; row<nrows ; row++ ) 
	{
        if( G_get_map_row(elevation_fd, cell, row)<0)
        exit(1);
        segment_put_row(&in_seg, cell, row);
        }
								
/* If the output layer containing the marked starting positions*/
/* already exists, create a linked list of starting locations  */
     if (flag == 1) {
         drain_path_fd = G_open_cell_old (drain_path_layer,
					 drain_path_mapset);
    
        if (drain_path_fd < 0) 
	{
        sprintf (buf,
                      "%s in %s -can't open cell file", 
                      drain_path_layer,
                      drain_path_mapset);
         G_fatal_error (buf);
         exit(1);  
         }

/*  Search for the marked starting pts and make list	*/
	for(row = 0; row < nrows; row++)
	{
	    if(G_get_map_row(drain_path_layer,cell,row) < 0)
	    exit(1);

	    for(col = 0; col < ncols; col++)
	    {
		if(cell[col] > 0)
		{
		    POINT *new;
		    new = make_point((POINT *)NULL,row,col,0);
		    if(head_start_pt == NULL)
			head_start_pt = new;
		    else
			NEXT_PT = new;
		    PRES_PT = new;
		}
	    }	/* loop over cols */
	}	/* loop over rows */

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
	value = (char *) &PRES_PT_ELEV;

	segment_get(&in_seg, value, PRES_PT_ROW, PRES_PT_COL);

	drain_path_finder (PRES_PT);

	PRES_PT = NEXT_PT;
	}


/*  Open output layer for writing   */
    drain_path_fd = G_open_cell_new(drain_path_layer);

/*  Write pending updates by segment_put() to outputmap   */ 
    segment_flush(&out_seg);  

    	for ( row=0 ; row<nrows; row++ ) 
	{
        segment_get_row(&out_seg,cell,row);
        if (G_put_map_row(drain_path_fd,cell,row)<0)
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

/*  Create colours for output map    */
    G_read_range (drain_path_layer,current_mapset, &range);
    G_make_color_wave (&colors,range.pmin, range.pmax);
    G_write_colors(drain_path_layer,current_mapset,&colors);

}

/*************** END OF "MAIN" PROGRAM *************************/


/***************************************************************

	This recursive function traces the drainage path
	of a drop of water on an elevation map

****************************************************************/

drain_path_finder (PRES_PT)
	
	POINT *PRES_PT;
	
{
	POINT *head = NULL, *make_neighbors_list();
{ /* start a new block to minimize variable use in recursion */
	int data,row,col;
	value = (char *) &data;

/* if the pt has already been traversed, return			*/ 
	segment_get(&out_seg, value, PRES_PT_ROW, PRES_PT_COL);
	if(data == 1) return;		/* already traversed	*/

/* otherwise, mark on output					*/
	data = 1;
	segment_put(&out_seg, value, PRES_PT_ROW, PRES_PT_COL);

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

		segment_get(&in_seg, value, row, col);

		/* elev of neighbor is higher. i.e. no chance of flow	*/
		if(data > PRES_PT_ELEV) continue;

		/* if elev of neighbor is equal or lower consider for	*/
		/* addition to the list of pts where water will flow	*/
		head = make_neighbors_list(head, row, col, data);

	    }	/* end of "col" loop */
	}	/* end of "row" loop */

	if(head == NULL) return;	/* lowest pt reached */
}

/* process the list of neighboring pts where the water will flow*/
	PRES_PT = head;
	while(PRES_PT != NULL)
	{
	drain_path_finder(PRES_PT);
	PRES_PT = NEXT_PT;
	}	

	free_list(head);	/* free list of neighboring pts*/

	return;
}

/********* END OF FUNCTION "DRAIN_PATH_FINDER" ******************/	



/****************************************************************

	This function makes a list of pts neighboring to the
 	one under consideration where the water	drop would flow
	next.

****************************************************************/

POINT *make_neighbors_list(head, row, col, data)
	POINT *head;
	int row, col, data;
{
	POINT *make_point();
	
	if(head == NULL)	/* first pt	*/
	{
	head = make_point(head,row, col, data);
	}
	else			/* otherwise	*/
	{
	if(data < FIRST_PT_ELEV)
	/* this pt is at a steeper slope than those in list	*/
		{
		/* start new list				*/
		head = make_point(head,row, col, data); 
		}

   /* if pt at same slope as those already in list, add to list */
	else if(data == FIRST_PT_ELEV)
		{POINT *PRES_PT;
		PRES_PT = make_point((POINT *) NULL, row, col, data);
		NEXT_PT = head;
		head = PRES_PT;
		}
	}
	
	return(head);
	
}

/************ END OF FUNCTION "MAKE_NEIGHBORS_LIST"**************/	



/****************************************************************

	This function creates a new point data structure using
	memory from the malloc pool and initializes all fields

****************************************************************/ 

POINT *make_point(PRES_PT, row, col, data)
	POINT *PRES_PT;
	int row, col, data;
{
	if (PRES_PT == NULL)
	    PRES_PT = (POINT *) malloc(sizeof (POINT));

	PRES_PT_ROW = row;
	PRES_PT_COL = col;
	PRES_PT_ELEV = data;
	NEXT_PT = NULL;

	return(PRES_PT);
}

/**************** END OF FUNCTION "MAKE_POINT" ******************/


	
/****************************************************************

	This function frees the memory allocated to a pt list

*****************************************************************/

free_list(head)
	POINT *head;
{
	POINT *PRES_PT;

	PRES_PT = head;
	while(PRES_PT != NULL)
	{
	PRES_PT = SECOND_PT;
	free(head);
	head = PRES_PT;
	}
}
	
/*************** END OF FUNCTION "FREE_LIST" ********************/

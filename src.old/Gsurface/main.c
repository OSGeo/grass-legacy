/***************************************************************
 *
 *	main.c 	in 	~/src/Gsurface
 *
 *	This is the main program for generating a surface
 *	through a set of irregularly spaced data values
 *	using a two-dimensional interpolation technique.
 *
 *	send any comments to "chuck" in cerl via mail.
 ***************************************************************/

#define		USAGE	"input=name  output=name"

#include "gis.h"
#include <math.h>
#define MAIN
#include "stash.h"
#define		SHORT		short

#define POINT	struct point
POINT	{
	SHORT row, col;
	float value;
};

#define NEIGHBOR 	struct neighbor
NEIGHBOR	{
	double distance;
	float value;
	NEIGHBOR *next;
};

#define		INCR		128

static double radius;	/* search radius for finding neighbors	*/
struct Cell_head window ;
CELL *cell;


main(argc, argv)
	int argc ;
	char **argv ;
{
	char *layer_mapset , *current_mapset;
	char buff[128] ;
	POINT *head = NULL, *PRES_PT, *make_point();
	SHORT nrows, ncols, npts, mlow, mhih;	
	SHORT row, col;
	int numalloc;
	int stash_away();
	int fd;
	struct Colors colors;
	struct Range range;


/* Initialize the GIS calls 					*/
	G_gisinit(argv[0]) ;

	current_mapset = G_mapset();


/* Check command line arguments					*/
	if(argc != 3)
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/*  Get database window parameters      			*/
    if(G_get_window (&window) < 0)
        {
       sprintf (buff,"can't read current window parameters");
       G_fatal_error (buff);
       exit(1);
        }

/*  Do command line parsing					*/
        if (D_parse_command(argc, argv, variables,
                                n_variables, stash_away))
        {
        fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE);
               exit(-1) ;
        }


/* Make sure map is available 					*/
        layer_mapset = G_find_cell2 (input_layer, "") ;
        if (layer_mapset == NULL)
        {
        sprintf(buff,"Cellfile [%s] not available", input_layer);
        G_fatal_error(buff) ;
        }

/* check if specified output layer name is legal        	*/
        if (G_legal_filename(output_layer) < 0)
        {
        sprintf (buff, "%s - illegal name", output_layer);
        G_fatal_error (buff);
        exit(1);
        }


/*  find number of rows and columns in window    		*/
    nrows = G_window_rows();
    ncols = G_window_cols();

/*  allocate buffer for row i/o					*/
    cell = G_allocate_cell_buf();

/*  Open input cell layer for reading  				*/
    fd = G_open_cell_old (input_layer, layer_mapset);
 

    if (fd < 0)
        {
        sprintf (buff,
                 "%s in %s -can't open cell file",
                 input_layer,
                 layer_mapset);
        G_fatal_error (buff);
        exit(1);
        }

/* Search and make a list of original data points		*/
	npts = numalloc = 0;
	for(row = 0; row < nrows; row++)
        {
            if(G_get_map_row(fd,cell,row) < 0)
	            exit(1);
            for(col = 0; col < ncols; col++)
            {
                if(cell[col] != 0)
                {
			if(npts++ >= numalloc)
				head = (POINT *) G_realloc(head,
					(numalloc += INCR) * sizeof(POINT));
			addpoint(row,col,(float) cell[col],head,npts);
                }
            }   /* loop over cols */
        }       /* loop over rows */

	G_close_cell(fd);

 
/* decide no: of neighboring pts to be considered for interp 	*/
/*	M = 12;	(old stuff) gives a very smooth surface		*/

	if(npts < 12) mlow = npts;
	else mlow = 12;
	mhih = mlow * 2;

        /*      open cell layer for writing output              */
	fd = G_open_cell_new(output_layer);
	if (fd < 0)
        {
        sprintf(buff, "%s - can't create cell file",output_layer);
        G_fatal_error (buff);
        exit(1);
        }

/* call the interpolation function				*/
	interpolate(head,npts,nrows,ncols,mlow,mhih,fd);

	G_close_cell(fd);

/*      create colours for output map				*/
	G_read_range (output_layer, current_mapset, &range);
	G_make_color_wave(&colors,range.pmin, range.pmax);
    	G_write_colors (output_layer,current_mapset,&colors);

}

/******************* END OF MAIN PROGRAM ************************/



/****************************************************************/

/*	This function fills in the values of all elements in 	*/	
/*	a matrix by interpolating from a given set of 		*/
/*	irregularly spaced data points				*/


interpolate(head, npts, nrows, ncols, mlow,mhih, out_fd)
	POINT head[];
	SHORT nrows, ncols, npts, mlow,mhih;
	int out_fd;
{
	extern double radius;
	extern struct Cell_head window;
	extern CELL *cell;
	int row,col,i;
	NEIGHBOR *nbr_head , *PRES_NEIGHBOR, *make_neighbors_list();
	double sum1, sum2, square, sqrt();

	/* set radius initially to twice value required to	*/
	/* contain M points if points are evenly distributed	*/
	radius = sqrt(2.0 * nrows * window.ns_res 
			  * ncols * window.ew_res / npts);

	for(row=0; row<nrows; row++)	/*  loop over rows	*/
	{

	for(col=0; col<ncols; col++)	/*  loop over columns	*/
	{
	
	/* make a list of neighboring data pts that fall inside	*/
	/* search radius					*/
	nbr_head = make_neighbors_list(head, row, col, mlow,mhih,npts);
	if (nbr_head == NULL) continue;

	/* calculate value to be marked for the cell from the	*/
	/* data values of M closest neighboring pts		*/
	sum1 = 0.0;
	sum2 = 0.0;

	PRES_NEIGHBOR = nbr_head;

	for(i=1; i<= mlow; i++)
	{
	square = PRES_NEIGHBOR->distance * PRES_NEIGHBOR->distance;

	sum1 += PRES_NEIGHBOR->value / square;
	sum2 += 1.0/square;

	PRES_NEIGHBOR = PRES_NEIGHBOR->next;
	}
	/* my stuff while(nbr_head != NULL)	{
		PRES_NEIGHBOR = nbr_head;
		square = PRES_NEIGHBOR->distance * PRES_NEIGHBOR->distance;
		sum1 += PRES_NEIGHBOR->value / square;
		sum2 += 1.0 / square;
		nbr_head = PRES_NEIGHBOR->next;
		free(PRES_NEIGHBOR);
	}	*/
	free_list(nbr_head);
	cell[col] = (CELL) (sum1 / sum2);	

	}	/* end of loop over columns 			*/

	G_put_map_row(out_fd, cell);
	
	}	/* end of loop over rows 			*/

}

/************** END OF FUNCTION "INTERPOLATE" *******************/



/****************************************************************/
/*								*/
/*	This function makes a list of neighboring pts that fall	*/
/*	inside the search radius around a cell whose value is 	*/
/*	to be interpolated using data value of its neighbors	*/

NEIGHBOR *make_neighbors_list(head, row, col, mlow,mhih,npts)
	POINT head[];
	SHORT row, col, mlow,mhih,npts;
{
	extern struct Cell_head window;
	extern double radius;
	extern CELL *cell;
	double distance, del_x, del_y, sqrt();
	NEIGHBOR *nbr_head ,*PRES_NEIGHBOR,*make_neighbor(),*sort_list();
	int nbrs, pres_pt;
	int compare();

	search:
	
	nbrs = 0;	/* no of neighbors			*/
	nbr_head = NULL;
	pres_pt = 0;
	
	/* loop over known data values to find those falling	*/
	/* inside search radius					*/
	while(pres_pt < npts)
	{
		del_x = (col - head[pres_pt].col) * window.ew_res;
		del_y = (row - head[pres_pt].row) * window.ns_res;

		distance = sqrt(del_x * del_x + del_y * del_y);
		if(distance == 0.0) {
			cell[col] = (CELL) head[pres_pt].value;
			return(NULL);
		}
		if(distance <= radius)
		{
			NEIGHBOR *new;
			nbrs++;
			new = make_neighbor(distance, head[pres_pt].value);
			if(nbr_head == NULL)
				nbr_head = new;
			else PRES_NEIGHBOR->next = new;
			PRES_NEIGHBOR = new;
		}
		pres_pt++;
	}	/* end of while */

	/* if less than M pts fall inside search radius, 	*/
	/*	increase radius by 25%				*/
	if(nbrs < mlow)
	{
	radius *= 1.25;
	free_list(nbr_head);
	goto search;
	}

	/* sort total neighbor list in order of increasing dist */
	nbr_head = sort_list(nbrs, nbr_head);

	/* if too many data values picked up, decrease	search	*/
	/* radius by 10%					*/
	if(nbrs > mhih) radius *= 0.9;
	
	return(nbr_head);
}

/******* END OF FUNCTION "MAKE_NEIGHBORS_LIST" ******************/


NEIGHBOR
*make_neighbor(dist, val)	
	double dist;
	float val;

{
	NEIGHBOR *PRES_NEIGHBOR;

	PRES_NEIGHBOR = (NEIGHBOR *) G_malloc(sizeof (NEIGHBOR));

	PRES_NEIGHBOR->distance = dist;
	PRES_NEIGHBOR->value = val;
	PRES_NEIGHBOR->next = NULL;

	return(PRES_NEIGHBOR);
}

/*********** END OF FUNCTION "MAKE_NEIGHBOR" ********************/


 
addpoint(row, col, data, head, npts)
        SHORT npts, row, col; 
	float data;
	POINT head[];
{
        head[--npts].row = row;
        head[npts].col = col;
        head[npts].value = data;
}
 
/**************** END OF FUNCTION "MAKE_POINT" *****************/



free_list(head)		/* frees list of neighboring pts	*/
        NEIGHBOR *head;
{
        NEIGHBOR *PRES_PT;

        PRES_PT = head;
        while(PRES_PT != NULL)
        {
        PRES_PT = head->next;
        free(head);
        head = PRES_PT;
        }
}

/*************** END OF FUNCTION "FREE_LIST" ********************/

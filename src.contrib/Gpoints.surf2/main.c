/***************************************************************
 *
 *	main.c 	
 *
 *	This is the main program for generating a surface
 *	through a set of irregularly spaced data values
 *	using a two-dimensional interpolation technique which 
 *	uses inverse distance squared weighting.
 *
 *	send any comments to koerper@cs.orst.edu
 ***************************************************************/

#include "gis.h"
#include <math.h>
#define MAIN
#include "stash.h"	/* defines MELEMENT */
#define		ROWCOL		short


#define MELEMENT        struct Melement
MELEMENT {
    ROWCOL       x, y;   /* grid coordinates */
    int         value;
    MELEMENT	*nrow, *prow;  /* next and prior element in row list */
    };

#define NEIGHBOR 	struct neighbor
NEIGHBOR {
    double	distance;
    MELEMENT 	*Mptr,		/* pointer to data in linked lists of input */
		**searchptr;	/* row search pointer that identified this
				   neighbor */
    NEIGHBOR 	*next;
    };

#define EW		struct ew
EW {
    MELEMENT	*east, *west, *start;
    EW		*next;
    };


struct Cell_head 	window ;
CELL 			*cell, *mask;
double			*rowdist2, *coldist2,
			ew2;


main(argc, argv)
    int argc ;
    char **argv ;
{
    char *layer_mapset , *current_mapset, *me;
    char buff[128] ;
    MELEMENT *rowlist, *row_lists();
    ROWCOL nrows, ncols, npts, n;
    int stash();
    int fd, maskfd;
    struct Colors colors;
    struct Range range;


/* Initialize the GIS calls 					*/
    G_gisinit(me = argv[0]);
    *input = *output = 0;
    n = G_parse_command (argc, argv, keys, stash);
    if (n != 0 || *input == 0 || *output == 0 || goal <= 0)
    {
        if (n <= 0)
            G_parse_command_usage (me, keys, USAGE_LONG);
        exit(1);
    }
    current_mapset = G_mapset();

/*  Get database window parameters      			*/
    if(G_get_window (&window) < 0)
    {
       sprintf (buff,"can't read current window parameters");
       G_fatal_error (buff);
       exit(1);
    }

/* Make sure layer_map is available 					*/
    layer_mapset = G_find_cell (input,"");
    if (layer_mapset == NULL)
    {
        fprintf (stderr, "%s: %s - cell file not found\n", me, input);
        exit(1);
    }

/* check if specified output layer name is legal        	*/
    if (G_legal_filename(output) < 0)
    {
        fprintf (stderr, "%s: %s - illegal name\n", me, output);
        exit(1);
    }

/*  find number of rows and columns in window    		*/
    nrows = G_window_rows();
    ncols = G_window_cols();

/* create distance squared lookup tables */
    lookup_tables (nrows, ncols);

/*  allocate buffers for row i/o				*/
    cell = G_allocate_cell_buf();
    if ((maskfd = G_maskfd()) >= 0)	/* apply mask to output file */
        mask = G_allocate_cell_buf();
    else
	mask = NULL;

/*  Open input cell layer for reading  				*/
    fd = G_open_cell_old (input, layer_mapset);
    if (fd < 0) {
        sprintf (buff, "%s in %s -can't open cell file", input, layer_mapset);
        G_fatal_error (buff);
        exit(1);
        }

/* Store input data in array-indexed doubly-linked lists and close input file */
    rowlist = row_lists (nrows, ncols, &npts, fd, cell);
    G_close_cell(fd);

 
/* set number of neighboring pts to be considered for interp 	*/
/* goal = 12 gives a very smooth surface and is the default	*/
if(npts < goal) goal = npts;

/*      open cell layer for writing output              */
fd = G_open_cell_new(output);
if (fd < 0) {
    sprintf(buff, "%s - can't create cell file",output);
    G_fatal_error (buff);
    exit(1);
    }

/* call the interpolation function				*/
interpolate(rowlist, nrows, ncols, goal, fd, maskfd);

/* free allocated memory */
free_row_lists (rowlist, nrows);
free (rowdist2);
free (coldist2);
G_close_cell(fd);

/*      create colours for output map				*/
/*
G_read_range (output, current_mapset, &range);
G_make_color_wave(&colors,range.pmin, range.pmax);
G_write_colors (output,current_mapset,&colors);
*/

}

/******************* END OF MAIN PROGRAM ************************/



/****************************************************************/
/*	This function fills in the values of all elements in 	*/	
/*	a matrix by interpolating from a given set of 		*/
/*	irregularly spaced data points				*/


interpolate(rowlist, nrows, ncols, goal, out_fd, maskfd)
MELEMENT	rowlist[];
ROWCOL 		nrows, ncols;
int 		goal, out_fd, maskfd;
{
    extern CELL *cell;
    extern struct Cell_head window;
    extern double		ew2;

    MELEMENT	*Rptr;
    EW		*search, *ewptr,
		*lastrow;	/* last element in search array */
    ROWCOL	row, col, i;
    NEIGHBOR 	*nbr_head , *Nptr, *make_neighbors_list();
    double 	sum1, sum2,
		maxdist = nrows * nrows + ncols * ncols * ew2 + 1.0;
		 /* value greater than squared distance to any data point */
		 /* note that distance units are row (ns) resolution */

    /* initialize search array */
    search = (EW *) G_calloc (nrows, sizeof (EW));
    lastrow = search + nrows - 1;
    fprintf (stderr, "Interpolating %s ...", output);

    for(row=0; row<nrows; row++) {	/*  loop over rows	*/
        G_percent (row, nrows, 2);

	/* if mask occurs, read current row of the mask */
	if (mask && G_get_map_row(maskfd, mask, row) < 0)
            exit(1);

	/* prepare search array for next row of interpolations */
	ewptr = search;
	Rptr = rowlist;
        for (i = 0; i < nrows; i++, Rptr++, ewptr++)
	    ewptr->start = Rptr->nrow;	/* start at first item in row */
 
	for(col=0; col<ncols; col++) {	/*  loop over columns	*/
	
    	    /* don't interpolate outside of the mask */
            if (mask && mask[col] == 0) {
                cell[col] = 0;
                continue;
           	}

	    /* make a list of goal neighboring data pts */
	    nbr_head = make_neighbors_list(search, lastrow, row, col,
	     goal, maxdist);
	    if (nbr_head == NULL)	/* data occurs at this point */
		continue;

	    /* calculate value to be set for the cell from the	*/
	    /* data values of goal closest neighboring points	*/
	    sum1 = sum2 = 0.0;
	    Nptr = nbr_head->next;

	    do {
		sum1 += Nptr->Mptr->value / Nptr->distance;
		sum2 += 1.0 / Nptr->distance;
		Nptr = Nptr->next;
		} while (Nptr);		/* to end of list */

	    free_list(nbr_head);
	    cell[col] = (CELL) (sum1 / sum2);	
	    /* printf ("%d,%d = %d\n", col, row, cell[col]); */ 

	    }	/* end of loop over columns 			*/

	G_put_map_row(out_fd, cell);
	
	}	/* end of loop over rows 			*/

    G_percent (row, nrows, 2);
    free (search);
}

/************** END OF FUNCTION "INTERPOLATE" *******************/



/****************************************************************/
/*								*/
/*	This function makes a list of neighboring pts that fall	*/
/*	inside the search radius around a cell whose value is 	*/
/*	to be interpolated using data value of its neighbors	*/

NEIGHBOR *make_neighbors_list(firstrow, lastrow, row, col, goal, maxdist)
EW	*firstrow, *lastrow;
ROWCOL 	row, col; 
int	goal;
double	maxdist;	/* squared distance of furthest neighbor */
{
    extern struct Cell_head window;
    extern CELL *cell;

    NEIGHBOR 	*head;
    ROWCOL 	neighbors = 0,	/* number of neighbors in current list */
		offset = 1;	/* offset of search boundary from start row */
    EW 		*north, *south, *start;

    /* initialize dummy on neighbors list */
    head = (NEIGHBOR *) G_malloc (sizeof (NEIGHBOR));
    head->next = NULL;
    head->distance = maxdist;

    /* begin north search in the row of the point to be interpolated */
    start = north = firstrow + row;
    first_west (north, col);
    north->next = NULL;

    /* curtail interpolation if this cell has a value */
    if (north->east && north->east->x == col) {
	cell[col] = north->east->value;
	return(NULL);
	}

    /* initialize south search routine */
    if (north == lastrow)
	south = NULL;
    else {
	south = north + 1;
        first_west (south, col);
    	south->next = NULL;
	}

    /*  initialize search cycle pattern */
    find_neighbors (&north->west, head, row, col, goal, &neighbors, 1);
    find_neighbors (&north->east, head, row, col, goal, &neighbors, 0);
    search (&north, head, row, col, goal, &neighbors, firstrow, -1);
    if (south) {
        find_neighbors (&south->west, head, row, col, goal, &neighbors, 1);
        find_neighbors (&south->east, head, row, col, goal, &neighbors, 0);
        }


    /* expand row search north and south until all nearest neighbors
       must occur within the current search boundaries */
    do {
	if (north)
	    search (&north, head, row, col, goal, &neighbors, firstrow, -1);
	if (south)
	    search (&south, head, row, col, goal, &neighbors, lastrow, 1);
	if (neighbors == goal)
	    maxdist = head->next->distance;
	++offset;	/* expand row search boundaries for next loop */
	} while (offset * offset < maxdist);

    /* exhaust search within current search boundaries */
    while (north || south) {
	if (north)
    	    exhaust (&north, start, head, row, col);
	if (south)
    	    exhaust (&south, start, head, row, col);
	} 
	
    return(head);	/* head points to dummy plus goal neighbors */
}

/******* END OF FUNCTION "MAKE_NEIGHBORS_LIST" ******************/


search (ewptr, head, row, col, goal, neighbors, boundary, south)
EW		**ewptr;		/* double-indirection !! */
NEIGHBOR	*head;	
ROWCOL		row, col, 
		*neighbors;
int		goal;
EW		*boundary;
ROWCOL		south;		/* search proceeds southward if == 1 */
{
    ROWCOL	new = 0;	/* no prior search in first row in list */
    EW		*current, *prior, *next_row();

    /* reset ewptr if row it points to has been thoroughly searched */
    while (!new && !(*ewptr)->west && !(*ewptr)->east) {
	*ewptr = next_row (*ewptr, boundary, &new, south);
	if (!*ewptr)
	    return;
	}
    current = *ewptr;

    /* process rows where search has already been initiated */
    while (!new && current) {
    	find_neighbors (&current->west, head, row, col, goal, neighbors, 1);
        find_neighbors (&current->east, head, row, col, goal, neighbors, 0);
	prior = current;
	current = next_row (current, boundary, &new, south);
	prior->next = current;
	}

    /* bound search path if no new rows available */
    if (!new) {
	prior->next = prior;
        return;
	}

    /* initiate search in next available row */
    first_west (current, col);
    current->next = NULL;
    find_neighbors (&current->west, head, row, col, goal, neighbors, 1);
    find_neighbors (&current->east, head, row, col, goal, neighbors, 0);
}


exhaust (ewptr, start, head, row, col)
EW		**ewptr,		/* double-indirection !! */
		*start;
NEIGHBOR	*head;	
ROWCOL		row, col;
{
    EW		*current;
    int		offset = *ewptr - start;

    /* check if further searching in this direction is necessary */
    if (offset * offset >=  head->next->distance) {
	*ewptr = NULL;
	return;
	}

    /* process row where search has already been initiated */
    exhaust_search (*ewptr, head, row, col);

    /* reset ewptr to point to a row that has not been thoroughly searched */
    do {
        current = *ewptr;
	*ewptr = (*ewptr)->next;
	if (*ewptr == current)
	    *ewptr = NULL;
	if (!*ewptr)
	    return;
        } while (!(*ewptr)->west && !(*ewptr)->east);
}


EW *next_row (ewptr, boundary, new, south)
EW	*ewptr,
	*boundary;	/* row boundary of map in search direction */
ROWCOL	*new,
	south;			/* search proceeds southward if == 1 */
{
    if (ewptr->next)
        if (ewptr->next == ewptr) 	/* signals end of this search */
	    return (NULL);
	else
	    return (ewptr->next);
    else if (ewptr != boundary) {
	    *new = 1;	/* call first_west before finding neighbors */
	    return (ewptr += south);
	    }
    else 
	return (NULL);
}


/************************************************************************/
/*	This function initializes the search for nearest neighbors	*/
/*	by locating the two data closest to the specified column in	*/
/*	a linked list of row data					*/

first_west (ewptr, col)
EW		*ewptr;
ROWCOL 		col;
{
    if (ewptr->start == NULL)		/* no data in this row */
	ewptr->west = ewptr->east = NULL;
    else if (col < ewptr->start->x) {	/* ewptr->start is west of point */
	ewptr->west = ewptr->start;
	ewptr->east = ewptr->west->prow;
	}
    else {				/* reset search start for this row */
	while (ewptr->start->nrow && col >= ewptr->start->x)
	 /* not at end of list and east of interpolation point */
	    ewptr->start = ewptr->start->nrow;
	if (col >= ewptr->start->x) {	/* at end of rowlist */
	    ewptr->east = ewptr->start;
	    ewptr->west = NULL;
	    }
	else {
	    ewptr->west = ewptr->start;
	    ewptr->east = ewptr->start->prow;
	    }
	}
}


/************************************************************************/
/*	This function evaluates nearest neighbor status for a given	*/
/*	datum and resets the row search pointer based on the result	*/

find_neighbors (Mptr, nbr_head, row, col, goal, neighbors, westward)
MELEMENT	**Mptr;		/* double-indirection !! */
NEIGHBOR	*nbr_head;	
int		goal;
ROWCOL		row, col, 
		*neighbors,
		westward;	/* 1 if west of interpolation point */
{
    double	distance, triangulate();

    if (*Mptr) {		/* not NULL */
	distance = triangulate (*Mptr, row, col);

	if (*neighbors < goal) {
	    add_neighbor (Mptr, nbr_head, distance);
	    ++(*neighbors);
	    }
	else if (!replace_neighbor (Mptr, nbr_head, distance)) {
	    *Mptr = NULL;	/* curtail search in this direction */
	    return;
	    }

	if (westward)
	    *Mptr = (*Mptr)->nrow;
	else
	    *Mptr = (*Mptr)->prow;
	}
}


/************************************************************************/
/*      This function exhausts all possible nearest neighhbors          */
/*      within the row indexed by the ew search pointer                 */

exhaust_search (ewptr, nbr_head, row, col)
EW              *ewptr;
NEIGHBOR        *nbr_head;
ROWCOL           row, col;
{
    double      distance, triangulate();

    while (ewptr->west) {               /* not NULL */
        distance = triangulate (ewptr->west, row, col);

        if (!replace_neighbor (&ewptr->west, nbr_head, distance)) 
            break; /* curtail search in this direction */
        else
            ewptr->west = ewptr->west->nrow;
        }

    while (ewptr->east) {               /* not NULL */
        distance = triangulate (ewptr->east, row, col);

        if (!replace_neighbor (&ewptr->east, nbr_head, distance)) 
            break; /* curtail search in this direction */
        else
            ewptr->east = ewptr->east->prow;
        }
}



/************************************************************************/
/*	triangulate returns the square of the hypoteneuse     		*/

double triangulate (Mptr, row, col)
MELEMENT	*Mptr;
ROWCOL		row, col;
{
    extern double	*rowdist2, *coldist2;
    int			rowoff, coloff;

    rowoff = abs (row - Mptr->y);
    coloff = abs (col - Mptr->x);
    return (*(rowdist2 + rowoff) + *(coldist2 + coloff));
}
    

add_neighbor (Mptr, nptr, distance)
MELEMENT	**Mptr;		/* double-indirection!! */
NEIGHBOR	*nptr;
double		distance;
{
    NEIGHBOR	*new;

    new = (NEIGHBOR *) G_malloc (sizeof (NEIGHBOR));
    new->distance = distance;
    new->Mptr = *Mptr;		/* points to row_list element */
    new->searchptr = Mptr;	/* points to east or west field of an EW */

    while (nptr->next && nptr->next->distance > distance)
	nptr = nptr->next;
    new->next = nptr->next;
    nptr->next = new;
}


int replace_neighbor (Mptr, nbr_head, distance)
MELEMENT	**Mptr;		/* double-indirection!! */
NEIGHBOR	*nbr_head;	
double		distance;
{
    NEIGHBOR	*furthest;

    furthest = nbr_head->next;
    if (distance < furthest->distance) {
	/* replace furthest neighbor and sort neighbor list */
	*(furthest->searchptr) = NULL;	
	  /* all other data in this direction are more distant */
	furthest->distance = distance;
	furthest->Mptr = *Mptr;		/* points to row_list element */
	furthest->searchptr = Mptr;
	 /* points to east or west field of an EW */
	
	/* keep neighbors list in descending order of distance */
	if (furthest->distance < furthest->next->distance)
	    sort_neighbors (nbr_head, distance);
	return (1);
	}
    else 
	return (0);
}
		

sort_neighbors (nbr_head, distance)
NEIGHBOR	*nbr_head;
double		distance;
{
    NEIGHBOR	*nptr, *new;

    new = nbr_head->next;
    nptr = nbr_head->next->next;

    while (nptr->next && nptr->next->distance > distance)
	nptr = nptr->next;
    nbr_head->next = new->next;
    new->next = nptr->next;
    nptr->next = new;
}

 

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


free_row_lists (rowlist, nrows)		/* frees indexed row lists of data */
MELEMENT	*rowlist;
ROWCOL		nrows;
{
    int		i;
    MELEMENT	*Mptr, *prev;

    for (i = 0; i < nrows; i++) {
	Mptr = (rowlist + i)->nrow;
	while (prev = Mptr) {
	    Mptr = Mptr->nrow;
	    free (prev);
	    }
	}
    free (rowlist);
}


MELEMENT *row_lists (rows, cols, npts, fd, cell)
/* Search and make array-indexed doubly-linked lists of original data points */
ROWCOL		rows, cols,	/* total rows and columns in window */
		*npts;		/* number of data points available */
int		fd;		/* file descriptor, input */
CELL		*cell;		/* array of data for a single row */
{
    int		row, col;	/* row and column indices */
    MELEMENT	*rowlist,	/* root of rowlist data structure index array */
		*Mptr,		/* pointer to a rowlist element */
		*Rptr;		/* pointer to a row list dummy */

    /* initialize row array (allocated memory set to zero),
       each dummy points to itself as last element entered in matrix */
    rowlist = (MELEMENT *) G_calloc (rows, sizeof (MELEMENT));
    for (row = 0, Rptr = rowlist; row < rows; row++, Rptr++)
	Rptr->prow = Rptr;	

    /* enter data by allocation of individual matrix elements */
    *npts = 0;
    fprintf (stderr, "Reading %s ...", input);

    for(row = 0, Rptr = rowlist; row < rows; row++, Rptr++) {
        G_percent (row, rows, 2);
        if(G_get_map_row_nomask (fd,cell,row) < 0)
            exit(1);
        for(col = 0; col < cols; col++) {
            if(cell[col] != 0) {
		++(*npts);
		Mptr = (MELEMENT *) G_malloc (sizeof (MELEMENT));
		Mptr->x = col;
		Mptr->y = row;
		Mptr->value = cell[col];

		/* doubly link new element to rowlist */
		Mptr->prow = Rptr->prow;
		Rptr->prow = (Rptr->prow->nrow = Mptr);
                }
            }   /* loop over cols */
        }       /* loop over rows */
    G_percent (row, rows, 2);

    /* add final link to complete doubly-linked lists */
    for (row = 0, Rptr = rowlist; row < rows; row++, Rptr++)
	if (Rptr->prow == Rptr)		/* no data in this row */
	    Rptr->prow = NULL;
	else {	/* place NULL sentinel at each end of list */
            Rptr->nrow->prow = NULL;     
            Rptr->prow->nrow = NULL;     
	    }

    return (rowlist);
}    	


/************************************************************************/
/*      Lookup tables containing distance squared (in units of ns.res)  */
/*      are created for later use in selecting nearest neighbors        */

lookup_tables (nrows, ncols)
ROWCOL	nrows, ncols;
{
    extern double		*rowdist2, *coldist2, ew2;
    extern struct Cell_head	window;
    double			*nextrow, *nextcol;
    int				i;

    /* calculate square of resolution ratio ew : ns */
    ew2 = window.ew_res / window.ns_res * window.ew_res / window.ns_res;

    nextrow = rowdist2 = (double *) G_calloc (nrows, sizeof (double));
    for (i = 0; i < nrows; i++, nextrow++) 
	*nextrow = (double) i * i;

    nextcol = coldist2 = (double *) G_calloc (ncols, sizeof (double));
    for (i = 0; i < ncols; i++, nextcol++) 
	*nextcol = (double) i * i * ew2;
}

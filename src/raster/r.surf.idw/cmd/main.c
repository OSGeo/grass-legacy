/***************************************************************
 
 	This is the main program for generating a surface
 	through a set of irregularly spaced data values
 	using a two-dimensional interpolation technique which 
 	uses inverse distance squared weighting.
 
910110  extended to latitude/longitude projection type
910115  modified rowlist array to include only those rows with data
910124  added -e flag option for output of error associated with
         interpolation of known cell values

 	send any comments to koerper@cs.orst.edu
 ***************************************************************/

#include <stdlib.h>
#include <math.h>
#include "gis.h"
#define MAIN
#include "main.h"

static int error_flag = 0;
static char *input;
static char *output;

int main (int argc, char **argv)
{
    char *layer_mapset , *current_mapset;
    char buff[128] ;
    MELEMENT *rowlist;
    SHORT nrows, ncols;
    SHORT datarows;
    int npoints;
	struct GModule *module;
    struct
    {
	struct Option *input, *output, *npoints;
    } parm;
    struct
    {
	struct Flag *e;
    } flag;
    int n, fd, maskfd;

/* Initialize the GIS calls 					*/
    G_gisinit(argv[0]);

	module = G_define_module();
    module->description =
		"Surface interpolation utility for raster map layers.";

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->type = TYPE_STRING;
    parm.input->required = YES;
    parm.input->description = "input raster map";
    parm.input->gisprompt = "old,cell,raster";

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->type = TYPE_STRING;
    parm.output->required = YES;
    parm.output->description = "output raster map";
    parm.output->gisprompt = "any,cell,raster";

    parm.npoints = G_define_option();
    parm.npoints->key = "npoints";
    parm.npoints->type = TYPE_INTEGER;
    parm.npoints->required = NO;
    parm.npoints->description = "number of interpolation points";
    parm.npoints->answer = "12";

    flag.e = G_define_flag();
    flag.e->key = 'e';
    flag.e->description = "output is the interpolation error";

    if (G_parser(argc,argv))
	exit(1);
    if (sscanf(parm.npoints->answer, "%d", &n) != 1 || n <= 0)
    {
	fprintf (stderr, "%s=%s - illegal value\n", parm.npoints->key, parm.npoints->answer);
	G_usage();
	exit(1);
    }
    npoints    = n;
    error_flag = flag.e->answer;
    input      = parm.input->answer;
    output     = parm.output->answer;

    current_mapset = G_mapset();

/*  Get database window parameters      			*/
    if(G_get_window (&window) < 0)
    {
       G_fatal_error ("can't read current region parameters");
       exit(1);
    }

/* Make sure layer_map is available 					*/
    layer_mapset = G_find_cell (input,"");
    if (layer_mapset == NULL)
    {
        fprintf (stderr, "%s: %s - raster map not found\n", G_program_name(), input);
        exit(1);
    }

/* check if specified output layer name is legal        	*/
    if (G_legal_filename(output) < 0)
    {
        fprintf (stderr, "%s: %s - illegal name\n", G_program_name(), output);
        exit(1);
    }

/*  find number of rows and columns in window    		*/
    nrows = G_window_rows();
    ncols = G_window_cols();

/* create distance squared or latitude lookup tables */
/* initialize function pointers */
    lookup_and_function_ptrs (nrows, ncols);

/*  allocate buffers for row i/o				*/
    cell = G_allocate_cell_buf();
    if ((maskfd = G_maskfd()) >= 0 || error_flag) {  /* apply mask to output */
    	if (error_flag) 	/* use input as mask when -e option chosen */
    	    maskfd = G_open_cell_old (input, layer_mapset);
        mask = G_allocate_cell_buf();
    }
    else
	mask = NULL;

/*  Open input cell layer for reading  				*/
    fd = G_open_cell_old (input, layer_mapset);
    if (fd < 0) {
        sprintf (buff, "%s in %s -can't open raster file", input, layer_mapset);
        G_fatal_error (buff);
        exit(1);
    }

/* Store input data in array-indexed doubly-linked lists and close input file */
    rowlist = row_lists (nrows, ncols, &datarows, &n, fd, cell);
    G_close_cell(fd);
    if (npoints > n)
	npoints = n;

 
/* open cell layer for writing output              */
    fd = G_open_cell_new(output);
    if (fd < 0) {
        sprintf(buff, "%s - can't create cell file",output);
        G_fatal_error (buff);
        exit(1);
    }

/* call the interpolation function				*/
    interpolate (rowlist, nrows, ncols, datarows, npoints, fd, maskfd);

/* free allocated memory */
    free_row_lists (rowlist, nrows);
    free (rowlook);
    free (collook);
    if (ll)
	free_dist_params ();
    G_close_cell(fd);

    exit(0);
}

/******************* END OF MAIN PROGRAM ************************/

/****************************************************************/
/*	The content of lookup tables created for distance       */	
/*	calculations depends on the projection type; function   */
/*	pointers are set dependent on projection type		*/

int lookup_and_function_ptrs (SHORT nrows, SHORT ncols)
{
    double a, e2;	/* used to control geodetic distance calculations */

    if ((ll = (G_projection () == PROJECTION_LL))) {
        /* equivalent to G_begin_distance_calculations () */
        G_get_ellipsoid_parameters (&a, &e2);
        G_begin_geodesic_distance_l (nrows,a, e2);
    
        LL_lookup_tables (nrows, ncols);
        init_row_search = first_west_LL;
        comp_row_search = completed_row_LL;
        locate_neighbors = find_neighbors_LL;
        check_offset = offset_distance_LL;
        exhaust_row = exhaust_search_LL;
        }
    else {  
        lookup_tables (nrows, ncols);
        init_row_search = first_west;
        comp_row_search = completed_row;
        locate_neighbors = find_neighbors;
        check_offset = offset_distance;
        exhaust_row = exhaust_search;
	}

	return 0;
}


/****************************************************************/
/*	This function fills in the values of all elements in 	*/	
/*	a matrix by interpolating from a given set of 		*/
/*	irregularly spaced data points				*/


int 
interpolate (MELEMENT rowlist[], SHORT nrows, SHORT ncols, SHORT datarows, int npoints, int out_fd, int maskfd)
{
    extern CELL *cell;

    MELEMENT	*Rptr;
    EW		*search, *ewptr,
		*current_row,	/* start row for north/south search */
		*lastrow;	/* last element in search array */
    SHORT	row, col;
    NEIGHBOR 	*nbr_head , *Nptr;
    double 	sum1, sum2;

    /* initialize search array and neighbors array */
    current_row = search = (EW *) G_calloc (datarows, sizeof (EW));
    lastrow = search + datarows - 1;
    nbr_head = (NEIGHBOR *) G_calloc (npoints + 1, sizeof (NEIGHBOR));
    /*nbr_head->distance = maxdist;
    nbr_head->searchptr = &(nbr_head->Mptr);      /* see replace_neighbor */

    fprintf (stderr, "Interpolating raster map <%s> ... %d rows ... ", output, nrows);

    for(row=0; row<nrows; row++) {	/*  loop over rows	*/
	fprintf (stderr, "%-10d\b\b\b\b\b\b\b\b\b\b", nrows-row);

	/* if mask occurs, read current row of the mask */
	if (mask && G_get_map_row(maskfd, mask, row) < 0)
            exit(1);

	/* prepare search array for next row of interpolations */
	for (ewptr = search, Rptr = rowlist; ewptr <= lastrow; Rptr++, ewptr++)
	    ewptr->start = Rptr->next;	/* start at first item in row */
 
	for(col=0; col<ncols; col++) {	/*  loop over columns	*/

	    /* if (row != 279 && col != 209) continue; */

    	    /* don't interpolate outside of the mask */
            if (mask && mask[col] == 0) {
                cell[col] = 0;
                continue;
           	}

	    /* make a list of npoints neighboring data pts */
    	    nbr_head->next = NULL;
	    if (make_neighbors_list (search, lastrow, current_row, row, col,
	     nbr_head, npoints)) {    /* otherwise, known data value assigned */

		/* calculate value to be set for the cell from the data values
		   of npoints closest neighboring points	*/
		sum1 = sum2 = 0.0;
		Nptr = nbr_head->next;

		do {
		    sum1 += Nptr->Mptr->value / Nptr->distance;
		    sum2 += 1.0 / Nptr->distance;
		    Nptr = Nptr->next;
		    } while (Nptr);		/* to end of list */

		cell[col] = (CELL) (sum1 / sum2 + .5);	
		/* fprintf (stdout,"%d,%d = %d\n", col, row, cell[col]); */

		if (error_flag)	/* output interpolation error for this cell */
		    cell[col] -= mask[col];
		}
       	    }	/* end of loop over columns */

	G_put_map_row(out_fd, cell);

	/* advance current row pointer if necessary */
	if (current_row->start->y == row && current_row != lastrow)
	    ++current_row;
	}			/* end of loop over rows */

    fprintf (stderr, "done          \n");
    G_free (search);

	return 0;
}

/************** END OF FUNCTION "INTERPOLATE" *******************/



/****************************************************************/
/*								*/
/*	This function makes a list of neighboring pts that fall	*/
/*	inside the search radius around a cell whose value is 	*/
/*	to be interpolated using data value of its neighbors	*/

int 
make_neighbors_list (
    EW *firstrow,
    EW *lastrow,
    EW *curr_row,
    SHORT row,
    SHORT col,
    NEIGHBOR *head,	/* head points to dummy plus npoints neighbors */
    int npoints
)
{
    extern CELL			*cell;

    SHORT 	neighbors = 0,	/* number of neighbors in current list */
		nsearch = 1, ssearch = 1;    /* expand search north and south */
    EW 		*north, *south;

    /* begin north search in the row of the point to be interpolated */
    north = curr_row;
    (*init_row_search) (north, col);
    north->next = NULL;

    /* curtail interpolation if this cell has a value and not -e option */
    if (north->east && north->east->x == col && north->east->y == row) 
	if (error_flag)	/* ignore value and interpolate */
	    if (ll)
		extend_east (north);
	    else
		north->east = north->east->next;
	else {			/* no interpolation required */
	    cell[col] = north->east->value;
	    return (0);
	    }

    /* initialize south search routine */
    if (north == lastrow)
	south = NULL;
    else {
	south = north + 1;
        (*init_row_search) (south, col);
    	south->next = NULL;
	}

    /*  initialize search cycle pattern */
    (*locate_neighbors) (north, head, row, col, npoints, &neighbors);
    search (&north, head, row, col, npoints, &neighbors, firstrow, -1);
    if (south)
    	(*locate_neighbors) (south, head, row, col, npoints, &neighbors);


    /* expand row search north and south until all nearest neighbors must occur 
       within the current search boundaries, then exhaust search region */
    do {
	if (north)
	    if (nsearch)
		nsearch = search (&north, head, row, col, npoints, &neighbors,
		 firstrow, -1);
	    else
		exhaust (&north, head, row, col);
	if (south)
	    if (ssearch)
		ssearch = search (&south, head, row, col, npoints, &neighbors,
		 lastrow, 1);
	    else
		exhaust (&south, head, row, col);
	} while (north || south);

    return (1);	
}

/******* END OF FUNCTION "MAKE_NEIGHBORS_LIST" ******************/


int 
search (
    EW **ewptr,		/* double-indirection !! */
    NEIGHBOR *head,
    SHORT row,
    SHORT col,
    int npoints,
    SHORT *neighbors,
    EW *boundary,
    SHORT south		/* search proceeds southward if == 1 */
)
{
    SHORT	new = 0;	/* no prior search in first row in list */
    EW		*current, *prior;

    /* reset ewptr if row it points to has been thoroughly searched */
    while (!new && (*comp_row_search) (*ewptr)) {
	*ewptr = next_row (*ewptr, boundary, &new, south);
	if (!*ewptr)
	    return (0);		/* search in this direction is completed */
	}
    current = *ewptr;
    prior = NULL;

    /* process rows where search has already been initiated */
    while (!new && current) {
    	(*locate_neighbors) (current, head, row, col, npoints, neighbors);
	prior = current;
	current = next_row (current, boundary, &new, south);
	prior->next = current;
	}

    /* bound search path if no new rows available */
    if (!new) {
	prior->next = prior;
        return (0);		/* exhaust search region in this direction */
	}

    /* check if new row could contain a nearest neighbor */
    if ((*check_offset) (abs ((int) (row - current->start->y))) >=
     head->next->distance) {
	if (prior && current != *ewptr)
	    /* latter condition to assure prior set in this call to search */
	    prior->next = prior;
	current = NULL;
	return (0);		/* exhaust search region in this direction */
	}

    /* initiate search in next available row */
    (*init_row_search) (current, col);
    current->next = NULL;
    (*locate_neighbors) (current, head, row, col, npoints, neighbors);

    return (1);		/* search expansion continues in this direction */
}


int 
exhaust (
    EW **ewptr,		/* double-indirection !! */
    NEIGHBOR *head,
    SHORT row,
    SHORT col
)
{
    EW		*current;

    /* check if further searching in this direction is necessary */
    if ((*check_offset) (abs ((int) (row - (*ewptr)->start->y))) >=
     head->next->distance) {
	*ewptr = NULL;
	return 0;
	}

    /* process row where search has already been initiated */
    (*exhaust_row) (*ewptr, head, row, col);

    /* reset ewptr to point to a row that has not been thoroughly searched */
    do {
        current = *ewptr;
	*ewptr = (*ewptr)->next;
	if (*ewptr == current)
	    *ewptr = NULL;
	if (!*ewptr)
	    return 0;
        } while ((*comp_row_search) (*ewptr));

	return 0;
}

double offset_distance (SHORT offset)
{
    return (offset * offset);   /* compare squared distances in this case */
}


/************************************************************************/
/*	Return TRUE if search is exhausted both west and east in a row  */

int 
completed_row (EW *ewptr)
{
        return (!ewptr->west && !ewptr->east);
}


EW *
next_row (
    EW *ewptr,
    EW *boundary,	/* row boundary of map in search direction */
    SHORT *new,
    SHORT south			/* search proceeds southward if == 1 */
)
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

int first_west (EW *ewptr, SHORT col)
{
    if (ewptr->start == NULL) {         /* no data in this row */
        ewptr->west = ewptr->east = NULL;
        return 0;
        }

    /* not at end of list and west of interpolation point */
    while (ewptr->start->next && col > ewptr->start->x)
        ewptr->start = ewptr->start->next;

    ewptr->east = ewptr->start;
    ewptr->west = ewptr->start->prior;
        return 0;
}


/************************************************************************/
/*	This function evaluates nearest neighbor status for a given	*/
/*	datum and resets the row search pointer based on the result	*/

int 
find_neighbors (EW *ewptr, NEIGHBOR *nbr_head, SHORT row, SHORT col, int npoints, SHORT *neighbors)
{
    MELEMENT    **Mptr;		/* double indirection !! */ 
    int 	westward = 1;       /* 1 if west of interpolation point */
    double      distance;

    Mptr = &ewptr->west;	/* process search west first, then east */
    do {
    	if (*Mptr) {                /* not NULL */
            distance = triangulate (*Mptr, row, col);

            if (*neighbors < npoints) 
            	add_neighbor (Mptr, nbr_head, distance, ++(*neighbors));
            else if (!replace_neighbor (Mptr, nbr_head, distance)) 
            	*Mptr = NULL;       /* curtail search in this direction */
            
	    if (*Mptr)
        	if (westward)
            	    *Mptr = (*Mptr)->prior;
        	else
            	    *Mptr = (*Mptr)->next;
            }
	
	Mptr = &ewptr->east;
	} while (westward--);	/* repeat loop for east and quit */
        return 0;
}


/************************************************************************/
/*      This function exhausts all possible nearest neighhbors          */
/*      within the row indexed by the ew search pointer                 */

int exhaust_search (EW *ewptr, NEIGHBOR *nbr_head, SHORT row, SHORT col)
{
    double      distance;

    while (ewptr->west) {               /* not NULL */
        distance = triangulate (ewptr->west, row, col);

        if (!replace_neighbor (&ewptr->west, nbr_head, distance)) 
            break; /* curtail search in this direction */
        else
            ewptr->west = ewptr->west->prior;
        }

    while (ewptr->east) {               /* not NULL */
        distance = triangulate (ewptr->east, row, col);

        if (!replace_neighbor (&ewptr->east, nbr_head, distance)) 
            break; /* curtail search in this direction */
        else
            ewptr->east = ewptr->east->next;
        }
        return 0;
}



/************************************************************************/
/*	triangulate returns the square of the hypoteneuse     		*/

double 
triangulate (MELEMENT *Mptr, SHORT row, SHORT col)
{
    extern double	*rowlook, *collook;
    int			rowoff, coloff;

    rowoff = abs (row - Mptr->y);
    coloff = abs (col - Mptr->x);
    return (*(rowlook + rowoff) + *(collook + coloff));
}
    

int 
add_neighbor (
    MELEMENT **Mptr,		/* double-indirection!! */
    NEIGHBOR *nptr,
    double distance,
    int neighbors
)
{
    NEIGHBOR	*new;

    new = nptr + neighbors;	/* offset from base of neighbors array */
    new->distance = distance;
    new->Mptr = *Mptr;		/* points to row_list element */
    new->searchptr = Mptr;	/* points to east or west field of an EW */

    while (nptr->next && nptr->next->distance > distance)
	nptr = nptr->next;
    new->next = nptr->next;
    nptr->next = new;
        return 0;
}


int 
replace_neighbor (
    MELEMENT **Mptr,		/* double-indirection!! */
    NEIGHBOR *nbr_head,
    double distance
)
{
    NEIGHBOR	*furthest;

    furthest = nbr_head->next;
    if (distance < furthest->distance) { /* replace furthest neighbor */
	/* this slight efficiency is not available to LL */
	if (!ll)	/* all other data in this direction are more distant */
	    *(furthest->searchptr) = NULL;	

	furthest->distance = distance;
	furthest->Mptr = *Mptr;		/* points to row_list element */
	furthest->searchptr = Mptr;  /* points to east or west field of an EW */
	
	/* keep neighbors list in descending order of distance */
	if (furthest->next && (furthest->distance < furthest->next->distance))
	    sort_neighbors (nbr_head, distance);
	return (1);
	}
    else 
	return (0);
}
		

int 
sort_neighbors (NEIGHBOR *nbr_head, double distance)
{
    NEIGHBOR	*nptr, *new;

    new = nbr_head->next;
    nptr = nbr_head->next->next;

    while (nptr->next && nptr->next->distance > distance)
	nptr = nptr->next;
    nbr_head->next = new->next;
    new->next = nptr->next;
    nptr->next = new;
        return 0;
}

 

int 
free_row_lists (		/* frees indexed row lists of data */
    MELEMENT *rowlist,
    SHORT nrows
)
{
    int		i;
    MELEMENT	*Mptr, *prev;

    for (i = 0; i < nrows; i++) {
	Mptr = (rowlist + i)->next;
	if (ll && Mptr)
	    Mptr->prior->next = NULL;
	while ((prev = Mptr)) {
	    Mptr = Mptr->next;
	    G_free (prev);
	    }
	}
    G_free (rowlist);
        return 0;
}


MELEMENT *
row_lists (
/* Search and make array-indexed doubly-linked lists of original data points */
    SHORT rows,
    SHORT cols,	/* total rows and columns in window */
    SHORT *datarows,	/* number of rows with non-zero input data */
    int *npts,		/* number of data points available */
    int fd,		/* file descriptor, input */
    CELL *cell		/* array of data for a single row */
)
{
    int		row, col;	/* row and column indices */
    MELEMENT	*rowlist,	/* root of rowlist data structure index array */
		*endlist,
		*Mptr,		/* pointer to a rowlist element */
		*Rptr;		/* pointer to a row list dummy */

    /* initialize row array (allocated memory set to zero),
       each dummy points to itself as last element entered in matrix */
    rowlist = (MELEMENT *) G_calloc (rows, sizeof (MELEMENT));
    for (row = 0, Rptr = rowlist; row < rows; row++, Rptr++)
	Rptr->prior = Rptr;	

    /* enter data by allocation of individual matrix elements */
    *npts = 0;
    fprintf (stderr, "Reading %s ...", input);

    for(row = 0, Rptr = rowlist; row < rows; row++) {
        G_percent (row, rows, 1);
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
		Mptr->prior = Rptr->prior;
		Rptr->prior = (Rptr->prior->next = Mptr);
                }
            }   /* loop over cols */

	if (Rptr->prior != Rptr)	/* non-zero input data in this row */
	    Rptr++->y = row;
        }       		/* loop over rows */
    G_percent (row, rows, 1);
    endlist = Rptr;	/* point to element after last valid row list dummy */

    /* add final link to complete doubly-linked lists */
    for (Rptr = rowlist; Rptr != endlist; Rptr++)
	if (ll) {	/* make the list circular */
	    Rptr->next->prior = Rptr->prior;
            Rptr->prior->next = Rptr->next;
	    }
	else 	/* place NULL sentinel at each end of list */
            Rptr->next->prior = Rptr->prior->next = NULL;     

    *datarows = endlist - rowlist;	/* number of non-zero input data rows */
    return (rowlist);
}    	


/************************************************************************/
/*      Lookup tables containing distance squared (in units of ns.res)  */
/*      are created for later use in selecting nearest neighbors        */

int 
lookup_tables (SHORT nrows, SHORT ncols)
{
    extern double		*rowlook, *collook, ew2;
    extern struct Cell_head	window;
    double			*nextrow, *nextcol;
    int				i;

    /* calculate square of resolution ratio ew : ns */
    ew2 = window.ew_res / window.ns_res * window.ew_res / window.ns_res;

    nextrow = rowlook = (double *) G_calloc (nrows, sizeof (double));
    for (i = 0; i < nrows; i++, nextrow++) 
	*nextrow = (double) i * i;

    nextcol = collook = (double *) G_calloc (ncols, sizeof (double));
    for (i = 0; i < ncols; i++, nextcol++) 
	*nextcol = (double) i * i * ew2;
        return 0;
}

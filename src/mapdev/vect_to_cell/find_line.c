/* @(#)find_line.c	2.2   9/21/87 */
#include "bmif.h"
#include "gis.h"

/*
#define	DEBUG
*/

find_line(num_verticies, xarr, yarr)
	double *xarr, *yarr ;
	int num_verticies ;
{
    int node ;

    int  x,  y,  x_end,  y_end ;
    int  xinc,  yinc,  error ;
    int  delta_x,  delta_y ;

    /* adjust Y grid coordinates to Y array coordinates */
    /*
    line_yadjust( num_verticies, yarr) ;
    */


    store_xy( ifloor (xarr[0]) , ifloor (yarr[0]) ) ;
    n_elements = 0 ;

    num_verticies-- ;    /* Adjustment for the number of vert pairs */
    for (node=0; node<num_verticies; node++)
    {
	/*
	x = (int)(xarr[node]);
	x_end = (int)(xarr[node+1]);
	y = (int)(yarr[node]);
	y_end = (int)(yarr[node+1]);
	*/
	x = ifloor (xarr[node]);
	x_end = ifloor (xarr[node+1]);
	y = ifloor (yarr[node]);
	y_end = ifloor (yarr[node+1]);

	if (x == x_end && y == y_end)
	{
	    store_xy (x, y);
	    continue ;
	}

	bres_line (x, y, x_end, y_end);
    }

}		/*  main()  */


static  int  first_time = 1 ;

store_xy( col, row)
	int  col, row ;
{
	row_put (row, col);
}

#ifdef OLD_STORE
static
store_xy( col, row)
	int  col, row ;
{

	char *G_realloc() ;

#ifdef  DEBUG 
fprintf(stderr," store   row: %d,  col: %d\n", row, col) ;
#endif  DEBUG 

	if( first_time )
	{
		first_time = 0 ;
		n_elem_alloc  = 5000 ;
		xy = (struct element *) G_calloc(n_elem_alloc, sizeof(struct element)) ;
	}

	/*  check space  */
	if (n_elements + 1 >= n_elem_alloc)
	{
		n_elem_alloc = n_elem_alloc + 1000 ;
		xy = (struct element *)
			G_realloc((char *)xy, n_elem_alloc * sizeof(struct element));
	}

    /*  store um  */


	xy[n_elements].row = row ;
	xy[n_elements].col = (float)col ;
	n_elements++ ;

}
#endif

#define  CURRENT_ROW	xy[i].row
#define  NEXT_ROW	xy[i+1].row
#define  CURRENT_COL	xy[i].col
#define  NEXT_COL	xy[i+1].col


#ifdef FOO
static
compress_row()
{

	int  i, k ;
	int  last_row ;
	float last_col ;

/*  NOTE: have to write this one yet  */


	k = 0 ;
	for ( i = 0; i < (n_elements - 1); i++ )
	{
		xy[k].row = CURRENT_ROW ;
		xy[k].col = CURRENT_COL ;
		++k ;

		while (CURRENT_ROW  ==  NEXT_ROW)
			++i ;

		xy[k].row = CURRENT_ROW ;
		xy[k].col = CURRENT_COL ;
		++k ;

	}

	n_elements = k ;
}
#endif

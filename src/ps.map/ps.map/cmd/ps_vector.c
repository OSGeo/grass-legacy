/* Functions: PS_vector_plot
**
** Modified by: Janne Soimasuo August 1994 line_cat added
** Author: Paul W. Carlson	March 1992
** modified to use G_plot_line() by Olga Waupotitsch on dec,93
*/

#include "Vect.h"
#include "ps_info.h"
#include "local_proto.h"
#include "vector.h"

int PS_vector_plot (struct Map_info *P_map, int vec, int type)
{   
    struct line_pnts *Points, *nPoints, *pPoints;
    int i, k, np, line=0, cat, line_cat;
    double *xarray, *yarray, tol=0.1, width;

    line_cat=vector.line_cat[vec];

    /* allocate memory for coordinates */
    Points = Vect_new_line_struct();

    /* process only vectors in current window */
    Vect_set_constraint_region(P_map, PS.w.north, PS.w.south, 
		PS.w.east, PS.w.west);

    tol /= PS.ew_to_x ; /* tolerance for parallel map units */
    width = vector.width[vec];

    /* read and plot vectors */
    k = 0;
    while (1)
    {
	int ret;
	double d;
	line++;
/*	if (0 > (ret = Vect_read_next_line(P_map, Points)))*/ /*line_cat*/
	if (0 > (ret = V2_read_line(P_map, Points,line)))
		
	{
	    if (ret == -1) G_warning("Read error in vector file\n");
	    break;
	}
	pPoints = Points; nPoints=0;
	cat = V2_line_att(P_map,line);
	if (line_cat< 0 || cat == line_cat)
	{
		if(vector.cwidth[vec])
		{
		    if(cat == 0 ) /* don't draw zero width line */     
			continue;
		
		    if(type == LINE_DRAW_HIGHLITE ) 
			width = cat * vector.cwidth[vec] + 2. * vector.hwidth[vec];   

		    if(type == LINE_DRAW_LINE)
			width = cat * vector.cwidth[vec];   

		    fprintf(PS.fp, "%.8f W\n", width);   
		}

		if ( vector.coffset[vec] != 0 || vector.offset[vec] != 0  )
		{
		    if ( vector.coffset[vec] != 0 )
			d = cat * vector.coffset[vec] / PS.ew_to_x ;
		    else
			d = vector.offset[vec] / PS.ew_to_x ;
		    
		    adjust_line ( Points ); /* LL projection */
		    nPoints = parallel_line ( Points, d, tol);
		    clean_parallel ( nPoints, Points, d );
		    pPoints = nPoints;
		}
		
		if ( vector.ref[vec] == LINE_REF_CENTER ) 
		{
		    np = pPoints->n_points;
		    xarray = pPoints->x;
		    yarray = pPoints->y;

		    if ( pPoints->n_points > 1)
		    {
        		start_line(xarray[0], yarray[0]);
	
			for (i = 0; i < np - 1; i++)
			{
	        	    sec_draw = 0;
        		    G_plot_line(xarray[0], yarray[0], xarray[1], yarray[1]);
			    if (k == 2)
			    {
				fprintf(PS.fp, "\n");
				k = 0;
			    }
			    else
			    {
				fprintf(PS.fp, " ");
				k++;
			    }
			    xarray++;
			    yarray++;
			}
			fprintf(PS.fp, "D\n");
		    }
		} else {
		    /* draw line as filled polygon between original line and parallel line (offset=width) */
		    d = width / PS.ew_to_x; 
    		    if ( vector.ref[vec] == LINE_REF_RIGHT ) d =- d;
		    adjust_line ( Points ); /* LL projection */
		    nPoints = parallel_line ( Points, d, tol);
		    clean_parallel ( nPoints, Points, d );
		    reverse_line ( nPoints );
		    fprintf(PS.fp, "NP\n");
		    if ( Points->n_points > 0 )
		    {
		        construct_path (Points, 0, START_PATH);
			construct_path (nPoints, 0, CLOSE_PATH);
		    } else {
		        construct_path (Points, 0, WHOLE_PATH);
		    }
		    fprintf(PS.fp, "F\n");
		}
		Vect_destroy_line_struct ( nPoints );
		Vect_reset_line ( Points );
	}
    }
    fprintf(PS.fp, "\n");
    return 0;
}


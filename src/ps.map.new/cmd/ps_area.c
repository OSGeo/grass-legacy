/* Functions: PS_area_plot
**
** Author: Radim Blazek Jan 2000
** modified copy of ps_vector.c by Paul W. Carlson March 1992
*/

#include "vector.h"
#include "Vect.h"
#include "ps_info.h"
#include "local_proto.h"

/* constuct subpath with moveto and repeated lineto from Points */
int construct_path (struct line_pnts *Points, double shift, int t)  
{
    int i, np, k = 1;
    double *xarray, *yarray, x, y;

    np = Points->n_points;
    xarray = Points->x;
    yarray = Points->y;

    for (i = 0; i < np ; i++)
    {
        x = XCONV(xarray[0]+shift);   
        y = YCONV(yarray[0]);
        fprintf(PS.fp, "%.1f %.1f ", x, y);
	if ( i==0 && ( t == START_PATH || t == WHOLE_PATH) )
        {
	    fprintf(PS.fp, "M ");
	}
	else
	{
	    fprintf(PS.fp, "LN ");    
	}
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
    if ( t == CLOSE_PATH || t == WHOLE_PATH )
    {
	fprintf(PS.fp, "CP\n");
    }
    return 1;
}

/* create paths for area and islands */
static int plot_area (struct Map_info *P_map, int area, double shift)  
{
    struct line_pnts *Points;
    int  j, ret, island;

    /* allocate memory for coordinates */
    Points = Vect_new_line_struct();

    /* plot areas */
    if (0 > (ret = Vect_get_area_points(P_map, area, Points)))
    {
	if (ret == -1) G_warning("Read error in vector file\n");
	return 0;
    }
    construct_path (Points, shift, WHOLE_PATH);

    /* plot islands */
    if (P_map->Area[area].n_isles)
    {
        for (j=0; j < P_map->Area[area].n_isles; j++)
	{
    	    island = P_map->Area[area].isles[j];
	    if (0 > (ret = Vect_get_isle_points(P_map, island, Points)))
	    {
		if (ret == -1) G_warning("Read error in vector file\n");
		return;
	    }
		construct_path (Points, shift, WHOLE_PATH);
        }
    }
    return 1; 
}

/* plot areas */
int PS_area_plot (struct Map_info *P_map, int vec)
{
    int  na, area, line_cat, ret;
    double e, w, n, s, aw, shift; 

    line_cat = vector.line_cat[vec];
    /* read and plot areas */
    na = V2_num_areas(P_map); 
    for (area = 1; area <= na; area++)
    {        
	if (line_cat< 0 || V2_area_att(P_map,area)==line_cat)
	{
	    V2_get_area_bbox (P_map, area, &n, &s, &e, &w);
	    if (PS.w.proj == PROJECTION_LL)
	    {	
		aw = G_adjust_easting(w, &PS.w);	    
	        if ( aw > PS.w.east ) aw -= 360.0; 
	        shift = aw - w;
	        e += shift;  
		w += shift;
	    }
	    /* check if in window */
	    if ( n < PS.w.south || s > PS.w.north || e < PS.w.west || w > PS.w.east )
		continue;
		
	    fprintf(PS.fp, "NP\n");
	    if (PS.w.proj == PROJECTION_LL)
	    {	
	        /* plot area while in window */
	        while ( e > PS.w.west ) 
	        {
		    ret = plot_area (P_map, area, shift);
		    if ( ret != 1) 
			return 0;
		    shift -= 360.0;
		    e -= 360.0; 
		}
	    }
	    else
	    {
		ret = plot_area (P_map, area, shift);
	    	if ( ret != 1) 
		    return 0;
	    }
	    fprintf(PS.fp, "%.2f %.2f %.2f C\n", (double) vector.acolor[vec].r/255., vector.acolor[vec].g/255., vector.acolor[vec].b/255.);  
	    fprintf(PS.fp, "F\n");			
	    if ( vector.width[vec] )
	    {
    		fprintf(PS.fp, "%.8f W\n", vector.width[vec] );  
		set_rgb_color(vector.colors[vec][0]);   
		fprintf(PS.fp, "stroke\n");
	    }
	}
    }
    fprintf(PS.fp, "\n");
    return 1;
}

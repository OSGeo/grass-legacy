/*
*  Update a history file.  Some of the digit file information  is placed in
*  the hist file.
*  returns  0  -  successful creation of history file
*          -1  -  error
*/

#include <string.h>
#include <stdio.h>
#include <grass/gis.h>

int update_hist (char *raster_name, char *vector_name,
    char *vector_mapset, long scale)
{
    char  *mapset ;
    struct History hist ;

    if(raster_name == NULL)
        return(-1) ;

    mapset = G_mapset() ;

    if (G_read_history(raster_name, mapset, &hist) < 0)
	return -1;


    strcpy(hist.title, raster_name) ;

/*  store information from digit file into history  */
    sprintf(hist.datsrc_1, "Vector Map: %s in mapset %s", vector_name, vector_mapset);
    sprintf(hist.datsrc_2, "Original Scale from Vector Map: 1:%ld",
        scale) ;  /* 4.0 */

    /***  copying to the  second page of history instead of 1st page
    sprintf(hist.edhist[hist.edlinecnt++], "Original Map Scale: 1:%s",
        dlg_struct->head.orig_scale) ;
    ***/

    return (G_write_history(raster_name, &hist)) ;
}

int 
update_colors (char *raster_name)
{
    struct Range range;
    struct Colors colors;
    CELL min,max;

    G_read_range (raster_name, G_mapset(), &range);
    G_get_range_min_max (&range, &min, &max);
    G_make_rainbow_colors (&colors, min, max);
    G_write_colors (raster_name, G_mapset(), &colors);

    return 0;
}


int
update_fcolors (char *raster_name)
{
    struct FPRange range;
    struct Colors colors;
    DCELL min,max;

    G_read_fp_range(raster_name, G_mapset(), &range);
    G_get_fp_range_min_max(&range, &min, &max);
    G_make_rainbow_colors(&colors, (CELL)min, (CELL)max);
    G_write_colors(raster_name, G_mapset(), &colors);

    return 0;
}


int 
update_cats (char *raster_name, char *vector_name, char *vector_mapset)
{
    /* TODO: maybe attribute transfer from vector map? 
       Use G_set_raster_cat() somewhere*/
    
    struct Categories cats;

    G_strip(raster_name);
    G_init_cats ((CELL) 0, raster_name, &cats);
    G_write_cats (raster_name, &cats);

    return 0;
}

#include "windows.h"
#include "lproto.h"
#include "raster.h"
#include "gis.h"
#include "D.h"
#include "variables.h"

int show_legend(void)
{
	struct Range range ;
	struct Categories cats;
	int fp, cats_num;
	char buff[256] ;

/* Draw legend */
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");
	Dchoose(LEG.name) ;
	Derase("black") ;
	R_close_driver();

	fp = G_raster_map_is_fp(mapname, mapset);

	if(!fp)
	{
	    if (-1 == G_read_range(mapname, mapset, &range))
	          return 0;
 	    cats_num = range.max - range.min + 2 ;
	    /* one for no-data */
	}
	else
	{
	   if (-1 == G_read_raster_cats(mapname, mapset, &cats))
		return 0;
           cats_num = G_number_of_raster_cats(&cats) + 1 ;
        }


	if (cats_num < 25)
	{
 	    sprintf(buff,"'%s@%s' lines=30", mapname, mapset) ;
	    gorun("d.legend", buff) ;
	}
	else
	{
 	    if (!fp && cats_num < 12 * 25)
		sprintf(buff,"'%s@%s' lines=25 cols=12", mapname, mapset) ;
	    else
		sprintf(buff,"'%s@%s'", mapname, mapset) ;
	    gorun("d.colortable", buff) ;
	}

	return 0;
}

#include "gis.h"
#include "local_proto.h"

int make_support (struct rr_state *theState, int percent)
{
    char title[100];
    struct History hist;
    struct Categories cats;
    struct Colors clr;
    
    if(G_read_raster_cats (theState->inraster, theState->mapset, &cats) >= 0)
    {
	sprintf (title, "Random sites on [%s in %s]", 
                theState->inraster, theState->mapset);
	G_set_cats_title (title, &cats);
	if (theState->use_nulls)
	    G_set_raster_cat (theState->nulls.data.v,
                    theState->nulls.data.v,  
                    "Sites with NULL values in original",
                    &cats,
                    theState->nulls.type);
	G_write_raster_cats (theState->outraster, &cats);
    }

#ifdef BUGGY_CODE_HERE
    if (G_read_history (theState->outraster, G_mapset(), &hist) >= 0)
    {
        G_short_history(theState->outraster, "raster", &hist);
	sprintf (hist.datsrc_1, "Based on map [%s in %s]", 
                theState->inraster, theState->outraster);
	if (percent)
	    sprintf (hist.datsrc_2, "Random sites over %s of the base map",
		theState->nRand);
	else
	    sprintf (hist.datsrc_2, "%s random sites on the base map", 
                    theState->nRand);
	G_write_history (theState->outraster, &hist);
    }
#endif
    
    if (G_read_colors (theState->inraster, theState->mapset, &clr) >= 0)
    {
        if (theState->use_nulls)
        {
            G_add_raster_color_rule (theState->nulls.data.v, 127, 127, 127, 
                    theState->nulls.data.v, 127, 127, 127, &clr,
                    theState->nulls.type);
        }
        G_write_colors (theState->outraster, G_mapset(), &clr);
    }
        
    return 0;
}

/* vim: set softtabstop=4 shiftwidth=4 expandtab: */

#include <stdlib.h>
#include "gis.h"
/*
 **********************************************************************
 *
 *   G__check_for_auto_masking()
 *
 *   On first call, opens the mask file if declared and available
 *      and allocates buffer for reading mask rows.  Returns 0 for
 *      mask unset or unavailable and 1 for set and available.
 *   On second call, returns 0 or 1 as above.
 *
 *   returns:
 *      0  mask unset or unavailable
 *      1  mask set and available and ready to use
 **********************************************************************/

#include "G.h"

int G__check_for_auto_masking (void)
{
    struct Cell_head cellhd;

    /* if mask is switched off (-2) return -2
       if G__.auto_mask is not set (-1) or set (>=0) recheck the MASK */
    if (G__.auto_mask < -1)
        return G__.auto_mask ;  

    /* if(G__.mask_fd > 0) free(G__.mask_buf);*/

/* look for the existence of the MASK file */
    G__.auto_mask = (G_find_cell ("MASK", G_mapset()) != 0);

    if (G__.auto_mask <= 0)
        return 0;

/* check MASK projection/zone against current region */
    if (G_get_cellhd ("MASK", G_mapset(), &cellhd) >= 0)
    {
	if (cellhd.zone != G_zone() || cellhd.proj != G_projection())
	{
	    G__.auto_mask = 0;
	    return 0;
	}
    }

    G_unopen_cell(G__.mask_fd );
    G__.mask_fd = G__open_cell_old ("MASK", G_mapset());
    if (G__.mask_fd < 0)
    {
        G__.auto_mask = 0;
        G_warning ("Unable to open automatic MASK file");
        return 0;
    }

/*    G__.mask_buf = G_allocate_cell_buf();*/

    G__.auto_mask = 1;

    return 1;
}

int G_suppress_masking (void)
{
    if (G__.auto_mask > 0)
    {
	G_close_cell (G__.mask_fd);
	/* free (G__.mask_buf); */
	G__.mask_fd = -1;
    }
    G__.auto_mask = -2;

    return 0;
}

int G_unsuppress_masking (void)
{
    if (G__.auto_mask < -1)
    {
	G__.mask_fd = -1;
        G__check_for_auto_masking ();
    }

    return 0;
}

#include <stdlib.h>
#include "sites.h"
#include "gis.h"
#include "site.h"
#include "text.h"
#include "misc.h"
#include "local_proto.h"

int do_sites (void)
{
    int i;
    FILE *fd;
    char buf[100];
    int x, y;
    struct Cell_head window;
    Site *mysite;
    int dims=0,strs=0,cat=0,dbls=0;

    G_get_window (&window);

    select_standard_font ();
    set_reasonable_text_size ();
    set_text_color (BLACK);
    set_text_width (1);
    set_text_rotation (0);
    set_text_background (WHITE);
    set_text_border (BLACK);
    set_text_xref (LEFT);
    set_text_yref (CENTER);

    i = site.count;
    while (i-- > 0)
    {
	fd = G_fopen_sites_old (site.name[i], site.mapset[i]);
	if (fd == NULL)
	{
	    sprintf (buf,"%s in %s", site.name[i], site.mapset[i]);
	    error ("site list",buf,"could not open");
	    continue;
	}
      if (G_site_describe (fd, &dims, &cat, &strs, &dbls)!=0)
        G_fatal_error("failed to guess format");
      mysite = G_site_new_struct (cat, dims, strs, dbls);

/*
 * sites must fall within current gis window (not paint window)
 */
        while (G_site_get (fd, mysite) == 0) 
	{
            if (!G_site_in_region (mysite, &window)) continue;
	    /* if (nn > window.north || nn < window.south) continue; */
	    /* if (ee > window.east  || ee < window.west ) continue; */

	    G_plot_where_xy (mysite->east, mysite->north, &x, &y);

	    set_color (site.color[i]);
	    draw_icon (&site.icon[i],x,y);
	    if (site.with_text[i])
	    {
		if (dbls >0 || strs > 0 || cat > 0)
		{
		    int offset ;
		    offset = site.icon[i].ncols - site.icon[i].xref + 3 ;
                    if (strs > 0)
                      sprintf(buf,"%s",mysite->str_att[0]);
                    else if (cat == CELL_TYPE)
                      sprintf(buf,"%d",mysite->ccat);
                    else if (cat == FCELL_TYPE )
                      sprintf(buf,"%g",mysite->fcat);
                    else if (cat == DCELL_TYPE )
                      sprintf(buf,"%g",mysite->dcat);
                    else if (dbls > 0)
                      sprintf(buf,"%g",mysite->dbl_att[0]);
                    draw_text (buf,x+offset,y);
		}
	    }
	}
	fclose (fd);
    }

    return 0;
}

#include "sites.h"
#include "gis.h"
#include "text.h"
#include "misc.h"

do_sites ()
{
    int i;
    FILE *fd;
    char buf[100];
    char *desc;
    double nn,ee;
    double atof();
    int x, y;
    struct Cell_head window;

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


/*
 * look for lines with format: point|east|north|desc 
 * or east|north|desc
 * points must fall within current gis window (not paint window)
 */
	while (G_get_site (fd, &ee, &nn, &desc) > 0)
	{
	    if (nn > window.north || nn < window.south) continue;
	    if (ee > window.east  || ee < window.west ) continue;

	    G_plot_where_xy (ee, nn, &x, &y);

	    set_color (site.color[i]);
	    draw_icon (&site.icon[i],x,y);
	    if (site.with_text[i])
	    {
		if (*desc)
		{
		    int offset ;

		    offset = site.icon[i].ncols - site.icon[i].xref + 3 ;
		    draw_text (desc,x+offset,y);
		}
	    }
	}
	fclose (fd);
    }
}

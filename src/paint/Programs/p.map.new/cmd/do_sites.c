#include "sites.h"
#include "gis.h"
#include "text.h"
#include "misc.h"
#include "fullwindow.h"

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
    double Mheight();

    G_get_window (&window);

    select_standard_font ();
    set_reasonable_text_size ();
    set_text_width (1);
    set_text_rotation (0);
    set_text_background (WHITE);
    set_text_border (BLACK);
    set_text_xref (LEFT);
    set_text_yref (CENTER);

    i = site.count;
    while (i-- > 0)
    {
    set_text_color (site.textcolor[i]);
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
		
	            if (site.north[i]>=0.0 && site.east[i]>=0.0)
	            G_plot_where_xy (site.east[i], 
			site.north[i], &x, &y);
		
		    if (site.textsize[i]>0.0)
	            {
			set_text_size(site.textsize[i]/Mheight(1));
    			set_text_border (BLACK);
	            }
		    



		    offset = site.icon[i].ncols - site.icon[i].xref + 3 ;
		    draw_text (desc,x+offset,y, 2);
		}
	    }
	}
	fclose (fd);
    }
}


static
double
Mheight(drawflag)
{
    BOX box;

    set_text_border(-1);
    set_text_background(-1);
    set_text_width(1);
    set_text_size(100.0);
    text_bounds ("M",0,0,&box, drawflag);

    return (fullwindow.ns_res * (box.bottom-box.top+1) /
 100.0) ;
}

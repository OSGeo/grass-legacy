/* Function: do_sites
**
** Author: Paul W. Carlson	May 1992
*/

#include "sites.h"
#include "ps_info.h"

#define LEFT 0
#define CENTER 2

extern int verbose;

do_sites()
{
    int i, j;
    FILE *sites_fp, *icon_fp;
    char buf[100];
    char *desc;
    double nn, ee;
    double s, x, y, x_off, y_off, xo[50], yo[50];
    int points, margin, multi_text;

    i = site.count;
    while (i-- > 0)
    {
	/* open the sites file */
	sites_fp = G_fopen_sites_old(site.name[i], site.mapset[i]);
	if (sites_fp == NULL)
	{
	    sprintf(buf,"%s in %s", site.name[i], site.mapset[i]);
	    error("site list", buf, "could not open");
	    continue;
	}

	if (verbose > 1)
	{
	    printf("PS-PAINT: reading sites file <%s in %s> ...",
	    site.name[i], site.mapset[i]);
	    fflush(stdout);
	}

	/* read icon file */
        if ((icon_fp = G_fopen_old("ps_icons", site.icon[i], site.mapset[i])) 
		!= NULL) 
        {
	    /* read past icon title */
	    fgets(buf, 100, icon_fp);

	    /* read coordinates */
	    points = 0;
    	    while (fscanf(icon_fp, "%lf %lf", &x_off, &y_off) == 2)
	    {
		xo[points] = x_off;
		yo[points] = y_off;
	    	points++;
	    }
	    fclose(icon_fp);
        }
        else

	/* use default icon (diamond) */
        {
	    points = 4;
	    xo[0] =  0.0;  yo[0] =  0.5;
	    xo[1] = -0.5;  yo[1] =  0.0;
	    xo[2] =  0.0;  yo[2] = -0.5;
	    xo[3] =  0.5;  yo[3] =  0.0;
	}

	/* set font and icon size */
    	set_font_name(site.font[i]);
    	s = 10.0 * site.size[i];
    	set_font_size((int)(s + 0.5));
    	margin = (int)(0.2 * s + 0.5);
    	if (margin < 2) margin = 2;
    	fprintf(PS.fp, "/mg %d def\n", margin);

	/* read the sites file */
	while (G_get_site(sites_fp, &ee, &nn, &desc) > 0)
	{
	    if (nn > PS.w.north || nn < PS.w.south) continue;
	    if (ee > PS.w.east  || ee < PS.w.west ) continue;

	    x = XCONV(ee);
	    y = YCONV(nn);

	    /* draw the icon */
	    set_rgb_color(site.color[i]);
	    fprintf(PS.fp, "%.1lf %.1lf NM\n", x + s * xo[0], y + s * yo[0]);
	    for (j = 1; j < points; j++) 
	       fprintf(PS.fp, "%.1lf %.1lf LN\n", x + s * xo[j], y + s * yo[j]);
	    fprintf(PS.fp, "CF\n");

	    /* draw the description */
	    if (site.with_text[i])
	    {
		if (*desc)
		{
		    int ix, iy;

		    multi_text = multi_lines(desc);
		    ix = (int)(x + 0.5 * s) + 5;
		    iy = (int)y;
		    if (multi_text)
		        multi_text_box_path(ix, iy, LEFT, CENTER, desc, 
				(int)(s + 0.5));
		    else
		        text_box_path(ix, iy, LEFT, CENTER, desc, 
				(int)(s + 0.5));
		    set_rgb_color(WHITE);
		    fprintf(PS.fp, "F ");
		    set_rgb_color(BLACK);
	    	    if (multi_text) fprintf(PS.fp, "D DMT\n");
		    else fprintf(PS.fp, "D TIB\n");
		}
	    }
	}
	fclose(sites_fp);
	if (verbose > 1) printf("\n");
    }
}

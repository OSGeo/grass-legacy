/* Function: do_sites
**
** Author: Paul W. Carlson	May 1992
*/

#include "gis.h"
#include "sites.h"
#include "ps_info.h"
#include "local_proto.h"

#define LEFT 0
#define CENTER 2

extern int verbose;

int do_sites (void)
{
    int i, j;
    FILE *sites_fp, *icon_fp;
    char buf[100], eps[50], epsfile[1024];
    char *desc, *ms;
    double nn, ee;
    double s, x, y, x_off, y_off, xo[50], yo[50], xt, yt;
    double llx, lly, urx, ury;
    int points, margin, multi_text, x_int, y_int, eps_exist;
    Site *mysite;
    int dims=0,strs=0,cat=0,dbls=0;

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
	    fprintf (stdout,"PS-PAINT: reading sites file <%s in %s> ...",
	    site.name[i], site.mapset[i]);
	    fflush(stdout);
	}
        if (G_site_describe (sites_fp, &dims, &cat, &strs, &dbls)!=0)
          G_fatal_error("failed to guess format");
        mysite = G_site_new_struct (cat, dims, strs, dbls);

	/* read icon file */
	ms= G_find_file ("ps_icons", site.icon[i], (char *) 0);
        if ((icon_fp = G_fopen_old("ps_icons", site.icon[i], ms)) 
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

	/* if eps file is specified as common for all sites then
	   read bbox and save eps to PS file */
	if (site.epstype[i] == 1)
	{
	    if ( !eps_bbox( site.epspre[i], &llx, &lly, &urx, &ury) )
	    {
		site.epstype[i] == 0; /* eps file can't be read */
	    }
	    else  /* save to PS */
	    {
		sprintf (eps, "SITEEPSF%d", i);
		eps_save ( PS.fp, site.epspre[i], eps);
	    }
	}
	
	/* set font and icon size */
    	set_font_name(site.font[i]);
    	s = 10.0 * site.size[i];
    	set_font_size((int)(s + 0.5));
    	margin = (int)(0.2 * s + 0.5);
    	if (margin < 2) margin = 2;
    	fprintf(PS.fp, "/mg %d def\n", margin);

	/* read the sites file */
        while (G_site_get (sites_fp, mysite) == 0) 
        {
            nn=mysite->north;
            ee=mysite->east;

	    if (nn > PS.w.north || nn < PS.w.south) continue;
	    if (ee > PS.w.east  || ee < PS.w.west ) continue;

	    G_plot_where_xy(ee, nn, &x_int, &y_int);
	    x = (double) x_int / 10.;
	    y = (double) y_int / 10.;

	    if (site.size_att[i] > 0 && site.size_att[i] <= dbls)
	    {
	        s = 10.0 * site.size[i] * mysite->dbl_att[ site.size_att[i] - 1 ];
	    }

	    if (site.epstype[i] == 1)  /* draw common eps */ 
	    {
        	/* calculate translation */
        	eps_trans (llx, lly, urx, ury, x, y, s/10, site.rotate[i], &xt, &yt);
		eps_draw_saved ( PS.fp, eps, xt, yt, s/10, site.rotate[i]);
	    }
	    else if (site.epstype[i] == 2)  /* draw epses */ 
	    {
	        sprintf (epsfile, "%s%d%s", site.epspre[i], mysite->ccat, site.epssuf[i]);
		if ( eps_exist = eps_bbox( epsfile, &llx, &lly, &urx, &ury) )
		{
		    eps_trans (llx, lly, urx, ury, x, y, s/10, site.rotate[i], &xt, &yt);

		    eps_draw ( PS.fp, epsfile, xt, yt, s/10, site.rotate[i]); 
		}
	    }

	     /* draw the icon */	    
	    if ( (site.epstype[i] == 0) || (site.epstype[i] == 2 && !eps_exist ) )   
	    {
		set_rgb_color(site.color[i]);
		fprintf(PS.fp, "%.1f %.1f NM\n", x + s * xo[0], y + s * yo[0]);
		for (j = 1; j < points; j++) 
	           fprintf(PS.fp, "%.1f %.1f LN\n", x + s * xo[j], y + s * yo[j]);
		fprintf(PS.fp, "CF\n");
	    }	

	    /* draw the description */
	    if (site.with_text[i])
	    {
		if (dbls >0 || strs > 0 || cat > 0)
		{
		    int ix, iy;
                    if ((desc=(char *)G_malloc(80*sizeof(char)))==NULL)
                      G_fatal_error("memory allocation error");
                    if (strs > 0)
                      sprintf(desc,"%s",mysite->str_att[0]);
                    else if (cat == CELL_TYPE)
                      sprintf(buf,"%d",mysite->ccat);
                    else if (cat == FCELL_TYPE )
                      sprintf(buf,"%g",mysite->fcat);
                    else if (cat == DCELL_TYPE )
                      sprintf(buf,"%g",mysite->dcat);
                    else if (dbls > 0)
                      sprintf(desc,"%g",mysite->dbl_att[0]);

		    multi_text = multi_lines(desc);
		    ix = (int)(x + 0.5 * s) + 5;
		    iy = (int)y;
		    if (multi_text)
		        multi_text_box_path(ix, iy, LEFT, CENTER, desc, 
				(int)(s + 0.5), 0);
		    else
		        text_box_path(ix, iy, LEFT, CENTER, desc, 
				(int)(s + 0.5), 0);
		    set_rgb_color(WHITE);
		    fprintf(PS.fp, "F ");
		    set_rgb_color(BLACK);
	    	    if (multi_text) fprintf(PS.fp, "D DMT\n");
		    else fprintf(PS.fp, "D TIB\n");
		}
	    }
	}
	fclose(sites_fp);
	if (verbose > 1) fprintf (stdout,"\n");
    }

    return 0;
}

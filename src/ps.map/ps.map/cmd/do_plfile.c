/* Function: do_plfile
**
** Author: Paul W. Carlson	March 1992
*/

#include <string.h>
#include "ps_info.h"
#include "local_proto.h"

extern int verbose;

int do_plfile (int after_masking)
{
    FILE *fp, *icon_fp;
    char buf[1024];
    char name[1024], prev_name[50], mapset[50];
    double e1, n1, e2, n2, llx, lly, urx, ury;
    int color, fcolor, fill;
    int masked;
    double size, scale, rotate;
    int i, j;
    int x_int, y_int;
    double width, s, x, y, x_off, y_off, xo[50], yo[50];
    int points=0, use_default;

    prev_name[0] = 0;

    if (PS.plfile == NULL) return 1;
    fp = fopen(PS.plfile, "r");
    if (fp == NULL)
    {
	error("point/line file", "", "can't open");
	return 1;
    }

    if (verbose > 1)
    {
        fprintf (stdout,"PS-PAINT: reading point/line file ...");
        fflush(stdout);
    }

    while (fgets(buf, sizeof buf, fp))
    switch (*buf)
    {
    case 'L':
	if(sscanf(buf, "L %d %lf %lf %lf %lf %d %lf",
	    &masked, &e1, &n1, &e2, &n2, &color, &width) == 7)
	{
	    if ( masked &&  after_masking) continue;
	    if (!masked && !after_masking) continue;
	    set_rgb_color(color);
	    set_line_width(width);
	    start_line(e1, n1);
	    sec_draw = 0;
	    G_plot_line(e1, n1, e2, n2);
	    fprintf(PS.fp, " stroke\n");
	}
	break;
	
    case 'R':
	if(sscanf(buf, "R %d %lf %lf %lf %lf %d %d %d %lf",
	    &masked, &e1, &n1, &e2, &n2, &color, &fcolor, &fill, &width) == 9)
	{
	    if ( masked &&  after_masking) continue;
	    if (!masked && !after_masking) continue;

	    fprintf(PS.fp, " NP\n");
	    G_plot_where_xy(e1, n1, &x_int, &y_int);
	    llx = (double) x_int / 10.;
	    lly = (double) y_int / 10.;
	    G_plot_where_xy(e2, n2, &x_int, &y_int);
	    urx = (double) x_int / 10.;
	    ury = (double) y_int / 10.;	    

	    fprintf(PS.fp, " %.1f %.1f M %.1f %.1f LN\n", llx, lly, urx, lly);
	    fprintf(PS.fp, " %.1f %.1f LN %.1f %.1f LN\n", urx, ury, llx, ury);
	    fprintf(PS.fp, " CP\n");
	    if (fill)
	    {
		set_rgb_color(fcolor);
		fprintf(PS.fp, " F\n");		
	    }
	    set_rgb_color(color);
	    set_line_width(width);
	    fprintf(PS.fp, " D\n");
	}
	break;	

    case 'P':
	i = sscanf (buf,"P %d %lf %lf %d %lf %s %s",
	    &masked, &e1, &n1, &color, &size, name, mapset);
	if (i == 5 || i == 7)
	{
	    if ( masked &&  after_masking) continue;
	    if (!masked && !after_masking) continue;
	    if (i == 7 && strcmp(name, prev_name))
	    {
		strcpy(prev_name, name);
        	if ((icon_fp = G_fopen_old("ps_icons", name, mapset)) != NULL) 
        	{
	    	    fgets(buf, 100, icon_fp);
	    	    points = 0;
    	    	    while (fscanf(icon_fp, "%lf %lf", &x_off, &y_off) == 2)
	    	    {
		        xo[points] = x_off;
		        yo[points] = y_off;
	    	        points++;
	            }
	            fclose(icon_fp);
		    use_default = 0;
                }
                else use_default = 1;
	    }
	    else use_default = 1;
	    if (use_default)
            {
	        points = 4;
	        xo[0] =  0.0;  yo[0] =  0.5;
	        xo[1] = -0.5;  yo[1] =  0.0;
	        xo[2] =  0.0;  yo[2] = -0.5;
	        xo[3] =  0.5;  yo[3] =  0.0;
	    }
	    if (size < 0.0) size = 1.0;
    	    s = 10.0 * size;

	    if (n1 > PS.w.north || n1 < PS.w.south) continue;
	    if (e1 > PS.w.east  || e1 < PS.w.west ) continue;
	    /*
	    x = XCONV(e1);
	    y = YCONV(n1);
	    */
	    G_plot_where_xy(e1, n1, &x_int, &y_int);
	    x = (double) x_int / 10.;
	    y = (double) y_int / 10.;
	    set_rgb_color(color);
	    fprintf(PS.fp, "%.1f %.1f NM\n", x + s * xo[0], y + s * yo[0]);
	    for (j = 1; j < points; j++) 
	       fprintf(PS.fp, "%.1f %.1f LN\n", x + s * xo[j], y + s * yo[j]);
	    fprintf(PS.fp, "CF\n");
	}
	break;
	
	case 'E':  /* EPS file */
	if (sscanf (buf,"E %d %lf %lf %lf %lf %s",
	    &masked, &e1, &n1, &scale, &rotate, name) == 6 );
	{
	    if ( masked &&  after_masking) continue;
	    if (!masked && !after_masking) continue;
	    
	    /* find eps bbox */
    	    if ( !eps_bbox(name, &llx, &lly, &urx, &ury) ) continue;
	    
	    G_plot_where_xy(e1, n1, &x_int, &y_int);
	    x = (double) x_int / 10.;
	    y = (double) y_int / 10.;
	    
	    /* calculate translation */
	    eps_trans (llx, lly, urx, ury, x, y, scale, rotate, &x_off, &y_off);
	    
	    /* write eps to PS */
	    eps_draw ( PS.fp, name, x_off, y_off, scale, rotate);	    
	}
	break;
    }

    fclose (fp);
    if (verbose > 1) fprintf (stdout,"\n");

    return 0;
}

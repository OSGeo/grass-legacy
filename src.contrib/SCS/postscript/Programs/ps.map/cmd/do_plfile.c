/* Function: do_plfile
**
** Author: Paul W. Carlson	March 1992
*/

#include "ps_info.h"

extern int verbose;

do_plfile(after_masking)
{
    FILE *fp, *icon_fp;
    char buf[1024];
    char name[50], prev_name[50], mapset[50];
    double e1, n1, e2, n2;
    int color;
    int masked;
    double size;
    int i, j;
    int width;
    double s, x, y, x_off, y_off, xo[50], yo[50];
    int points, use_default;

    prev_name[0] = 0;

    if (PS.plfile == NULL) return;
    fp = fopen(PS.plfile, "r");
    if (fp == NULL)
    {
	error("point/line file", "", "can't open");
	return;
    }

    if (verbose > 1)
    {
        printf("PS-PAINT: reading point/line file ...");
        fflush(stdout);
    }

    while (fgets(buf, sizeof buf, fp))
    switch (*buf)
    {
    case 'L':
	if(sscanf(buf, "L %d %lf %lf %lf %lf %d %d",
	    &masked, &e1, &n1, &e2, &n2, &color, &width) == 7)
	{
	    if ( masked &&  after_masking) continue;
	    if (!masked && !after_masking) continue;
	    set_rgb_color(color);
	    set_line_width(width);
	    PS_plot_vect(e1, n1, e2, n2, (int)0);
	    fprintf(PS.fp, " stroke\n");
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
	    x = XCONV(e1);
	    y = YCONV(n1);
	    set_rgb_color(color);
	    fprintf(PS.fp, "%.1lf %.1lf NM\n", x + s * xo[0], y + s * yo[0]);
	    for (j = 1; j < points; j++) 
	       fprintf(PS.fp, "%.1lf %.1lf LN\n", x + s * xo[j], y + s * yo[j]);
	    fprintf(PS.fp, "CF\n");
	}
	break;
    }

    fclose (fp);
    if (verbose > 1) printf("\n");
}

/* Function: read_cfg
** added #include <stdlib.h> 7/98 Richard Nairn
#include <unistd.h>
**
** This function reads the configuration file to get the printer info.
**
** Author: Paul W. Carlson	April 1992
*/

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include "ps_info.h"

#define FIELD(x) strcmp(x,field)==0

extern int verbose;
extern int rotate_plot;

int 
read_cfg (void)
{
    char field[80];
    char value[80];
    char buf[80];
    char path[1024];
    char *G_gisbase();
    char *ps_painter;
    FILE *fp;

    ps_painter = G__getenv("PSPAINTER");
    sprintf(path, "%s/etc/paint/ps.devices/", G_gisbase());
    if (ps_painter != NULL) strcat(path, ps_painter);
    if (ps_painter && access(path, 0) != 0)
    {
	G_unsetenv("PSPAINTER");
	ps_painter = NULL;
    }

    if (ps_painter == NULL || (fp = fopen(path, "r")) == NULL)
    {
	if (verbose > 1)
	{
	    fprintf (stdout,"\nPS-PAINT: using default configuration values.\n");
	    fflush(stdout);
	}
	PS.level = 2;
	PS.page_width = (rotate_plot) ? 11.0 : 8.5;
	PS.page_height = (rotate_plot) ? 8.5 : 11.0;
	PS.left_marg = (rotate_plot) ? 0.5 : 0.25;
	PS.right_marg = (rotate_plot) ? 0.5 : 0.25;
	PS.top_marg = (rotate_plot) ? 0.25 : 0.5;
	PS.bot_marg = (rotate_plot) ? 0.25 : 0.5;
    	PS.map_width = (rotate_plot) ? 10.0 : 8.0;
    	PS.map_height = (rotate_plot) ? 8.0 : 10.0;
    	PS.map_x_orig = PS.left_marg;
    	PS.map_y_orig = PS.page_height - PS.top_marg;
	PS.min_y = 72.0 * PS.map_y_orig;
	PS.res = 75;
	return 1;
    }

    if (verbose > 1)
    {
        fprintf (stdout,"\nPS-PAINT: PostScript painter \"%s\" selected.\n", ps_painter);
        fflush(stdout);
    }

    while (fgets(buf, sizeof buf, fp))
    {
        *value = 0;
        *field = 0;
        if (sscanf(buf,"%[^:]:%[^\n]", field, value) < 1) continue;

        if (FIELD("level"))
        {
	    PS.level = atoi(value);
	    if (PS.level < 1 || PS.level > 2) PS.level = 2;
	    continue;
	}

	if (FIELD("page width"))
  	{
	    PS.page_width = atof(value);
	    /*
	    if (PS.page_width < 8.5) PS.page_width = 8.5;
	    if (PS.page_width > 48.0) PS.page_width = 48.0;
	    */
	    continue;
	}

	if (FIELD("page height"))
  	{
	    PS.page_height = atof(value);
	    /*
	    if (PS.page_height < 8.5) PS.page_height = 8.5;
	    if (PS.page_height > 48.0) PS.page_height = 48.0;
	    */
	    continue;
	}

	if (FIELD("top margin"))
  	{
	    PS.top_marg = atof(value);
	    if (PS.top_marg < 0.0) PS.top_marg = 0.0;
	    if (PS.top_marg > 3.0) PS.top_marg = 3.0;
	    continue;
	}

	if (FIELD("bottom margin"))
  	{
	    PS.bot_marg = atof(value);
	    if (PS.bot_marg < 0.0) PS.bot_marg = 0.0;
	    if (PS.bot_marg > 3.0) PS.bot_marg = 3.0;
	    continue;
	}

	if (FIELD("left margin"))
  	{
	    PS.left_marg = atof(value);
	    if (PS.left_marg < 0.0) PS.left_marg = 0.0;
	    if (PS.left_marg > 3.0) PS.left_marg = 3.0;
	    continue;
	}

	if (FIELD("right margin"))
  	{
	    PS.right_marg = atof(value);
	    if (PS.right_marg < 0.0) PS.right_marg = 0.0;
	    if (PS.right_marg > 3.0) PS.right_marg = 3.0;
	    continue;
	}

	if (FIELD("resolution"))
  	{
	    PS.res = atoi(value);
	    if (PS.res < 75) PS.res = 75;
	    continue;
	}
    }
    fclose(fp);

    if (rotate_plot)
    {
	double t;

	t = PS.page_width;
	PS.page_width = PS.page_height;
	PS.page_height = t;

	t = PS.left_marg;
    	PS.left_marg = PS.bot_marg;
	PS.bot_marg = t;

	t = PS.right_marg;
	PS.right_marg = PS.top_marg;;
	PS.top_marg = t;

    	t = PS.map_width;
    	PS.map_width = PS.map_height;
    	PS.map_height = t;
    }

    PS.map_width  = PS.page_width  - PS.left_marg - PS.right_marg;
    PS.map_height = PS.page_height - PS.top_marg  - PS.bot_marg;
    PS.map_y_orig = PS.page_height - PS.top_marg;
    PS.map_x_orig = PS.left_marg;
    PS.min_y = 72.0 * PS.map_y_orig;
    return 0;
}


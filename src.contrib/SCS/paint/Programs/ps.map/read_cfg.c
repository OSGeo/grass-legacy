/* Function: read_cfg
**
** This function reads the ps.cfg file to get the printer info.
**
** Author: Paul W. Carlson	April 1992
*/

#include <stdio.h>
#include <math.h>
#include "ps_info.h"

#define FIELD(x) strcmp(x,field)==0

read_cfg()
{
    char field[80];
    char value[80];
    char buf[80];
    FILE *fp;

    if ((fp = fopen("ps.cfg", "r")) == NULL)
    {
	PS.level = 2;
	PS.page_width = 8.5;
	PS.page_height = 11.0;
	PS.left_marg = 0.25;
	PS.right_marg = 0.25;
	PS.top_marg = 0.5;
	PS.bot_marg = 0.5;
    	PS.map_width = 8.0;
    	PS.map_height = 10.0;
    	PS.map_x_orig = 0.25;
    	PS.map_y_orig = 10.5;
	PS.min_y = 72.0 * PS.map_y_orig;
	return 1;
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
	    if (PS.page_width < 8.5) PS.page_width = 8.5;
	    if (PS.page_width > 48.0) PS.page_width = 48.0;
	    continue;
	}

	if (FIELD("page height"))
  	{
	    PS.page_height = atof(value);
	    if (PS.page_height < 8.5) PS.page_height = 8.5;
	    if (PS.page_height > 48.0) PS.page_height = 48.0;
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
    }
    fclose(fp);
    PS.map_width  = PS.page_width  - PS.left_marg - PS.right_marg;
    PS.map_height = PS.page_height - PS.top_marg  - PS.bot_marg;
    PS.map_y_orig = PS.page_height - PS.top_marg;
    PS.map_x_orig = PS.left_marg;
    PS.min_y = 72.0 * PS.map_y_orig;
    return 0;
}


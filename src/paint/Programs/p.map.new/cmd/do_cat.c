#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "text.h"
#include "cats.h"
#include "misc.h"
#include "fullwindow.h"
#include "parms.h"
#include "colormode.h"
#include "graphics.h"
#include "local_proto.h"

#define FIELD(x) strcmp(x,field)==0

int do_cats (struct Colors *pcolr)
{
	FILE	*fd;

	if (cats.other) {
		fd = fopen (cats.other, "r");
	if (fd == NULL)
	{
	return 1;
	}
	else 
		do_cat(fd, pcolr);
	}
	return 0;
}

int do_cat (FILE *fd, struct Colors *pcolr)
{
    double east=0.0L, north=0.0L;
	char buf[1024];
	char value[1024];
	char field[1024];
	DCELL dmin, dmax;
	int width;
	int height;
	double dtmp;

	width 	= 20;
	height  = 10;
	while (fgets (buf, sizeof buf, fd) )
	{

        *value = 0;
        *field = 0;
        if (sscanf (buf,"%[^:]:%[^\n]", field, value) < 1) continue;


		if (FIELD("catnum"))
		{
		    if(sscanf(value, "%lf-%lf",&dmin, &dmax)!=2)
		    {
			if(strncmp(value,"nv",2)==0)
			    G_set_d_null_value(&dmin, 1);
                        else
			{
			    sscanf(value, "%lf", &dmin);
			    dmax = dmin;
			}
                    }
 		    draw_cat(east, north, width, height, dmin, dmax, pcolr);
		}

			
        if (FIELD("cwidth"))
        {
			width	= atoi(value);
            continue;
            }
        if (FIELD("cheight"))
        {
			height = atoi(value);
            continue;
        }

		if (FIELD("north"))
        {
	    if (scan_northing (value, &dtmp))
		north = dtmp;
            continue;
        }

        if (FIELD("east"))
        {
	    if (scan_easting (value, &dtmp))
		east = dtmp;
            continue;
        }

	}

	return 0;
}

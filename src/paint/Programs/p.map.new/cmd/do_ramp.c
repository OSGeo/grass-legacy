#include "gis.h"
#include "text.h"
#include "ramp.h"
#include "misc.h"
#include "fullwindow.h"
#include "parms.h"
#include "colormode.h"

#define FIELD(x) strcmp(x,field)==0

do_ramps(statf)
struct Cell_stats *statf;
{
	FILE	*fd;

	if (ramp.other) {
		fd = fopen (ramp.other, "r");
	if (fd == NULL)
	{
	return;
	}
	else 
		do_ramp(fd, *statf);
	}
}

do_ramp(fd,  statf)
    FILE *fd;
	struct Cell_stats statf;
{
    double east, north;
	char buf[1024];
	char value[1024];
	char field[1024];
	int width, height;
	double dtmp;
	int orient =0;

	width 	= 20;
	height  = 3;
	while (fgets (buf, sizeof buf, fd) )
	{

        *value = 0;
        *field = 0;
        if (sscanf (buf,"%[^:]:%[^\n]", field, value) < 1) continue;


	if (FIELD("ramp"))
	{
	   draw_ramp(east, north, width, height, orient, &statf);
	continue;
	}

	if (FIELD("orientation") )
	{
	   if (strcmp (value, "horizontal") == NULL)
		orient = 1;
	   else
		orient = 0;
	continue;
	}

        if (FIELD("rwidth"))
        {
	   width = atoi(value);
        continue;
        }

        if (FIELD("rheight"))
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
}

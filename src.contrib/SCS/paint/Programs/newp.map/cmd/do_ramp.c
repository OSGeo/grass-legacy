#include "gis.h"
#include "text.h"
#include "ramp.h"
#include "misc.h"
#include "fullwindow.h"
#include "parms.h"
#include "colormode.h"

#define FIELD(x) strcmp(x,field)==0

do_ramps(pcolr, pcats, statf)
struct Colors *pcolr;
struct Categories *pcats;
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
		do_ramp(fd, pcolr, pcats, statf);
	}
}

do_ramp(fd, pcolr, pcats, statf)
    FILE *fd;
	struct Colors *pcolr;
	struct Categories *pcats;
	struct Cell_stats statf;
{
    double east, north;
	char buf[1024];
	char value[1024];
	char field[1024];
	int width;
	double dtmp;
	int orient =0;

	width 	= 20;
	while (fgets (buf, sizeof buf, fd) )
	{

        *value = 0;
        *field = 0;
        if (sscanf (buf,"%[^:]:%[^\n]", field, value) < 1) continue;


		if (FIELD("ramp"))
		{
		draw_ramp(east, north, width, orient, &pcolr, &pcats, &statf);
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

			
        if (FIELD("cwidth"))
        {
			width	= atoi(value);
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

#include "gis.h"
#include "text.h"
#include "cats.h"
#include "misc.h"
#include "fullwindow.h"
#include "parms.h"
#include "colormode.h"

#define FIELD(x) strcmp(x,field)==0

do_cats(pcolr)
struct Colors *pcolr;

{
	FILE	*fd;

	if (cats.other) {
		fd = fopen (cats.other, "r");
	if (fd == NULL)
	{
	/*
		char msg[100];
		sprintf (msg, "can't open temp cats file %s", catsfile);
		G_warning (msg);
		*/
	return;
	}
	else 
		do_cat(fd, pcolr);
	}
}

do_cat(fd, pcolr)
    FILE *fd;
	struct Colors *pcolr;
{
	int	c;
	int red, grn, blu;

    double east, north;
	char buf[1024];
	char value[1024];
	char field[1024];
	int catnum;
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
			catnum = atoi(value);
			draw_cat(east, north, width, height, catnum, &pcolr);
		


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


}

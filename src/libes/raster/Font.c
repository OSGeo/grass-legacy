#include "raster.h"
#include <stdio.h>
#include "gis.h"
#include "graph.h"

static int select_font(char *);

int R_font(char *name)
{
	if(!select_font (name))
	    select_font ("romand");

	return 0;
}

static int select_font(char *name)
{
	char filename[1024];
	int stat;

	sprintf (filename, "%s/fonts/%s", G_gisbase(), name);

	_send_ident(FONT) ;
	_send_text(filename) ;
	_get_int (&stat);

	return stat == 0;
}

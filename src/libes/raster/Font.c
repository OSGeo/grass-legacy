#include <stdio.h>

#include "graph.h"

R_font(name)
	char *name ;
{
	if(!select_font (name))
	    select_font ("romand");
}
static
select_font(name)
	char *name;
{
	char filename[1024];
	char *G_gisbase();
	int stat;

	sprintf (filename, "%s/fonts/%s", G_gisbase(), name);

	_send_ident(FONT) ;
	_send_text(filename) ;
	_get_int (&stat);

	return stat == 0;
}

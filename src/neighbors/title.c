/* %W% %G% */
#include "gis.h"
#include "ncb.h"

title (type)
    char *type;
{
    char default_title[1024];

    sprintf (default_title,"%dx%d neighborhood: %s of %s",
	    ncb.nsize,
	    ncb.nsize,
	    type,
	    ncb.oldcell.name);

    do
    {
	printf("enter title for <%s> [%s]  ",ncb.newcell.name, default_title);
    }
    while (!G_gets(ncb.title));

    G_strip (ncb.title);
    if (*ncb.title == 0)
	strcpy (ncb.title, default_title);
}

/* Function open_icon_file
**
** This function opens an icon file (in read mode if it exists, in
** write mode if it doesn't).
**
** Author: Paul W. Carlson	May 1992
*/

#include <string.h>
#include "gis.h"
#include "ps_icon.h"

int 
open_icon_file (char *name)
{
    char *mapset;
    char buf[40];
    int n;

    mapset = G_mapset();
    if ((icon.fp = G_fopen_old("ps_icons", name, mapset)) != NULL) 
    {
	fgets(buf, 40, icon.fp);
	n = strlen(buf);
	buf[n - 1] = 0;
	icon.file_exists = 1;
    }
    else
    {
	if ((icon.fp = G_fopen_new("ps_icons", name)) == NULL)
	{
	    fprintf(stderr, "Can't create \"%s\" ps_icon file.\n", name);
	    exit(-1);
	}
	*buf = 0;
	while (*buf == 0)
	{
    	    G_clear_screen();
    	    fprintf (stdout,"\n\n\n\n\n\n");
	    fprintf (stdout,"\t\t\tEnter icon title: ");
	    fgets(buf,40,stdin);
	}
	icon.file_exists = 0;
    }
    G_strip(buf);
    icon.title = G_store(buf);

    return 0;
}

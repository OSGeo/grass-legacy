/* Function open_icon_file
**
** This function opens an icon file (in read mode if it exists, in
** write mode if it doesn't).
**
** Author: Paul W. Carlson	May 1992
*/

#include "gis.h"
#include "ps_icon.h"

open_icon_file(name)
char *name;
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
    	    printf("\n\n\n\n\n\n");
	    printf("\t\t\tEnter icon title: ");
	    gets(buf);
	}
	icon.file_exists = 0;
    }
    G_strip(buf);
    icon.title = G_store(buf);
}

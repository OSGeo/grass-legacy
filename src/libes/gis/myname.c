/**********************************************************************
 *
 *   char *
 *   G_myname()
 *
 *   returns pointer to string containing the descriptive
 *   name of the data base.
 *
 *   note:
 *    This name is the first line in the file 
 *    $GISDBASE/$LOCATION_NAME/PERMANENT/MYNAME
 *
 **********************************************************************/
#include <string.h>
#include "gis.h"

char *
G_myname()
{
    static char name[128];
    char path[500];
    FILE *fd;
    int ok;

    ok = 0;

    G__file_name (path,"","MYNAME","PERMANENT");
    if ((fd = fopen(path,"r")))
    {
	ok = G_getl(name, sizeof name, fd);
	fclose (fd);
    }
    if (!ok)
	strcpy(name, "Unknown Location") ;

    return name;
}

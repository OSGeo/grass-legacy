#include "glob.h"

set_cat (result, cat, pcats)
    CELL result;
    CELL *cat;
    struct Categories *pcats;
{
    int i;
    char buf[1024];
    char item[128];


    if (result == 0) return;
/*
 * the primary file may not have been the first on the command line
 * enforce the results in the order that the files were specified
 */
    *buf = 0;
    for (i = 0; i < nfiles; i++)
    {
	sprintf (item, "%s(%ld) ", names[i], (long) cat[i]);
	strcat (buf, item);
    }
    G_set_cat (result, buf, pcats);
}

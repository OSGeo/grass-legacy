#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "gis.h"

int do_reclass (
    char *old_name,char *old_mapset,char *new_name,
    struct Categories *cats,struct Categories *new_cats,
    CELL *table,
    CELL min,CELL max)
{
    FILE *fd;
    char *tempfile;
    long n;
    int stat;
    char command[1024];
    char *c,*t;

/* write reclass rules to a temp file */
    tempfile = G_tempfile();
    fd = fopen (tempfile, "w");
    if (fd == NULL)
	G_fatal_error ("can't open any tempfiles");
    for (n = min; n <= max; n++)
    {
	if (!G_is_c_null_value(&table[n-min]))
	    fprintf (fd, "%ld = %ld\n", (long) n, (long) table[n-min]);
        else
	    fprintf (fd, "%ld = null\n", (long) n);
    }
    fclose (fd);

/* build the reclass command */
    sprintf (command, "r.reclass input='%s' 'output=%s'",
	G_fully_qualified_name(old_name, old_mapset), new_name);
    if (new_cats->title[0])
    {
	strcat (command, " title='");
	for(c = command; *c; c++)
		;
	for (t = new_cats->title; *t; t++)
	{
	    if (*t == '\'')
	    {
		*c++ = '\'';
		*c++ = '"';
		*c++ = '\'';
		*c++ = '"';
	    }
	    *c++ = *t;
	}
	*c++ = '\'';
	*c = 0;
    }
    strcat (command, " < ");
    strcat (command, tempfile);

/* run reclass */
    stat = system (command);
    unlink (tempfile);
    if (stat == 0)
	G_write_cats (new_name, new_cats);
    return stat;
}

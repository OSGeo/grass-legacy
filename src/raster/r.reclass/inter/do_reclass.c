#include "gis.h"

do_reclass (old_name, old_mapset, new_name, cats, new_cats, table, min, max)
    char *old_name, *old_mapset, *new_name;
    struct Categories *cats, *new_cats;
    long *table;
    CELL min, max;
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
	if (table[n-min])
	    fprintf (fd, "%ld = %ld\n", (long) n, (long) table[n-min]);
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

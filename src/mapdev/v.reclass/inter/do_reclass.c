#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "gis.h"

int do_reclass (char *old_name, char *old_mapset, char *new_name,
    struct Categories *cats, struct Categories *new_cats,
    long *table, int area, int dissolve)
{
    FILE *fd;
    char *tempfile;
    long n;
    int stat;
    char command[1024];
    char *c,*t;
    char buf[20];

    if (area)
	 G_strcpy(buf,"type=area ");
    else
	 G_strcpy(buf,"type=line ");
    if (dissolve) G_strcat(buf,"-d");
/* write reclass rules to a temp file */
    tempfile = G_tempfile();
    fd = fopen (tempfile, "w");
    if (fd == NULL)
	G_fatal_error ("can't open any tempfiles");
    for (n = 0; n <= cats->num; n++)
	if (table[n])
	    fprintf (fd, "%ld = %ld\n", (long) n, (long) table[n]);
    fclose (fd);

/* build the reclass command */
    sprintf (command, "v.reclass input='%s' 'output=%s' %s",
	G_fully_qualified_name(old_name, old_mapset), new_name, buf);
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
	G_write_vector_cats (new_name, new_cats);
    return stat;
}

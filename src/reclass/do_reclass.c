/* %W% %G% */

#include "gis.h"

do_reclass (old_name, old_mapset, new_name, cats, new_cats, table)
    char *old_name, *old_mapset, *new_name;
    struct Categories *cats, *new_cats;
    long *table;
{
    FILE *fd;
    char *tempfile;
    long n;
    int stat;
    char command[1024];

/* write Greclass rules to a temp file */
    tempfile = G_tempfile();
    fd = fopen (tempfile, "w");
    if (fd == NULL)
	G_fatal_error ("can't open any tempfiles");
    for (n = 0; n <= cats->num; n++)
	if (table[n])
	    fprintf (fd, "%ld = %ld\n", (long) n, (long) table[n]);
    fclose (fd);

/* build the Greclass command */
    sprintf (command, "Greclass '%s in %s' '%s'", old_name, old_mapset, new_name);
    if (new_cats->title[0])
    {
	strcat (command, " '");
	strcat (command, new_cats->title);
	strcat (command, "'");
    }
    strcat (command, " < ");
    strcat (command, tempfile);

/* run Greclass */
    stat = system (command);
    unlink (tempfile);
    if (stat == 0)
	G_write_cats (new_name, new_cats);
    return stat;
}

#include "gis.h"

script (command, level) char *command;
{
    char buf[100];

    sprintf (buf, "%d %s", level, command);
    append_key ("script", buf);
}


run_script(choose_window, min_level)
{
    char *get_background_color();
    char *color;
    char **list;
    int count;
    int have_cell;
    char command[100];
    char name_part[50];
    int level, lv, n;
    int max_level;
    FILE *fd;
    extern char *tempfile;

/* open a shell script file */
    fd = fopen (tempfile, "w");
    if (fd == NULL)
    {
	perror (tempfile);
	return;
    }
    fprintf (fd, "#!/bin/sh\n");

/* if min_level == 0, this is a complete redraw with erase
 * otherwise no erase, no new cell file.
 * only scripts at this level or higher.
 */

/* get cell file name */

    get_key ("cell", &list, &count);
    if (have_cell = (count > 0 && list[0][0] != 0))
    {
	strcpy (command, list[0]);
	R_pad_freelist (list, count);
    }

/* get rest of script */
    get_key ("script", &list, &count);
    color=get_background_color();

/* choose a new window */
    if (choose_window) G_system ("Dchoose");

    set_background_color (color);

    if (min_level == 0)
	erase ();
    else
	delete_key ("script");
    if (have_cell)
    {
	if (min_level == 0)
	{
	    sscanf (command, "%s", name_part);
	    fprintf (fd, "echo cell '%s'\n", name_part);
	    fprintf (fd, "Dcell '%s'\n", command);
	}
	set_cell (command);
    }
    else
	set_cell (NULL);

    max_level = 0;
    for (n = 0; n < count; n++)
    {
	if(sscanf (list[n],"%d", &lv) != 1) continue;
	if (lv > max_level)
	    max_level = lv;
	append_key ("script", list[n]);
    }

    for (level = min_level; level <= max_level; level++)
    {
	for (n = 0; n < count; n++)
	{
	    if (sscanf (list[n], "%d %[^\n]", &lv, command) != 2) continue;
	    if (lv == level)
	    {
		fprintf (fd, "%s\n", command);
	    }
	}
    }

    if (count)
	R_pad_freelist (list, count);
    fclose (fd);
    if (count || have_cell)
    {
	chmod (tempfile, 0700);
	G_system (tempfile);
    }
}

set_cell (command) char *command;
{
    if (command)
	set_key ("cell", command);
    else
	delete_key ("cell");
}

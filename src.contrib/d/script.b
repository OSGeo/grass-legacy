#include "gis.h"

static int max_level = 0;
static int any = 0;
extern char *scriptfile;
extern char *tempfile;
static char window_name[128];
static char cell_call[128];

begin_script()
{
    clear_script();
}

script (command, level) char *command;
{
    FILE *fd;

    fd = fopen (scriptfile, any?"a":"w");
    if (fd)
    {
	fprintf (fd, "%d\n", level);
	fprintf (fd, "%s\n", command);
	fclose (fd);
	any = 1;
	if (level > max_level) max_level = level;
    }
    else perror (scriptfile);
}

clear_script()
{
    unlink (scriptfile);
    any = 0;
    max_level = 0;
    *cell_call = 0;
}

run_script(choose_window)
{
    FILE *in;
    FILE *out;
    char buf[1000];
    int level, n;

    if (choose_window) G_system ("Dchoose");
    erase (0,0);
    if (!any && *cell_call == 0) return;

    if (any)
    {
	in = fopen (scriptfile, "r");
	if (in == NULL)
	{
	    perror (scriptfile);
	    return;
	}
    }
    out = fopen (tempfile, "w");
    if (out == NULL)
    {
	if (any)
	    fclose (in);
	perror (tempfile);
	return;
    }
    fprintf (out, "#!/bin/sh\n");
    if (cell_call)
	fprintf (out, "%s\n", cell_call);
    if (any)
    {
	for (level = 1; level <= max_level; level++)
	{
	    rewind (in);
	    while (fgets (buf, sizeof buf, in))
	    {
		if (sscanf (buf, "%d", &n) != 1) break;
		if (fgets(buf, sizeof buf, in) == NULL)
		    break;
		if (n != level) continue;
		fprintf (out, "%s", buf);
	    }
	}
	fclose (in);
    }
    fclose (out);
    chmod (tempfile, 0700);
    G_system (tempfile);
}

set_cell (name) char *name;
{
    *cell_call = 0;
    if (name)
	sprintf (cell_call, "Dcell '%s'", name);
}

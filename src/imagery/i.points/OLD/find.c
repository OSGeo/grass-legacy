#include "globals.h"
find_gis_files (fd, element)
    FILE *fd;
    char *element;
{
    int len, len1, len2;
    char *mapset;
    char command[1024];
    char name[40];
    char *dir;
    char *G__mapset_name();
    int n;
    FILE *ls, *popen();

    strcpy (command, "ls ");
    dir = command + strlen (command);

    len1 = len2 = 0;
    fseek (fd, 0L, 0);
    fwrite (&len1, sizeof(len1), 1, fd);
    fwrite (&len2, sizeof(len2), 1, fd);
    for (n=0; (mapset = G__mapset_name(n)) != NULL; n++)
    {
	G__file_name (dir, element, "", mapset);
	if (access (dir,0) != 0)
	    continue;
	ls = popen (command, "r");
	if (ls == NULL) continue;

	len = strlen (mapset);
	if (len > len2)
	    len2 = len;
	while (fscanf (ls, "%s", name) == 1)
	{
	    fprintf (fd, "%s %s\n", name, mapset);
	    len = strlen (name);
	    if (len > len1)
		len1 = len;
	}
	pclose (ls);
    }
    fflush (fd);
    fseek (fd, 0L, 0);
    fwrite (&len1, sizeof(len1), 1, fd);
    fwrite (&len2, sizeof(len2), 1, fd);
}

find_target_cellfiles()
{
    FILE *fd;

    if (cell_list != NULL) return;

    cell_list = G_tempfile();
    fd = fopen (cell_list, "w");
    if (fd == NULL)
	G_fatal_error ("Can't open any tempfiles");
    select_target_env();
    find_gis_files (fd, "cell");
    select_current_env();
    fclose (fd);
}

#include "dba_imagery.h"

I_get_block_target (block, location, mapset)
    char *block;
    char *location;
    char *mapset;
{
    FILE *fd;
    int ok;

    *location = *mapset = 0;
    G_suppress_warnings (1);
    fd = I_fopen_block_file_old (block, "TARGET");
    G_suppress_warnings (0);
    if (fd == NULL)
	return 0;
    ok = (fscanf (fd, "%s %s", location, mapset) == 2);
    fclose (fd);
    if (!ok)
    {
	char msg[100];
	*location = *mapset = 0;
	sprintf (msg, "unable to read target file for block [%s]", block);
	G_warning (msg);
    }
    return ok;
}

I_put_block_target (block, location, mapset)
    char *block;
    char *location;
    char *mapset;
{
    FILE *fd;

    fd = I_fopen_block_file_new (block, "TARGET");
    if (fd == NULL)
	return 0;
    fprintf (fd, "%s\n%s\n", location, mapset);
    fclose (fd);
    return 1;
}

#include "dba_imagery.h"

I_get_block_title (block, title, n)
    char *block;
    char *title;
{
    FILE *fd;

    *title = 0;
    G_suppress_warnings(1);
    fd = I_fopen_block_file_old (block, "TITLE");
    G_suppress_warnings(0);
    if (fd != NULL)
    {
	G_getl (title, n, fd);
	fclose (fd);
    }
    return fd != NULL;
}

I_put_block_title (block, title)
    char *block;
    char *title;
{
    FILE *fd;
    fd = I_fopen_block_file_new (block, "TITLE");
    if (fd != NULL)
    {
	fprintf (fd, "%s\n", title);
	fclose (fd);
    }
    return fd != NULL;
}


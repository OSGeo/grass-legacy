#include "gis.h"
/******************************************************
* I_open_block_file_new()
* I_open_block_file_old()
*
* open new and old imagery block files in the current mapset
*******************************************************/

I_open_block_file_new (block, file)
    char *block;
    char *file;
{
    int fd;
    char element[100];

/* get block element name */
    sprintf (element, "block/%s", block);

    fd = G_open_new (element, file);
    if (fd < 0)
	error (block, file, "can't create ", "");
    return fd;
}

I_open_block_file_old (block, file)
    char *block;
    char *file;
{
    int fd;
    char element[100];

/* find the file first */
    if (!I_find_block_file (block, file))
    {
	error (block, file, "", " not found");
	return -1;
    }

/* get block element name */
    sprintf (element, "block/%s", block);

    fd = G_open_old (element, file, G_mapset());
    if (fd < 0)
	error (block, file, "can't open ", "");
    return fd;
}

static error (block, file, msga, msgb)
    char *block;
    char *file;
    char *msga;
    char *msgb;
{
    char buf[100];
    sprintf (buf, "%sfile [%s] of block [%s in %s]%s",
	msga, file, block, G_mapset(), msgb);
    G_warning (buf);
}





#include "gis.h"
/******************************************************
* I_fopen_block_file_new()
* I_fopen_block_file_append()
* I_fopen_block_file_old()
*
* fopen new block files in the current mapset
* fopen old block files anywhere
*******************************************************/

FILE *
I_fopen_block_file_new (block, file)
    char *block;
    char *file;
{
    FILE *fd;
    char element[100];

/* get block element name */
    sprintf (element, "block/%s", block);

    fd = G_fopen_new (element, file);
    if (!fd)
	error (block, file, "can't create ", "");
    return fd;
}

FILE *
I_fopen_block_file_append (block, file)
    char *block;
    char *file;
{
    FILE *fd;
    char element[100];

/* get block element name */
    sprintf (element, "block/%s", block);

    fd = G_fopen_append (element, file);
    if (!fd)
	error (block, file, "unable to open ", "");
    return fd;
}

FILE *
I_fopen_block_file_old (block, file)
    char *block;
    char *file;
{
    FILE *fd;
    char element[100];

/* find file first */
    if (!I_find_block_file (block, file))
    {
	error (block, file, "", " not found");
	return ((FILE *) NULL);
    }

/* get block element name */
    sprintf (element, "block/%s", block);

    fd = G_fopen_old (element, file, G_mapset());
    if (!fd)
	error (block, file, "can't open ", "");
    return fd;
}

static error (block,file,msga,msgb)
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

FILE *
I_fopen_subblock_file_new (block, subblock, file)
    char *block;
    char *subblock;
    char *file;
{
    FILE *fd;
    char element[300];

/* get subblock element name */
    sprintf (element, "block/%s/subblock/%s", block, subblock);

    fd = G_fopen_new (element, file);
    if (!fd)
	error2 (block, subblock, file, "can't create ", "");
    return fd;
}

FILE *
I_fopen_subblock_file_append (block, subblock, file)
    char *block;
    char *subblock;
    char *file;
{
    FILE *fd;
    char element[300];

/* get subblock element name */
    sprintf (element, "block/%s/subblock/%s", block, subblock);

    fd = G_fopen_append (element, file);
    if (!fd)
	error2 (block, subblock, file, "unable to open ", "");
    return fd;
}

FILE *
I_fopen_subblock_file_old (block, subblock, file)
    char *block;
    char *subblock;
    char *file;
{
    FILE *fd;
    char element[300];

/* find file first */
    if (!I_find_subblock_file (block, subblock, file))
    {
	error2 (block, subblock, file, "", " not found");
	return ((FILE *) NULL);
    }

/* get subblock element name */
    sprintf (element, "block/%s/subblock/%s", block, subblock);

    fd = G_fopen_old (element, file, G_mapset());
    if (!fd)
	error2 (block, subblock, file, "can't open ", "");
    return fd;
}

static error2 (block,subblock,file,msga,msgb)
    char *block;
    char *subblock;
    char *file;
    char *msga;
    char *msgb;
{
    char buf[200];
    sprintf (buf, "%sfile [%s] for subblock [%s] of block [%s in %s]%s",
	msga, file, subblock, block, G_mapset(), msgb);
    G_warning (buf);
}





/**************************************************************
* I_find_block (block)
*
* Find the a block in the current mapset
**************************************************************/
#include "gis.h"

I_find_block (block)
    char *block;
{
    if (block == NULL || *block == 0)
	return 0;

    return G_find_file ("block", block, G_mapset()) != NULL ;
}

I_find_block_file (block, file)
    char *block;
    char *file;
{
    char element[100];

    if (block == NULL || *block == 0)
	return 0;
    if (file == NULL || *file == 0)
	return 0;

    sprintf (element, "block/%s", block);

    return G_find_file (element, file, G_mapset()) != NULL ;
}

I_find_subblock (block, subblock)
    char *block;
    char *subblock;
{
    char element[300];

    if (block == NULL || *block == 0)
	return 0;
    if (subblock == NULL || *subblock == 0)
	return 0;

    sprintf (element, "block/%s/subblock", block);

    return G_find_file (element, subblock, G_mapset()) != NULL ;
}

I_find_subblock_file (block, subblock, file)
    char *block;
    char *subblock;
    char *file;
{
    char element[300];

    if (block == NULL || *block == 0)
	return 0;
    if (subblock == NULL || *subblock == 0)
	return 0;
    if (file == NULL || *file == 0)
	return 0;

    sprintf (element, "block/%s/subblock/%s", block, subblock);

    return G_find_file (element, file, G_mapset()) != NULL ;
}





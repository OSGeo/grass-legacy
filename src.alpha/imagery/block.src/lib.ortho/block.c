/**********************************************************
* I_get_block (block);
* I_put_block (block);
*
* I_get_block_Block_Image_Group_Ref (block, &Block_Image_Group_Ref);
* I_put_block_Block_Image_Group_Ref (block, &Block_Image_Group_Ref);
* I_get_subblock_Block_Image_Group_Ref_file (block, subblock, &Block_Image_Group_Ref);
* I_put_subblock_Block_Image_Group_Ref_file (block, subblock, &Block_Image_Group_Ref);
* I_add_file_to_block_Block_Image_Group_Ref (name, mapset, &Block_Image_Group_Ref)
* I_transfer_block_Block_Image_Group_Ref_file (&Src_Block_Image_Group_Ref, n, &Dst_Block_Image_Group_Ref)
* I_init_block_Block_Image_Group_Ref (&Block_Image_Group_Ref);
* I_free_block_Block_Image_Group_Ref (&Block_Image_Group_Ref);
**********************************************************/
#include "dba_imagery.h"

/* get current block name from file BLOCK in current mapset */
I_get_block (block)
    char *block;
{
    FILE *fd;
    int stat;

    *block = 0;
    G_suppress_warnings(1);
    fd = G_fopen_old ("", "BLOCK", G_mapset());
    G_suppress_warnings(0);
    if (fd == NULL)
	return 0;
    stat = (fscanf (fd, "%s", block) == 1);
    fclose (fd);
    return stat;
}

/* write block name to file BLOCK in current mapset */
I_put_block (block)
    char *block;
{
    FILE *fd;

    fd = G_fopen_new ("", "BLOCK");
    if (fd == NULL)
	return 0;
    fprintf (fd, "%s\n", block);
    fclose (fd);
    return 1;
}

/* get current subblock for block in current mapset */
I_get_subblock (block, subblock)
    char *block, *subblock;
{
    FILE *fd;
    FILE *I_fopen_block_file_old();
    int stat;

    *subblock = 0;
    if (!I_find_block(block))
	return 0;
    G_suppress_warnings(1);
    fd = I_fopen_block_file_old (block, "SUBBLOCK");
    G_suppress_warnings(0);
    if (fd == NULL)
	return 0;
    stat = (fscanf (fd, "%s", subblock) == 1);
    fclose (fd);
    return stat;
}

/* write current subblock to block in current mapset */
I_put_subblock (block, subblock)
    char *block, *subblock;
{
    FILE *fd;
    FILE *I_fopen_block_file_new();

    if (!I_find_block(block))
	return 0;
    fd = I_fopen_block_file_new (block, "SUBBLOCK");
    if (fd == NULL)
	return 0;
    fprintf (fd, "%s\n", subblock);
    fclose (fd);
    return 1;
}

I_get_block_Block_Image_Group_Ref (block, Block_Image_Group_Ref)
    char *block;
    struct Block_Image_Group_Ref *Block_Image_Group_Ref;
{
    return get_Block_Image_Group_Ref (block, "", Block_Image_Group_Ref);
}

I_get_subblock_Block_Image_Group_Ref (block, subblock, Block_Image_Group_Ref)
    char *block;
    char *subblock;
    struct Block_Image_Group_Ref *Block_Image_Group_Ref;
{
    return get_Block_Image_Group_Ref (block, subblock, Block_Image_Group_Ref);
}

static
get_Block_Image_Group_Ref (block, subblock, Block_Image_Group_Ref)
    char *block;
    char *subblock;
    struct Block_Image_Group_Ref *Block_Image_Group_Ref;
{
    int n;
    char buf[200];
    char name[30], mapset[30];
    char color[20];
    FILE *fd;
    FILE *I_fopen_block_Block_Image_Group_Ref_old();
    FILE *I_fopen_subblock_Block_Image_Group_Ref_old();

    I_init_block_Block_Image_Group_Ref (Block_Image_Group_Ref);

    G_suppress_warnings(1);
    if (*subblock == 0)
    {
	fd = I_fopen_block_Block_Image_Group_Ref_old(block) ;
    }
    else
    {
	fd = I_fopen_subblock_Block_Image_Group_Ref_old(block,subblock) ;
    }
    G_suppress_warnings(0);
    if (!fd) return 0;

    while (fgets(buf, sizeof buf, fd))
    {
	n=sscanf (buf, "%29s %29s", name, mapset);
	if (n==2)
	{
	    I_add_file_to_block_Block_Image_Group_Ref (name, mapset, Block_Image_Group_Ref);
	}
    }

    fclose (fd);
    return 1;
}

I_put_block_Block_Image_Group_Ref (block, Block_Image_Group_Ref)
    char *block;
    struct Block_Image_Group_Ref *Block_Image_Group_Ref;
{
    return put_Block_Image_Group_Ref (block, "", Block_Image_Group_Ref);
}

I_put_subblock_Block_Image_Group_Ref (block, subblock, Block_Image_Group_Ref)
    char *block;
    char *subblock;
    struct Block_Image_Group_Ref *Block_Image_Group_Ref;
{
    return put_Block_Image_Group_Ref (block, subblock, Block_Image_Group_Ref);
}

static
put_Block_Image_Group_Ref (block, subblock, Block_Image_Group_Ref)
    char *block;
    char *subblock;
    struct Block_Image_Group_Ref *Block_Image_Group_Ref;
{
    int n;
    FILE *fd;
    FILE *I_fopen_block_Block_Image_Group_Ref_new();
    FILE *I_fopen_subblock_Block_Image_Group_Ref_new();


    if (*subblock == 0)
	fd = I_fopen_block_Block_Image_Group_Ref_new(block) ;
    else
	fd = I_fopen_subblock_Block_Image_Group_Ref_new(block,subblock) ;
    if (!fd) return 0;

    for (n=0; n < Block_Image_Group_Ref->nfiles; n++)
    {
	fprintf (fd, "%s %s", Block_Image_Group_Ref->file[n].name, Block_Image_Group_Ref->file[n].mapset);
	fprintf (fd, "\n");
    }
    fclose (fd);
    return 1;
}

I_add_file_to_block_Block_Image_Group_Ref (name, mapset, Block_Image_Group_Ref)
    char *name;
    char *mapset;
    struct Block_Image_Group_Ref *Block_Image_Group_Ref;
{
    int n;

    for (n = 0; n < Block_Image_Group_Ref->nfiles; n++)
    {
	if (strcmp (Block_Image_Group_Ref->file[n].name, name) == 0
	&&  strcmp (Block_Image_Group_Ref->file[n].mapset, mapset) == 0)
		return n;
    }

    if (n = Block_Image_Group_Ref->nfiles++)
	Block_Image_Group_Ref->file = (struct Block_Image_Group_Ref_Files *) G_realloc (Block_Image_Group_Ref->file, Block_Image_Group_Ref->nfiles * sizeof (struct Block_Image_Group_Ref_Files));
    else
	Block_Image_Group_Ref->file = (struct Block_Image_Group_Ref_Files *) G_malloc (Block_Image_Group_Ref->nfiles * sizeof (struct Block_Image_Group_Ref_Files));
    strcpy (Block_Image_Group_Ref->file[n].name, name);
    strcpy (Block_Image_Group_Ref->file[n].mapset, mapset);
    return n;
}

I_transfer_block_Block_Image_Group_Ref_file (Block_Image_Group_Ref2, n, Block_Image_Group_Ref1)
    struct Block_Image_Group_Ref *Block_Image_Group_Ref1, *Block_Image_Group_Ref2;
{
    int k;

/* insert old name into new Block_Image_Group_Ref */
    k = I_add_file_to_block_Block_Image_Group_Ref(Block_Image_Group_Ref2->file[n].name, Block_Image_Group_Ref2->file[n].mapset, Block_Image_Group_Ref1);

}


I_init_block_Block_Image_Group_Ref (Block_Image_Group_Ref)
    struct Block_Image_Group_Ref *Block_Image_Group_Ref;
{
    Block_Image_Group_Ref->nfiles = 0;
}

I_free_block_Block_Image_Group_Ref (Block_Image_Group_Ref)
    struct Block_Image_Group_Ref *Block_Image_Group_Ref;
{
    if (Block_Image_Group_Ref->nfiles > 0)
	free (Block_Image_Group_Ref->file);
    Block_Image_Group_Ref->nfiles = 0;
}




#include <stdio.h>
#define OLD 1
#define NEW 2

I_vask_block_new (info, block, cancel_msg)
    char **info;
    char *block;
    char *cancel_msg;
{
    return ask (info, block, (char *) NULL, NEW, 1, cancel_msg);
}

I_vask_block_old (info, block, cancel_msg)
    char **info;
    char *block;
    char *cancel_msg;
{
    return ask (info, block, (char *) NULL, OLD, 1, cancel_msg);
}

I_vask_subblock_new (info, block, subblock, both, cancel_msg)
    char **info;
    char *block;
    char *subblock;
    char *cancel_msg;
{
    return ask (info, block, subblock, NEW, both, cancel_msg);
}

I_vask_subblock_old (info, block, subblock, both, cancel_msg)
    char **info;
    char *block;
    char *subblock;
    char *cancel_msg;
{
    return ask (info, block, subblock, OLD, both, cancel_msg);
}

static
ask (info, block, subblock, type, both, cancel_msg)
    char **info;
    char *block, *subblock;
    char *cancel_msg;
{
    int line;
    char tblock[30], tsubblock[30];

/* read the current block and save its name */
    if (both)
    {
	I_get_block (block);
    }
    else
    {
	if (subblock == NULL)
	    G_fatal_error ("vask_block: illegal use by programmer");
	if(!I_find_block(block))
		return 0;
    }

    strcpy (tblock, block);

/* do the same for the subblock */
    if (subblock != NULL)
    {
	if (type == OLD)
	    I_get_subblock (block, subblock);
	else
	    *subblock = 0;
	strcpy (tsubblock, subblock);
    }

/* set up the screen prompts */
    V_clear();
    if (cancel_msg != NULL && *cancel_msg != 0)
	V_intrpt_msg (cancel_msg);
    line = 0;
    while (*info)
	V_line (line++, *info++);
    line += 2;
    if (both)
    {
	V_line (line, "BLOCK:                           (list will show available blocks)");
	V_ques (block, 's', line++, 10, 20);
    }
    else
    {
	V_line (line, "BLOCK:");
	V_const (block, 's', line++, 10, 20);
    }
    if (subblock != NULL)
    {
	V_line (line, "SUBBLOCK:                        (list will show available subblocks)");
	V_ques (subblock, 's', line++, 10, 20);
    }


    while(1)
    {
/* ask the questions */
	V_intrpt_ok();
	if (!V_call())
	    return 0;

/* check the block name */
	G_strip (block);
	if (subblock != NULL)
	    G_strip (subblock);
	if (*block == 0 || strcmp (block, "list") == 0)
	{
	    I_list_blocks (1);
	    strcpy (block, tblock);
	    continue;
	}
	strcpy (tblock, block);
/* if type is OLD, or looking for subblock as well, block must exist */
	if (subblock != NULL || type == OLD)
	{
	    if (!I_find_block(block))
	    {
		printf ("block [%s] not found\n", block);
		I_list_blocks (1);
		continue;
	    }
	}
/*
 * new block request is checked for legal names
 * caller must check if the block exists or not using I_find_block()
 */
	else if (G_legal_filename (block) >= 0)
	    break;
	else
	{
	    printf ("[%s] ** illegal block name **", block);
	    I_list_blocks (1);
	    continue;
	}

/* check the subblock */
	if (subblock != NULL)
	{
	    if (*subblock == 0 || strcmp (subblock, "list") == 0)
	    {
		I_list_subblocks (block, 1);
		strcpy (subblock, tsubblock);
		continue;
	    }
	    strcpy (tsubblock, subblock);
	    if (type == OLD && !I_find_subblock(block, subblock))
	    {
		printf ("subblock [%s] not found\n", subblock);
		I_list_subblocks (block, 1);
		continue;
	    }
	    if (type == NEW && I_find_subblock(block, subblock))
	    {
		printf ("subblock [%s] already exists. choose another name\n",
		    subblock);
		I_list_subblocks (block, 1);
		continue;
	    }
	    if (type == NEW && G_legal_filename (subblock) < 0)
	    {
		printf ("[%s] ** illegal subblock name **\n", subblock);
		I_list_subblocks (block, 1);
		continue;
	    }
	}
	break;
    }
    I_put_block (block);
    if (subblock != NULL)
	I_put_subblock (block, subblock);
    return 1;
}

/*************************************************************
* I_ask_subblock_old (prompt,block,subblock)
* I_ask_subblock_new (prompt,block,subblock)
*
* prompt the user for an imagery subblock name
*************************************************************/
#include "dba_imagery.h"

I_ask_subblock_old (prompt,block,subblock)
    char *prompt;
    char *block;
    char *subblock;
{
    char pmt[100];

    if (*prompt == 0)
	sprintf(prompt=pmt, "Select a subblock from block [%s]", block);
    while(1)
    {
	if (!ask_subblock(prompt, block, subblock))
	    return 0;
	if (I_find_subblock (block, subblock))
	    return 1;
	printf ("\n** %s - not found **\n\n", subblock);
    }
}

I_ask_subblock_new (prompt, block, subblock)
    char *prompt;
    char *block;
    char *subblock;
{
    char pmt[100];

    if (*prompt == 0)
	sprintf(prompt=pmt, "Enter a new subblock for block [%s]", block);
    while(1)
    {
	if (!ask_subblock(prompt, block, subblock))
	    return 0;
	if (!I_find_subblock (block, subblock))
	    return 1;
	printf ("\n** %s - exists, select another name **\n\n", subblock);
    }
}

static
ask_subblock (prompt, block, subblock)
    char *prompt;
    char *block;
    char *subblock;
{
    char buf[1024];

    while (1)
    {
	printf ("\n%s\n", prompt);
	printf ("Enter 'list' for a list of subblocks in block [%s]\n", block);
	printf ("Enter 'list -f' for a verbose listing\n");
	printf ("Hit RETURN %s\n", G_get_ask_return_msg());
	printf ("> ");
	if (!G_gets(buf)) continue;

	G_squeeze (buf);
	printf ("<%s>\n", buf);
	if (*buf == 0) return 0;

	if (strcmp (buf, "list") == 0)
	    I_list_subblocks (block, 0);
	else if (strcmp (buf, "list -f") == 0)
	    I_list_subblocks (block, 1);
	else if (G_legal_filename(buf) < 0)
	    printf ("\n** <%s> - illegal name **\n\n", buf);
	else
	    break;
    }
    strcpy (subblock, buf);
    return 1;
}

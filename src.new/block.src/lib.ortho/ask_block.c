/*************************************************************
* I_ask_block_old (prompt,block)
* I_ask_block_new (prompt,block)
* I_ask_block_any (prompt,block)
*
* prompt the user for an imagery block file name
*************************************************************/
#include "dba_imagery.h"

I_ask_block_old (prompt,block)
    char *prompt;
    char *block;
{
    while(1)
    {
	if (*prompt == 0)
	    prompt = "Select an imagery block file";
	if (!ask_block(prompt, block))
	    return 0;
	if (I_find_block (block))
	    return 1;
	printf ("\n** %s - not found **\n\n", block);
    }
}

I_ask_block_new (prompt, block)
    char *prompt;
    char *block;
{
    while(1)
    {
	if (*prompt == 0)
	    prompt = "Enter a new imagery block file name";
	if (!ask_block(prompt, block))
	    return 0;
	if (!I_find_block (block))
	    return 1;
	printf ("\n** %s - exists, select another name **\n\n", block);
    }
}

I_ask_block_any (prompt,block)
    char *prompt;
    char *block;
{
    if (*prompt == 0)
	prompt = "Enter a new or existing imagery block file";
    return ask_block(prompt, block);
}

static
ask_block (prompt, block)
    char *prompt;
    char *block;
{
    char buf[1024];

    while (1)
    {
	printf ("\n%s\n", prompt);
	printf ("Enter 'list' for a list of existing imagery blocks\n");
	printf ("Enter 'list -f' for a verbose listing\n");
	printf ("Hit RETURN %s\n", G_get_ask_return_msg());
	printf ("> ");
	if (!G_gets(buf)) continue;

	G_squeeze (buf);
	printf ("<%s>\n", buf);
	if (*buf == 0) return 0;

	if (strcmp (buf, "list") == 0)
	    I_list_blocks(0);
	else if (strcmp (buf, "list -f") == 0)
	    I_list_blocks(1);
	else if (G_legal_filename(buf) < 0)
	    printf ("\n** <%s> - illegal name **\n\n", buf);
	else
	    break;
    }
    strcpy (block, buf);
    return 1;
}

/*************************************************************
* I_ask_group_old (prompt,group)
* I_ask_group_new (prompt,group)
* I_ask_group_any (prompt,group)
*
* prompt the user for an imagery group file name
*************************************************************/
#include "imagery.h"

I_ask_group_old (prompt,group)
    char *prompt;
    char *group;
{
    while(1)
    {
	if (*prompt == 0)
	    prompt = "Select an imagery group file";
	if (!ask_group(prompt, group))
	    return 0;
	if (I_find_group (group))
	    return 1;
	printf ("\n** %s - not found **\n\n", group);
    }
}

I_ask_group_new (prompt, group)
    char *prompt;
    char *group;
{
    while(1)
    {
	if (*prompt == 0)
	    prompt = "Enter a new imagery group file name";
	if (!ask_group(prompt, group))
	    return 0;
	if (!I_find_group (group))
	    return 1;
	printf ("\n** %s - exists, select another name **\n\n", group);
    }
}

I_ask_group_any (prompt,group)
    char *prompt;
    char *group;
{
    if (*prompt == 0)
	prompt = "Enter a new or existing imagery group file";
    return ask_group(prompt, group);
}

static
ask_group (prompt, group)
    char *prompt;
    char *group;
{
    char buf[1024];

    while (1)
    {
	printf ("\n%s\n", prompt);
	printf ("Enter 'list' for a list of existing imagery groups\n");
	printf ("Enter 'list -f' for a verbose listing\n");
	printf ("Hit RETURN %s\n", G_get_ask_return_msg());
	printf ("> ");
	if (!G_gets(buf)) continue;

	G_squeeze (buf);
	printf ("<%s>\n", buf);
	if (*buf == 0) return 0;

	if (strcmp (buf, "list") == 0)
	    I_list_groups(0);
	else if (strcmp (buf, "list -f") == 0)
	    I_list_groups(1);
	else if (G_legal_filename(buf) < 0)
	    printf ("\n** <%s> - illegal name **\n\n", buf);
	else
	    break;
    }
    strcpy (group, buf);
    return 1;
}

/*************************************************************
* I_ask_subgroup_old (prompt,group,subgroup)
* I_ask_subgroup_new (prompt,group,subgroup)
*
* prompt the user for an imagery subgroup name
*************************************************************/
#include "imagery.h"

I_ask_subgroup_old (prompt,group,subgroup)
    char *prompt;
    char *group;
    char *subgroup;
{
    char pmt[100];

    if (*prompt == 0)
	sprintf(prompt=pmt, "Select a subgroup from group [%s]", group);
    while(1)
    {
	if (!ask_subgroup(prompt, group, subgroup))
	    return 0;
	if (I_find_subgroup (group, subgroup))
	    return 1;
	printf ("\n** %s - not found **\n\n", subgroup);
    }
}

I_ask_subgroup_new (prompt, group, subgroup)
    char *prompt;
    char *group;
    char *subgroup;
{
    char pmt[100];

    if (*prompt == 0)
	sprintf(prompt=pmt, "Enter a new subgroup for group [%s]", group);
    while(1)
    {
	if (!ask_subgroup(prompt, group, subgroup))
	    return 0;
	if (!I_find_subgroup (group, subgroup))
	    return 1;
	printf ("\n** %s - exists, select another name **\n\n", subgroup);
    }
}

static
ask_subgroup (prompt, group, subgroup)
    char *prompt;
    char *group;
    char *subgroup;
{
    char buf[1024];

    while (1)
    {
	printf ("\n%s\n", prompt);
	printf ("Enter 'list' for a list of subgroups in group [%s]\n", group);
	printf ("Enter 'list -f' for a verbose listing\n");
	printf ("Hit RETURN %s\n", G_get_ask_return_msg());
	printf ("> ");
	if (!G_gets(buf)) continue;

	G_squeeze (buf);
	printf ("<%s>\n", buf);
	if (*buf == 0) return 0;

	if (strcmp (buf, "list") == 0)
	    I_list_subgroups (group, 0);
	else if (strcmp (buf, "list -f") == 0)
	    I_list_subgroups (group, 1);
	else if (G_legal_filename(buf) < 0)
	    printf ("\n** <%s> - illegal name **\n\n", buf);
	else
	    break;
    }
    strcpy (subgroup, buf);
    return 1;
}

/* %W% %G% */
#include "externs.h"
#include "gis.h"
set_mapset_path ()
{
	char command[1024];
	int pathoffset;
	int n;
	int i;
	int skip;
	char *cur_mapset;

	if (nchoices == 0)	/* they didn't choose any, exit w/out change */
		return;

	cur_mapset = G_mapset();

	strcpy (command, "echo '< '`Gmapsets ");	/* start building command string */
	pathoffset = strlen (command);	/* get pointer to path list */


/*
 * make sure current mapset is specified in the list
 * if not add it to the head of the list
 */

	skip = 0;
	for (n = 0; n < nchoices; n++)
		if (strcmp (cur_mapset, mapset_name[choice[n]]) == 0)
		{
			skip = 1;
			break;
		}
	if (!skip)
	{
		strcat (command, cur_mapset);
		strcat (command, " ");
	}

/*
 * output the list, removing duplicates
 */
	for (n = 0; n < nchoices; n++)
	{
		skip = 0;
		for (i = 0; i < n; i++)
			if (strcmp (mapset_name[choice[i]], mapset_name[choice[n]]) == 0)
			{
				skip = 1;
				break;
			}
		if (!skip)
		{
			strcat (command, mapset_name[choice[n]]);
			strcat (command, " ");
		}
	}
	strcat (command, "`'>'");

	printf("\nMapset search list set to\n ");
	fflush (stdout);
	if (system (command) == 0)
	{
	    return (0);
	}
	else
	{
	    G_warning ("mapsets: call to Gmapsets failed");
	    return (-1);
	}
}

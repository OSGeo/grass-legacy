#include "imagery.h"
find_all_cellfiles (fd, ref)
    FILE *fd;
    struct Ref *ref;
{
    char *mapset;
    char command[1024];
    char name[40];
    char *dir;
    char *G__mapset_name();
    int any;
    int i, n;
    FILE *ls, *popen();

    strcpy (command, "ls ");
    dir = command + strlen (command);

    for (n=0; (mapset = G__mapset_name(n)) != NULL; n++)
    {
	any = 0;
	G__file_name (dir, "cell", "", mapset);
	if (access (dir,0) != 0)
	    continue;
	ls = popen (command, "r");
	if (ls == NULL) continue;
	while (fscanf (ls, "%s", name) == 1)
	{
/*
 * ignore files already in ref list
 */
	    for (i = 0; i < ref->nfiles; i++)
		if (strcmp (name, ref->file[i].name) == 0 &&
		    strcmp (mapset, ref->file[i].mapset) == 0)
			break;
	    if (i < ref->nfiles)
		continue;
/*
 * first file in mapset, must write mapset name
 */
	    if (!any)
	    {
		fprintf (fd, "#%s\n", mapset);
	    	any = 1;
	    }
	    fprintf (fd, " %s\n", name);
	}
	pclose (ls);
    }
}

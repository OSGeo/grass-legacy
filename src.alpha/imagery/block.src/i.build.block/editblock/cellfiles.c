#include "dba_imagery.h"

find_all_groupfiles (fd, Block_Image_Group_Ref)
    FILE *fd;
    struct Block_Image_Group_Ref *Block_Image_Group_Ref;
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
	G__file_name (dir, "group", "", mapset);
	if (access (dir,0) != 0)
	    continue;
	ls = popen (command, "r");
	if (ls == NULL) continue;
	while (fscanf (ls, "%s", name) == 1)
	{
/*
 * ignore files already in Block_Image_Group_Ref list
 */
	    for (i = 0; i < Block_Image_Group_Ref->nfiles; i++)
		if (strcmp (name, Block_Image_Group_Ref->file[i].name) == 0 &&
		    strcmp (mapset, Block_Image_Group_Ref->file[i].mapset) == 0)
			break;
	    if (i < Block_Image_Group_Ref->nfiles)
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









#include <stdlib.h>
#include "gis.h"

int more_print (char *tmpname)
{
    char line[80],command[256],name[128];

/*    sprintf(command,"clear; more -d %s",tmpname);*/
     sprintf(command,"clear; $PAGER %s",tmpname);
    system(command);
    if (G_yes("Do you wish to print this report out? ", 0))
    {
	sprintf(command,"lpr %s",tmpname);
	system(command);
    }
    while (G_yes("Do you wish to save this report in a file? ",0))
    {
	fprintf (stdout,"Enter the file name --> ");
	if (!G_gets(line)) continue;
	if (sscanf (line, "%s", name) != 1) continue;
	if(name[0] != '/')
	{
	    sprintf (command,"cp %s %s/%s",tmpname,G_home(),name);
	    fprintf (stdout,"'%s' being saved in your home directory\n",name);
	}
	else
	{
	    sprintf (command,"cp %s %s",tmpname,name);
	    fprintf (stdout,"'%s' being saved\n",name);
	}
	system(command);
	break;
    }
    sprintf(command,"rm %s",tmpname);
    system(command);

    return 0;
}

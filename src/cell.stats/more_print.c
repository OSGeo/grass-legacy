#include "gis.h"

more_print(tmpname, unexpected_cats)
    char *tmpname;
{
    char ans[128],line[80],command[256],name[128];

    sprintf(command,"clear; more -d %s",tmpname);
    system(command);
    if (unexpected_cats)
    {
	printf("\n");
	printf("NOTE: unexpected categor%s encountered in the data\n",
	    unexpected_cats == 1 ? "y" : "ies");
	printf("      run SUPPORT to correct the problem\n\n");
    }
    sprintf(command,"lpr %s",tmpname);
    do
    {
	fprintf(stderr,"Do you wish to print this report out? (y/n) [n] ");
    }while (!G_gets(ans));
    if(ans[0] == 'y' || ans[0] == 'Y')
	system(command);
    while (1)
    {
	fprintf(stderr,"Do you wish to save this report in a file? (y/n) [n] ");
	if(!G_gets(ans))
	    continue;
	if(ans[0] == 'y' || ans[0] == 'Y')
	{
	    fprintf(stderr,"Enter the file name --> ");
	    if (!G_gets(line))
		continue;
	    if(sscanf(line,"%s",name) != 1)
		continue;
	    G_strip (name);
	    if (*name == 0)
		continue;
	    if(name[0] != '/')
	    {
		sprintf(command,"cp %s %s/%s",tmpname,G_home(),name);
		fprintf(stderr,"'%s' being saved in your home directory",name);
	    }
	    else
	    {
		sprintf(command,"cp %s %s",tmpname,name);
		fprintf(stderr,"'%s' being saved",name);
	    }
	    system(command);
	    fprintf(stderr,"\n");
	}
	break;
    }
    unlink (tmpname);
}

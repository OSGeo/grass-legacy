#include "gis.h"

more_print(reportfile)
    char *reportfile;
{
    char ans[128],line[80],command[1024],name[128];

    sprintf(command,"clear; more -d %s",reportfile);
    system(command);
    sprintf(command,"lpr %s",reportfile);
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
		sprintf(command,"cp %s %s/%s",reportfile,G_home(),name);
		fprintf(stderr,"'%s' being saved in your home directory",name);
	    }
	    else
	    {
		sprintf(command,"cp %s %s",reportfile,name);
		fprintf(stderr,"'%s' being saved",name);
	    }
	    system(command);
	    fprintf(stderr,"\n");
	}
	break;
    }
}

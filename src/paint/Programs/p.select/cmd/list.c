#include "gis.h"
list_painters()
{
    char command[1024];
    char *drivers();

    sprintf (command, "ls %s\n", drivers(""));
    system (command);
}

char *
ls_painters()
{
    char command[1024];
    char *drivers();
    FILE *popen();
    FILE *fd;
    char *list;
    char name[256];
    int len;

    list = G_malloc(1);
    *list = 0;
    len = 0;
    sprintf (command, "ls %s\n", drivers(""));
    if(fd = popen (command, "r"))
    {
	while (fscanf (fd, "%s", name) == 1)
	{
	    if (len > 0) strcat(list,",");
	    len += strlen(name)+2;
	    list = G_realloc (list, len);
	    strcat (list,name);
	}
    }
    pclose (fd);
    return list;
}

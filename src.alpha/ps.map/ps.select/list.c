#include "gis.h"

list_painters()
{
    char command[1024];
    char *drivers();

    sprintf(command, "ls %s\n", ps_devices(""));
    system(command);
}

char *
ls_painters()
{
    char command[1024];
    char *drivers();
    FILE *popen();
    FILE *fp;
    char *list;
    char name[256];
    int len;

    list = NULL;
    len = 0;
    sprintf(command, "ls %s\n", ps_devices(""));
    if (fp = popen (command, "r"))
    {
	while (fscanf (fp, "%s", name) == 1)
	{
	    if (list) strcat(list, ",");
	    len += strlen(name) + 2;
	    list = G_realloc(list, len);
	    strcat(list, name);
	}
    }
    pclose(fp);
    return list;
}

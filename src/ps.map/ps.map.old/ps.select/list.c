#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "local_proto.h"

int list_painters (void)
{
    char command[1024];

    sprintf(command, "ls %s\n", ps_devices(""));
    system(command);

    return 0;
}

char *ls_painters (void)
{
    char command[1024];
    FILE *fp;
    char *list;
    char name[256];
    int len;
    int first = 1; /* added by jhickey@hpcc.nectec.or.th */

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
	    
	    /* added by jhickey@hpcc.nectec.or.th */
	    /* Since list will point somewhere in memory the first time, we
		have to make sure it is an empty string. */
	    if (first)
	    {
		list[0] = '\0';
		first = 0;
	    }
	    /* end of added code */
	     
	    strcat(list, name);
	}
    }
    pclose(fp);
    return list;
}

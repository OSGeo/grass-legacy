#include <string.h>
#include "imagery.h"

int I_list_group (
    char *group,
    struct Ref *ref,
    FILE *fd)
{
    char buf[80];
    int i;
    int len, tot_len;
    int max;

    if (ref->nfiles <= 0)
    {
	fprintf (fd, "group [%s] is empty\n", group);
	return 0;
    }
    max = 0;
    for (i=0; i < ref->nfiles; i++)
    {
	sprintf (buf, "%s in %s", ref->file[i].name, ref->file[i].mapset);
	len = strlen(buf)+4;
	if (len > max) max = len;
    }
    fprintf (fd, "group [%s] references the following cellfiles\n", group);
    fprintf (fd, "-------------\n");
    tot_len = 0;
    for (i=0; i < ref->nfiles; i++)
    {
	sprintf (buf, "%s in %s", ref->file[i].name, ref->file[i].mapset);
	tot_len += max;
	if (tot_len > 78)
	{
	    fprintf (fd, "\n");
	    tot_len = max;
	}
	fprintf (fd, "%-*s", max, buf);
    }
    if (tot_len)
	fprintf (fd, "\n");
    fprintf (fd, "-------------\n");

    return 0;
}

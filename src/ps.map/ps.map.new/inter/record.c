#include <string.h>
#include "gis.h"

extern char *recordfile;

static int len;

#define LEN 11

int begin_record (char *label)
{
    FILE *fd;

    len = 0;
    if((fd = fopen (recordfile, "a"))==NULL)
	return 1;
    fprintf (fd, "%-*s", LEN, label);
    fclose (fd);

    return 0;
}

int add_record (char *name)
{
    FILE *fd;
    int n;

    if((fd = fopen (recordfile, "a"))==NULL)
	return 1;
    n = strlen (name);
    if (len+n+3 > (75-LEN))
    {
	fprintf (fd,"\n%*s",LEN,"");
	len = 0;
    }
    if (len > 0)
    {
	fprintf (fd, ",");
	len++;
    }
    fprintf (fd, " %s",name);
    fclose (fd);
    len += n+1;

    return 0;
}

int end_record (void)
{
    FILE *fd;

    len = 0;
    if((fd = fopen (recordfile, "a"))==NULL)
	return 1;
    fprintf (fd, "\n");
    fclose (fd);

    return 0;
}

int print_record (void)
{
    FILE *fd;

    char buf[1024];
    if ((fd = fopen (recordfile,"r")) != NULL)
    {
	while (fgets(buf, sizeof buf, fd))
	    fprintf (stdout,"%s", buf);
	fclose (fd);
	fprintf (stdout,"\n");
    }

    return 0;
}

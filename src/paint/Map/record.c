#include "gis.h"

extern char *recordfile;

static int len;

#define LEN 11

begin_record (label)
    char *label;
{
    FILE *fd;

    len = 0;
    if((fd = fopen (recordfile, "a"))==NULL)
	return;
    fprintf (fd, "%-*s", LEN, label);
    fclose (fd);
}

add_record (name)
    char *name;
{
    FILE *fd;
    int n;

    if((fd = fopen (recordfile, "a"))==NULL)
	return;
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
}

end_record()
{
    FILE *fd;

    len = 0;
    if((fd = fopen (recordfile, "a"))==NULL)
	return;
    fprintf (fd, "\n");
    fclose (fd);
}

print_record()
{
    FILE *fd;

    char buf[1024];
    if ((fd = fopen (recordfile,"r")) != NULL)
    {
	while (fgets(buf, sizeof buf, fd))
	    printf ("%s", buf);
	fclose (fd);
	printf ("\n");
    }
}

#include "global.h"

int mail (char *mailfile)
{
    FILE *fd, *mail, *popen();
    char *G_program_name();
    char buf[1024];
    int any;


    mail = popen ("mail `whoami`","w");
    if (mail == NULL) return 1;

    fprintf (mail, "Subject: %s\n\n", G_program_name());
    any = 0;
    fd = fopen (mailfile, "r");
    if (fd != NULL)
    {
	while (fgets(buf, sizeof buf, fd))
	{
	    fprintf (mail, "%s", buf);
	    any = 1;
	}
	fclose (fd);
    }
    if (!any)
	fprintf (mail, "DONE\n");

    pclose (mail);

    return 0;
}

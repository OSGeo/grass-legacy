/*
 ****************************************************************
 * char *
 * G_whoami ()
 *
 *   returns char pointer to user name
 *   dies if can't determine
 *
 * char *
 * G__whoami()
 *
 *   returns char pointer to user name
 *   NULL if can't determine
 *
 ***************************************************************/
#include "gis.h"

char *
G_whoami ()
{
    char *G__whoami();
    char *name;

    if (name = G__whoami())
	return name;

    G_fatal_error ("unable to determine user's name");
    exit(-1);
}

char *
G__whoami ()
{
    static char *name = 0;
    char buf[1024];
    FILE *fd,*popen();

/* first call must get name
 * execute the command "whoami and read the
 * output to get the home directory
 */
    if (!name)
    {
	if(fd = popen ("whoami","r"))
	{
	    if (fscanf (fd,"%s", buf) == 1)
		name = G_store (buf);
	    pclose (fd);
	}
    }

    return name;
}

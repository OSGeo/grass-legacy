#define GLOBAL
#include "P.h"
Pinit()
{
    sprintf (cyanfile, "/tmp/Pc.%d", getpid());
    sprintf (magentafile, "/tmp/Pm.%d", getpid());
    sprintf (yellowfile, "/tmp/Py.%d", getpid());
    umask(0);
    close (creat(cyanfile,0600));
    close (creat(magentafile,0600));
    close (creat(yellowfile,0600));
    cyanfd = open (cyanfile, 2);	/* read and write */
    if (cyanfd < 0)
	cantopen (cyanfile);
    magentafd = open (magentafile, 2);
    if (magentafd < 0)
	cantopen (magentafile);

    yellowfd = open (yellowfile, 2);
    if (yellowfd < 0)
	cantopen (yellowfile);

    ras_row = 0;
    ras_nrows = 3320/2;
}

cantopen(name) char *name;
{
    char msg[100];

    sprintf (msg, "%s - can't create", name);
    error (msg);
}

#include <stdio.h>

Pmap (scriptfile, background)
    char *scriptfile;
{
    char *getenv();

    freopen (scriptfile, "r", stdin);
    if (background && (G_fork() > 0))
	exit(0);
    if (getenv ("PMAP_TRACE"))
	execlp ("Pmap", "Pmap","trace",0);
    else
	execlp ("Pmap", "Pmap",0);
    G_fatal_error ("unable to exec Pmap");
}

#include "interface.h"
#include <stdio.h>
Pconnect()
{
    char *Ppainter_name();
    char *G_gisbase();
    char *painter;
    char pgm[1024];
    char *argv[3];
    char transparent;

    painter = Ppainter_name();
    if (painter == NULL)
    {
	fprintf (stderr, "no PAINTER selected\n");
	exit(1);
    }
    sprintf (pgm,"%s/etc/paint/driver.shell", G_gisbase());

    argv[0] = "driver.shell";
    argv[1] = painter;
    argv[2] = NULL;

    P__opendev (pgm, argv, painter);
    P__readdev (&transparent, 1);
    P__transparent (transparent);
}
Pdisconnect()
{
    P__closedev();
}

#include "gis.h"
#include <stdlib.h>

int menu (void)
{
    char buf[40];
AGAIN:
    G_clear_screen () ;

    fprintf (stdout,"ICONS    display/paint icon generator\n\n\n");

    fprintf (stdout,"Please select one of the following\n\n");
    fprintf (stdout,"   1    preview an existing icon\n");
    fprintf (stdout,"   2    create/edit an icon\n\n");
    fprintf (stdout," RETURN exit\n\n");

    while (1)
    {
	fprintf (stdout,"> ");
	if (!G_gets(buf)) goto AGAIN;
	G_strip(buf);
	switch (atoi(buf))
	{
	case 1: return 1;
	case 2: return 2;
	default: if (*buf == 0) return 0;
	}
    }
}


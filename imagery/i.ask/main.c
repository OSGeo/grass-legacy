#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "raster.h"
#include "gis.h"
#include "glocale.h"
#include "local_proto.h"

int TOP, BOTTOM, LEFT, RIGHT;

int main (int argc, char *argv[])
{
    char msg[200];
    double fx, fy;
    int x, y;
    FILE *fd;

    if (argc < 2 || (argc > 3 && argc != 5))
	usage (argv[0]);
    fd = fopen (argv[1], "r");
    if (fd == NULL)
    {
	perror (argv[1]);
	G_message(_("usage: %s file [prompt %%x %%y]\n"), argv[0]);
	exit(0);
    }

    strcpy (msg, "Double click on the");
    if (argc > 2 && argv[2][0] != '-')
    {
	if (argv[2][0] != ' ')
	    strcat (msg, " ");
	strcat (msg, argv[2]);
    }
    else
	strcat (msg, " file to be selected");

    fx = 50.0;
    fy = 50.0;
    if (argc > 3)
    {
	if (sscanf (argv[3], "%lf", &fx) != 1 || fx < 0.0 || fx > 100.0)
	    usage (argv[0]);
	if (sscanf (argv[4], "%lf", &fy) != 1 || fy < 0.0 || fy > 100.0)
	    usage (argv[0]);
    }
    R_open_driver();
    TOP    = R_screen_top();
    BOTTOM = R_screen_bot();
    LEFT   = R_screen_left();
    RIGHT  = R_screen_rite();

    fx /= 100.0;
    fy /= 100.0;
    x = (1.0-fx)*LEFT + fx*RIGHT;
    y = fy*TOP + (1.0-fy) * BOTTOM;

    x = popup (fd, x, y, msg);
    R_close_driver();
    exit(x);
}

int 
usage (char *me)
{
    G_message(_("usage: %s file [prompt %%x %%y]\n"), me);
    exit(1);
}

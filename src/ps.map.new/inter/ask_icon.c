#include <stdio.h>
#include "gis.h"
#include "ps_map.h"

int ask_for_icon (char *name, FILE *fd)
{
    char prompt[80];
    char *mapset;
    char icon_name[40];

    sprintf(prompt, "please select an icon for <<%s>>", name);
    G_set_ask_return_msg("to select DEFAULT icon");
    if (mapset = G_ask_old(prompt, icon_name, "ps_icons", "icon"))
	fprintf(fd, "  icon %s\n", icon_name);

    fprintf(fd, "  size %f\n", ask_icon_size());

    return 0;
}

float ask_icon_size (void)
{
    char buf[100];
    char dummy[2];
    float scale;

    while(1)
    {
	fprintf (stdout,"enter icon size (default 1.0): ");
	input(buf);
	if (sscanf(buf, "%1s", dummy) != 1) return 1.0;

	scale = 0.0;
	if (sscanf(buf,"%f%1s", &scale, dummy) == 1 && scale > 0.0)
	    return scale;
	fprintf (stdout,"\nsize should be entered as a positive number\n\n");
    }
}

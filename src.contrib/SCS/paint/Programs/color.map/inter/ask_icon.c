#include <stdio.h>
#include "icon.h"
ask_for_icon (name, fd)
    char *name;
    FILE *fd;
{
    char prompt[80];
    char *mapset;
    char icon_name[40];
    float ask_icon_size();

    sprintf (prompt, "please select an icon for <<%s>>", name);
    G_set_ask_return_msg ("to select DEFAULT icon");
    if (mapset = ask_icon_old (prompt, icon_name))
	fprintf (fd, "  icon %s\n", G_fully_qualified_name(icon_name, mapset));

    fprintf (fd, "  size %f\n", ask_icon_size());
}

float
ask_icon_size ()
{
    char buf[100];
    char dummy[2];
    float scale;

    while(1)
    {
	printf("enter icon size (default 1.0): ");
	input (buf);
	if (sscanf (buf, "%1s", dummy) != 1)
	    return 1.0;

	scale = 0.0;
	if (sscanf(buf,"%f%1s", &scale, dummy) == 1 && scale > 0.0)
	    return scale;
	printf("\nsize should be entered as a positive number\n\n");
    }
}

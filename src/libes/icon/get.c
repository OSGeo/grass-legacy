#include "icon.h"
#include "gis.h"

get_icon (name, mapset, icon)
    char *name;
    char *mapset;
    ICON *icon;
{
    FILE *fd;
    char msg[100];

    if (fd = G_fopen_old ("icons", name, mapset))
    {
	read_icon (fd, icon);
	fclose (fd);
	return 1;
    }

    sprintf (msg, "can't read icon file [%s] in [%s]", name, mapset);
    G_warning (msg);

    return -1;
}

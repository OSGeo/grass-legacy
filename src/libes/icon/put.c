#include "icon.h"
#include "gis.h"

int put_icon (
    char *name,
    ICON *icon)
{
    FILE *fd;
    char msg[100];

    if (fd = G_fopen_new ("icons", name))
    {
	write_icon (fd, icon, 0);
	fclose (fd);
	return 1;
    }

    sprintf (msg, "can't write icon file [%s]", name);
    G_warning (msg);

    return -1;
}

#include "gis.h"
#include "icon.h"

preview_icons()
{
    char name[20];
    char *mapset;
    ICON icon;
    char buf[80];

    while (mapset = ask_icon_old ("select icon to be previewed", name))
    {
	if (get_icon (name, mapset, &icon) >= 0)
	{
	    G_clear_screen();

	    printf ("ICON [%s] in [%s]\n", name, mapset);

	    divider (icon.ncols);
	    write_icon (stdout, &icon, 0);
	    divider (icon.ncols);

	    release_icon (&icon);
	}

	printf ("hit RETURN to continue -->");
	G_gets(buf);
    }
}

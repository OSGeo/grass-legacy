#include "gis.h"
#include "icon.h"
#include "local_proto.h"

int 
preview_icons (void)
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

	    fprintf (stdout,"ICON [%s] in [%s]\n", name, mapset);

	    divider (icon.ncols);
	    write_icon (stdout, &icon, 0);
	    divider (icon.ncols);

	    release_icon (&icon);
	}

	fprintf (stdout,"hit RETURN to continue -->");
	G_gets(buf);
    }

  return 0;
}

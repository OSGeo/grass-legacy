/* %W% %G% */
#include "gis.h"

main()
{
    G_gisinit ("ICONS");

    while (1)
    {
	switch (menu())
	{
	case 1: preview_icons(); break;
	case 2: edit_icons(); break;
	default: exit(0);
	}
    }
}

#include "gis.h"
#include "icon.h"
#include "parms.h"

do_plfile (after_masking)
{
    FILE *fd;
    char buf[1024];
    char name[50], mapset[50];
    ICON icon1, icon2;
    double e1,n1,e2,n2;
    int color;
    int masked;
    float size;
    int i;
    int x,y;
    int width;
    struct Cell_head window;

    if (parms.plfile == NULL)
	return;
    fd = fopen (parms.plfile,"r");
    if (fd == NULL)
    {
	error("point/line file","","can't open");
	return;
    }

    G_get_window (&window);

    while (fgets(buf, sizeof buf, fd))
    switch (*buf)
    {
    case 'L':
	if(sscanf (buf,"L %d %lf %lf %lf %lf %d %d",
	    &masked, &e1, &n1, &e2, &n2, &color, &width) == 7)
	{
	    if (masked && after_masking) continue;
	    if (!masked && !after_masking) continue;
	    set_color (color);
	    set_width (width);
	    G_plot_line (e1, n1, e2, n2);
	}
	break;

    case 'P':
	i = sscanf (buf,"P %d %lf %lf %d %f %s %s",
	    &masked, &e1, &n1, &color, &size, name, mapset);
	if (i == 5 || i == 7)
	{
	    if (masked && after_masking) continue;
	    if (!masked && !after_masking) continue;

	    if (n1 > window.north || n1 < window.south) continue;
	    if (e1 > window.east  || e1 < window.west ) continue;
	    set_color (color);

	    G_plot_where_xy (e1, n1, &x, &y);

	    if (i == 5 || get_icon (name, mapset, &icon1) < 0)
		get_default_icon (&icon1);
	    if (size <= 0.0) size = 1.0;
	    scale_icon (&icon1, &icon2, size);
	    draw_icon (&icon2, x, y);
	    release_icon (&icon1);
	    release_icon (&icon2);
	}
	break;
    }

    fclose (fd);
}

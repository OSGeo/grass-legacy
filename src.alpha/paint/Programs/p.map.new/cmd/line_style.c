#include "gis.h"
#include "graphics.h"

set_line_style_solid()
{
    graphics.linestyle.len = 0;
}

set_line_style (text, colors)
    char *text;
    int colors[9];
{
    int len;
    int width;
    int i,n;
    int x;
    int *table;

/*
printf ("linestyle(%s) colors (", text);
for (i=0; i < 9; i++) printf (" %d", colors[i]);
printf (")\n");
*/
/*
 * the linestyle is a lookup table of colors, indexed via a modular index
 * it is assumed that the linestyle is to be scaled based on the current
 * line width.
 */
    len = strlen (text);
    width = graphics.width1 + graphics.width2 + 1;

    graphics.linestyle.cur = 0;
    graphics.linestyle.prevx = -1000;
    graphics.linestyle.prevy = -1000;
    graphics.linestyle.len = len * width;
    table = graphics.linestyle.table =
	(int *) G_realloc (graphics.linestyle.table, sizeof(int) * graphics.linestyle.len);

    for (i = 0; i < len; i++)
    {
	if (*text >= '1' && *text <= '9')
	    x = *text - '1';
	else
	    x = -1;
	text++;
	for (n = 0; n < width; n++)
	    *table++ = x;
    }
    for (i = 0; i < 9; i++)
	graphics.linestyle.colors[i] = colors[i];
}

regress_line_style()
{
    if (graphics.linestyle.len > 0)
    {
	graphics.linestyle.cur--;
	if(graphics.linestyle.cur < 0)
	    graphics.linestyle.cur = 0;
    }
}


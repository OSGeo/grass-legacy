#include <string.h>
#include "gis.h"
#include "graphics.h"

int 
set_line_style_solid (void)
{
    graphics.linestyle.len = 0;

    return 0;
}

int 
set_line_style (char *text, int colors[9])
{
    int len;
    int width;
    int i,n;
    int x;
    int *table;

/*
fprintf (stdout,"linestyle(%s) colors (", text);
for (i=0; i < 9; i++) fprintf (stdout," %d", colors[i]);
fprintf (stdout,")\n");
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

    return 0;
}

int regress_line_style (void)
{
    if (graphics.linestyle.len > 0)
    {
	graphics.linestyle.cur--;
	if(graphics.linestyle.cur < 0)
	    graphics.linestyle.cur = 0;
    }

    return 0;
}


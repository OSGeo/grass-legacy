#include <stdio.h>
#include "graphics.h"
#include "local_proto.h"

int graph_point (int x, int y)
{
    register int i,j;
    int n;

    if (graphics.linestyle.len > 0)
    {
	if (x == graphics.linestyle.prevx && y == graphics.linestyle.prevy)
	    return 0;

	graphics.linestyle.prevx = x;
	graphics.linestyle.prevy = y;
	n = graphics.linestyle.table[graphics.linestyle.cur++];
	if (graphics.linestyle.cur >= graphics.linestyle.len)
	    graphics.linestyle.cur = 0;
	if (n < 0)
	    return 0;

	set_color (graphics.linestyle.colors[n]);
    }
    for (i = -graphics.width1; i <= graphics.width2; i++)
	for (j = -graphics.width1; j <= graphics.width2; j++)
	    dot (x+i, y+j);

    return 0;
}

#include "P.h"
Ptext (s) char *s;
{
    char *t;
    int i;
    int x,y;
    char z;

/* one white line */
    z = 124;
    Pdata (&z, 1);

/* initialize textbuf to WHITE */
    for (x = 0; x < TEXT_COLS; x++)
	for (y = 0; y < TEXT_ROWS; y++)
	    textbuf[y][x] = 124;

/* draw text into textbuf */
    x = 0;
    y = TEXT_MIDDLE;
    graph_text (&x, &y, TEXT_SIZE, 0.0, s);

    for (i = 0; i < TEXT_ROWS; i++)
	Pdata (textbuf[i], TEXT_COLS);

/* another white line */
    z = 124;
    Pdata (&z, 1);
}

#include "P.h"
Ptext (s) char *s;
{
    char *t;
    int i,n;
    int x,y;
    unsigned char white;

    white = WHITE;
    if (ncols <= 0) return;

/* one white line */
    Pdata (&white, 1);

/* initialize textbuf to white */
    for (x = 0; x < TEXT_COLS; x++)
	for (y = 0; y < TEXT_ROWS; y++)
	    textbuf[y][x] = white;

/* draw text into textbuf */
    /*
    x = 10;
    */
    x = 0;
    y = TEXT_MIDDLE;
    graph_text (&x, &y, TEXT_SIZE, 0.0, s);

    for (i = 0; i < TEXT_ROWS; i++)
	Pdata (textbuf[i], TEXT_COLS);

/* another white line */
    Pdata (&white, 1);
}

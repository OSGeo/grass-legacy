#include <stdio.h>
#include "dlg.h"
#include "vector.h"
#include "misc.h"

vector_info (pixels)
{
    unsigned char color,hcolor;
    unsigned char white, grey;
    int len;
    int i,ii;
    int n;
    int x;
    int count;
    int pad;
    char buf[200];

    len = 80;
    if (len > pixels) len = pixels;
    pad = pixels - len;

    white = WHITE;
    grey  = GREY;

    Palpha ();
    for (i=0; i < vector.count; i++)
    {
        Praster ();

/* white line */
	Prle_begin();
	if (vector.hwidth[i] > 0 && vector.hcolor[i] == white)
	    Prle (grey, len+pad);
	else
	    Prle (white, len+pad);
	Prle_end();

/* highlight above */
	if (ii = highlight_count(i))
	{
	    if(vector.hwidth[i])
		hcolor = vector.hcolor[i];
	    else
		hcolor = grey;

	    while (ii-- > 0)
	    {
		Prle_begin();
		Prle (hcolor, len);
		Prle (white, pad);
		Prle_end();
	    }
	}
	else
	    hcolor = white;

/* body of the line, with line style */
	count = vector.linestyle[i] ? strlen (vector.linestyle[i]) : 0;
	for (ii = 0; ii < vector.width[i]; ii++)
	{
	    Prle_begin();
	    if (count)
	    {
		for (n = 0; n < len; n++)
		{
		    x = vector.linestyle[i][(n/vector.width[i])%count] - '1';
		    if (x < 0)
			color = hcolor;
		    else
			color = vector.colors[i][x];
		    Prle (color, 1);
		}
	    }
	    else
	    {
		Prle (color = vector.colors[i][0], len);
	    }
	    Prle (white, pad);
	    Prle_end();
	}

/* highlight below */
	if (ii = highlight_count(i))
	{
	    if(vector.hwidth[i])
		hcolor = vector.hcolor[i];
	    else
		hcolor = grey;

	    while (ii-- > 0)
	    {
		Prle_begin();
		Prle (hcolor, len);
		Prle (white, pad);
		Prle_end();
	    }
	}

/* white line */
	Prle_begin();
	if (vector.hwidth[i] > 0 && vector.hcolor[i] == white)
	    Prle (grey, len+pad);
	else
	    Prle (white, len+pad);
	Prle_end();

        Palpha ();
        sprintf (buf, "%s (%s)", vector.name[i], vector.mapset[i]);
	Ptext (buf);
	Ptext ("");
    }
}

vector_info_lines(i)
{
    return (2+vector.width[i] + 2*highlight_count(i));
}

static
highlight_count(i)
{
    char *lp;

    if (vector.hwidth[i]) return vector.hwidth[i];
    if (vector.hcolor[i] != WHITE) return 0;
    if ((lp = vector.linestyle[i]) == NULL)
	return (vector.colors[i][0] == WHITE);

    for ( ; *lp; lp++)
    {
	if (*lp == '0') continue;
	if (vector.colors[i][*lp - '1'] == WHITE)
	    return 1;
    }
    return 0;
}

#include <stdio.h>
#include "dlg.h"
#include "vector.h"
#include "drawsitevect.h"
#include "misc.h"

dvect(vcount, vlen)
int vcount, vlen;
{
    unsigned char color,hcolor;
    unsigned char white, grey;
    int len;
    int i,ii;
    int n, w;
	int hw;
    int x, z;
    int count;


printf ("vcount is %d\n", vcount);

	for (i=0; i<vcount; i++) {
/* highlight above */

	hw = 0;
	if (ii = highlight_count(drwv[i].index))
	{
	    if(vector.hwidth[drwv[i].index])
		hcolor = vector.hcolor[drwv[i].index];
	    else
		hcolor = grey;

		set_color (hcolor);

		hw = vector.hwidth[drwv[i].index];

		for (w=0; w<hw; w++) 
			draw_line(drwv[i].x, drwv[i].y+w, drwv[i].x+vlen, drwv[i].y+w); 
	
	}


	count = vector.linestyle[drwv[i].index] ? strlen
	(vector.linestyle[drwv[i].index]) : 0;

printf (" hw is %d\n", hw);
printf ( " vector.width is %d\n", vector.width[drwv[i].index]);


	for (ii=0; ii < vector.width[drwv[i].index]; ii++)
	{
	if (count)
	{ int color;

	for (n=0; n<vlen+1; n++)
	{
	z = vector.linestyle[drwv[i].index][(n/vector.width[drwv[i].index])%count]
		- '1';


	if (z <0)
		color = hcolor;
	else
		color = vector.colors[drwv[i].index][z];


	set_color(color);
	dot(drwv[i].x+n, drwv[i].y+ii+hw);

	}
	}
	else {
		color = vector.colors[drwv[i].index][0];
		for (n=0; n<vlen+1; n++)
		{
		set_color(color);
		dot(drwv[i].x+n, drwv[i].y+hw+ii);
		}

	}

	}

/* highlight below */
	if (ii = highlight_count(drwv[i].index))
	{
	    if(vector.hwidth[drwv[i].index]) 
		hcolor = vector.hcolor[drwv[i].index];
	    else
		hcolor = grey;

		set_color (hcolor);

		hw = vector.hwidth[drwv[i].index];

		for (w=0; w<hw; w++) 
			draw_line(drwv[i].x, drwv[i].y+w+hw+vector.width[drwv[i].index]+1, drwv[i].x+vlen, drwv[i].y+w+hw+vector.width[drwv[i].index]+1) ; 

	    }
	}


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



#include "P.h"
erase()
{
    static int first = 1;
    static int color;
    char *getenv(), *background;

    if (first)
    {
	if ((background = getenv ("PREVIEW_BACKGROUND")) == NULL)
	    background = "grey";
	color = D_translate_color(background) ;
	first = 0;
	current_row = 0;
    }
    if (current_row != 0)
    {
    /* use mouse to ask for continuation */
	if (!ask())
	{
	    Pclose();
	    exit(1);
	}
    }

    R_standard_color(color) ;
    R_box_abs (left, top, right, bottom);
    current_row = 0;
    top_edge = top + 1;
    left_edge = left + 1;
}
static
ask()
{
    char *text[5];
    text[0] = "PAINT PREVIEW";
    text[1] = "continue plot";
    text[2] = "hide this for 5 seconds";
    text[3] = "abort plot";
    text[4] = NULL;

    while(1)
    {
	switch (D_popup(
		    D_translate_color("red"),
		    D_translate_color("white"),
		    D_translate_color("white"),
		    0,0,3,text))
	{
	case 1: return 1;
	case 3: return 0;
	}
	sleep(5);
    }
}

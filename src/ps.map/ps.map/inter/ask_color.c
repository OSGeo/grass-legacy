#include <stdio.h>
#include "ps_map.h"
/*
extern int ncolors;
*/
int ask_color (char *name, FILE *fd)
{
    int n;
    char color[100];
    char *G_color_name();

    while(1)
    {
        fprintf (stdout,"select a color for <<%s>>: ",name);
	input (color);
	if (scan_color (color /*,ncolors*/))
	    break;

        fprintf (stdout,"\nyou may select the color by entering the\n");
        fprintf (stdout,"  color name\n(%s", G_color_name(0));
        for (n = 1; G_color_name(n) != NULL; n++)
	    fprintf (stdout," %s",G_color_name(n));
        fprintf (stdout,")\n");
/*
        fprintf (stdout,"  rgb intensities\n(eg: .5, .4, .7)\n");
        fprintf (stdout,"  printer color number\n(from 0-%d)\n\n", ncolors-1);
*/
    }

    fprintf (fd, "  color %s\n", color);

    return 0;
}

int ask_acolor (char *name, FILE *fd)
{
    char color[100];
    char *prompt;

    prompt = "do you want to paint filled areas";
    if (yes(prompt))    
    {
	while(1)
	{
    	    fprintf (stdout,"select area color for <<%s>>: ",name);
	    input (color);
	    if (scan_acolor (color))
		break;
    	    fprintf (stdout,"\nyou may select the color by entering the\n");
	    fprintf (stdout,"  rgb intensities\n(eg: 50 255 0)\n");
	}

	fprintf (fd, "  acolor %s\n", color);
    }
    return 0;
}

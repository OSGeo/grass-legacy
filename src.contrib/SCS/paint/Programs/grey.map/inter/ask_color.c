#include <stdio.h>
extern int ncolors;
ask_color (name,fd)
    char *name;
    FILE *fd;
{
    int n;
    char color[100];
    char *G_color_name();

    while(1)
    {
        printf("select a color for <<%s>>: ",name);
	input (color);
	if (scan_color (color,ncolors))
	    break;

        printf("\nyou may select the color by entering the\n");
        printf("  color name\n(%s", G_color_name(0));
        for (n = 1; G_color_name(n) != NULL; n++)
	    printf(" %s",G_color_name(n));
        printf(")\n");
        printf("  rgb intensities\n(eg: .5, .4, .7)\n");
        printf("  printer color number\n(from 0-%d)\n\n", ncolors-1);
    }

    fprintf (fd, "  color %s\n", color);
}

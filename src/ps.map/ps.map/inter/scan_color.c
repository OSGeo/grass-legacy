#include "gis.h"

int scan_color (char *color)
{
    float r,g,b;

    if (G_color_values (color, &r, &g, &b) > 0)
	return 1;

/*
    r = g = b = -1;
    if (sscanf(color,"%f %*[,] %f %*[,] %f %1s",&r,&g,&b,dummy) == 3
    && r >= 0 && r <= 1 && g >= 0 && g <= 1 && b >= 0 && b <= 1)
	return 1;

    n = -1;
    if (sscanf(color,"%d%1s", &n, dummy) == 1 && n >= 0 && n < ncolors)
	return 1;
*/

    return 0;
}

int scan_acolor (char *color)
{
    int r,g,b;

    r = g = b = -1;
    if (sscanf(color,"%d %d %d",&r,&g,&b) == 3
    && r >= 0 && r <= 255 && g >= 0 && g <= 255 && b >= 0 && b <= 255)
	return 1;

    return 0;
}
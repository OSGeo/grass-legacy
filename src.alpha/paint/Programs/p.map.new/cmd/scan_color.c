#include "misc.h"
scan_color (buf, n, r, g, b)
    char *buf;
    int *n;
    int *r,*g,*b;
{
    float fr,fg,fb;

    char name[100];

    if (sscanf (buf, "%s", name) != 1)
	return 0;
    if (G_color_values (name, &fr, &fg, &fb) > 0)
    {
	*n = Pcolornum (fr,fg,fb);
	*r = fr * 256.0;
	*g = fg * 256.0;
	*b = fb * 256.0;
	if (*r > 255) *r = 255;
	if (*g > 255) *g = 255;
	if (*b > 255) *b = 255;
	return 1;
    }

    fr = fg = fb = -1;
    if (sscanf(buf,"%f %f %f %s",&fr,&fg,&fb,name) == 3
    && fr >= 0 && fr <= 1 && fg >= 0 && fg <= 1 && fb >= 0 && fb <= 1)
    {
	*n = Pcolornum (fr,fg,fb);
	*r = fr * 256.0;
	*g = fg * 256.0;
	*b = fb * 256.0;
	if (*r > 255) *r = 255;
	if (*g > 255) *g = 255;
	if (*b > 255) *b = 255;
	return 1;
    }

    *n = -1;
    if (sscanf(buf,"%d%s", n, name) == 1
    && *n >= 0 && *n < ncolors)
    {
	Pcolorvalue (*n, &fr, &fg, &fb);
	*r = fr * 256.0;
	*g = fg * 256.0;
	*b = fb * 256.0;
	if (*r > 255) *r = 255;
	if (*g > 255) *g = 255;
	if (*b > 255) *b = 255;
	return 1;
    }

    *n = -1;
    return 0;
}

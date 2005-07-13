#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "includes.h"
#include "colors.h"
#include "XDRIVER.h"

static int table_type = FIXED;

static int Red[256], Grn[256], Blu[256];
static int Gray[256];

static int r_pos, g_pos, b_pos;
static int r_size, g_size, b_size;
static int r_scale, g_scale, b_scale;

int
reset_color(int number, int red, int grn, int blu)
{
    XColor xcolor;

    if (table_type != FLOAT)
    {
        G_warning("reset_color: called in FIXED color mode\n");
        return 1;
    }

    /* out-of-range check */
    if (number >= NCOLORS || number < 0)
    {
        G_warning("reset_color: can't set color %d\n", number);
        return 1;
    }

    xcolor.pixel  = (unsigned long) number;
    xcolor.red    = (unsigned short) (red * 0xFFFF / 0xFF);
    xcolor.green  = (unsigned short) (grn * 0xFFFF / 0xFF);
    xcolor.blue   = (unsigned short) (blu * 0xFFFF / 0xFF);
    xcolor.flags  = DoRed | DoGreen | DoBlue;

    XStoreColor(dpy, floatcmap, &xcolor);

    return 0;
}

int
Color_table_float(void)
{
    if (!can_do_float())
    {
	G_warning("Color_table_float: not available on this device\n");
	return -1;
    }

    table_type = FLOAT;

    XSetWindowColormap(dpy, grwin, floatcmap);

    Color_offset(0);

    reset_color(RED, 255, 0, 0);
    reset_color(ORANGE, 255, 127, 0);
    reset_color(YELLOW, 255, 255, 0);
    reset_color(GREEN, 0, 255, 0);
    reset_color(BLUE, 0, 0, 255);
    reset_color(INDIGO, 0, 127, 255);
    reset_color(VIOLET, 255, 0, 255);
    reset_color(WHITE, 255, 255, 255);
    reset_color(BLACK, 0, 0, 0);
    reset_color(GRAY, 127, 127, 127);
    reset_color(BROWN, 180, 75, 25);
    reset_color(MAGENTA, 255, 0, 127);
    reset_color(AQUA, 100, 127, 255);

    return 0;
}

static void
get_shifts(unsigned mask, int *pos, int *size, int *scale)
{
    int i, j;

    for (i = 0; (mask & 1) == 0; i++)
	mask >>= 1;
    if (pos)
	*pos = i;

    for (j = i; (mask & 1) != 0; j++)
	mask >>= 1;
    if (size)
	*size = j - i;
    if (scale)
	*scale = 8 - (j - i);
}

static int
get_rgb_shifts(void)
{
    get_shifts(use_visual->red_mask,   &r_pos, &r_size, &r_scale);
    get_shifts(use_visual->green_mask, &g_pos, &g_size, &g_scale);
    get_shifts(use_visual->blue_mask,  &b_pos, &b_size, &b_scale);

    return (1 << r_size) * (1 << g_size) * (1 << b_size);
}

static unsigned long
find_color_gray(unsigned int r, unsigned int g, unsigned int b)
{
    unsigned int y = (r + g + b) / 3;

    return xpixels[Gray[y]];
}

static unsigned long
find_color_indexed(unsigned int r, unsigned int g, unsigned int b)
{
    return xpixels[Red[r] + Grn[g] + Blu[b]];
}

static unsigned long
find_color_rgb(unsigned int r, unsigned int g, unsigned int b)
{
    unsigned int rr = r >> r_scale;
    unsigned int gg = g >> g_scale;
    unsigned int bb = b >> b_scale;

    return (rr << r_pos) + (gg << g_pos) + (bb << b_pos);
}

unsigned long
find_color(unsigned int r, unsigned int g, unsigned int b)
{

    switch (use_visual->class)
    {
    case StaticGray:
    case GrayScale:
	return find_color_gray(r, g, b);
    case StaticColor:
    case PseudoColor:
	return find_color_indexed(r, g, b);
    case TrueColor:
    case DirectColor:
	return find_color_rgb(r, g, b);
    default:
	G_fatal_error("Unknown visual class %d\n", use_visual->class);
	return 0;
    }
}

static void
get_max_levels(int n_colors, int *rr, int *gg, int *bb)
{
    int r, g, b, i;

    for (i = 0; i * i * i < n_colors; i++)
	;

    for (r = g = b = i; ; )
    {
	if (r * g * b <= n_colors) break;
	b--;
	if (r * g * b <= n_colors) break;
	r--;
	if (r * g * b <= n_colors) break;
	g--;
    }

    *rr = r;
    *gg = g;
    *bb = b;
}

static int
get_fewer_levels(int *rr, int *gg, int *bb)
{
    int r = *rr;
    int g = *gg;
    int b = *bb;

    /* 888 -> 887 -> 787 -> 777 -> ... */

    if (r > b)		/* 887 -> 787 */
	r--;
    else
	if (g > b)	/* 787 -> 777 */
	    g--;
	else		/* 888 -> 888 */
	    b--;

    *rr = r;
    *gg = g;
    *bb = b;

    return r >= 2 && g >= 2 && b >= 2;
}

static int
try_get_colors(Colormap cmap, int nr, int ng, int nb)
{
    XColor xcolor;
    int n_pixels;
    int r, g, b;

    xpixels = (unsigned long *) G_realloc(xpixels,
					  nr * ng * nb * sizeof(unsigned long));
    n_pixels = 0;

    xcolor.flags = DoRed | DoGreen | DoBlue;

    for (r = 0; r < nr; r++)
    {
	for (g = 0; g < ng; g++)
	{
	    for (b = 0; b < nb; b++)
	    {
		xcolor.red   = (unsigned short) (r * 0xFFFF / (nr - 1));
		xcolor.green = (unsigned short) (g * 0xFFFF / (ng - 1));
		xcolor.blue  = (unsigned short) (b * 0xFFFF / (nb - 1));
		if (!XAllocColor(dpy, cmap, &xcolor))
		{
		    XFreeColors(dpy, cmap, xpixels, n_pixels, 0);
		    return 0;
		}

		xpixels[n_pixels++] = xcolor.pixel;
	    }
	}
    }

    return 1;
}

static int
try_get_grays(Colormap cmap, int ny)
{
    XColor xcolor;
    int n_pixels;
    int y;

    xpixels = (unsigned long *) G_realloc(xpixels, ny * sizeof(unsigned long));
    n_pixels = 0;

    xcolor.flags = DoRed | DoGreen | DoBlue;

    for (y = 0; y < ny; y++)
    {
	unsigned short v = (unsigned short) (y * 0xFFFF / (ny - 1));
	xcolor.red   = v;
	xcolor.green = v;
	xcolor.blue  = v;
	if (!XAllocColor(dpy, cmap, &xcolor))
	{
	    XFreeColors(dpy, cmap, xpixels, n_pixels, 0);
	    return y;
	}

	xpixels[n_pixels++] = xcolor.pixel;
    }

    return ny;
}

static Colormap
ramp_colormap(void)
{
    int n_colors = use_visual->map_entries;
    Colormap cmap = XCreateColormap(dpy, RootWindow(dpy, scrn),
				    use_visual, AllocAll);
    int i;

    for (i = 0; i < n_colors; i++)
    {
	int k = i * 65535 / (n_colors - 1);
	int l = i * 255 / (n_colors - 1);
	XColor xcolor;

	xcolor.flags = DoRed | DoGreen | DoBlue;
	xcolor.blue  = k;
	xcolor.green = k;
	xcolor.red   = k;
	xcolor.pixel = find_color_rgb(l, l, l);

	XStoreColor(dpy, cmap, &xcolor);
    }

    return cmap;
}

Colormap
InitColorTableFixed(Colormap cmap)
{
    int n_colors = use_visual->map_entries;
    int r, g, b, y, i;

    /* "truecolor" really indicates that we can't do "float" color mode */
    switch (use_visual->class)
    {
    case GrayScale:
    case PseudoColor:
	truecolor = 0;
	break;
    case StaticGray:
    case StaticColor:
    case TrueColor:
    case DirectColor:
	truecolor = 1;
	break;
    }

    switch (use_visual->class)
    {
    case StaticGray:
    case GrayScale:
	/* determine how many levels of gray we can actually get */
	y = try_get_grays(cmap, n_colors);
	if (y > 2 && y < n_colors)
	    y = try_get_grays(cmap, y);
	if (y < 2)
	    G_fatal_error("Unable to get sufficient gray shades\n");

	NCOLORS = y;

        for (i = 0; i < 256; i++)
            Gray[i] = i * y / 256;

	break;

    case StaticColor:
    case PseudoColor:
	/* determine how many levels of r, g, and b are possible */
	get_max_levels(n_colors, &r, &g, &b);

	/* now see how many we can actually get */
	while (!try_get_colors(cmap, r, g, b))
	    if (!get_fewer_levels(&r, &g, &b))
		G_fatal_error("Unable to get sufficient colors\n");

	NCOLORS = r * g * b;

        for (i = 0; i < 256; i++)
	{
            Red[i] = (i * r / 256) * g * b;
            Grn[i] = (i * g / 256) * b;
            Blu[i] = (i * b / 256);
        }

	break;

    case DirectColor:
	G_warning("Using private colormap for DirectColor visual\n");

	/* free any previously-allocated Colormap */
	if (cmap != DefaultColormap(dpy, scrn))
	    XFreeColormap(dpy, cmap);

	/* get shift factors for R,G,B masks */
	NCOLORS = get_rgb_shifts();

	/* create colormap (emulates TrueColor visual) */
	cmap = ramp_colormap();
	break;

    case TrueColor:
	/* get shift factors for R,G,B masks */
	NCOLORS = get_rgb_shifts();
	break;

    default:
	G_fatal_error("Unknown visual class %d\n", use_visual->class);
	break;
    }

    /* Generate lookup for "standard" colors */
    assign_standard_color(RED,     _get_lookup_for_color(255,   0,   0));
    assign_standard_color(ORANGE,  _get_lookup_for_color(255, 128,   0));
    assign_standard_color(YELLOW,  _get_lookup_for_color(255, 255,   0));
    assign_standard_color(GREEN,   _get_lookup_for_color(  0, 255,   0));
    assign_standard_color(BLUE,    _get_lookup_for_color(  0,   0, 255));
    assign_standard_color(INDIGO,  _get_lookup_for_color(  0, 128, 255));
    assign_standard_color(VIOLET,  _get_lookup_for_color(255,   0, 255));
    assign_standard_color(BLACK,   _get_lookup_for_color(  0,   0,   0));
    assign_standard_color(WHITE,   _get_lookup_for_color(255, 255, 255));
    assign_standard_color(GRAY,    _get_lookup_for_color(175, 175, 175));
    assign_standard_color(BROWN,   _get_lookup_for_color(180,  77,  25));
    assign_standard_color(MAGENTA, _get_lookup_for_color(255,   0, 128));
    assign_standard_color(AQUA,    _get_lookup_for_color(100, 128, 255));

    return cmap;
}

int Color_table_fixed (void)
{
    table_type = FIXED;

    /* by cho; 2001/08/08 */
    /* Generate lookup for "fixed" colors */
    assign_fixed_color(RED,     _get_lookup_for_color(255,   0,   0));
    assign_fixed_color(ORANGE,  _get_lookup_for_color(255, 128,   0));
    assign_fixed_color(YELLOW,  _get_lookup_for_color(255, 255,   0));
    assign_fixed_color(GREEN,   _get_lookup_for_color(  0, 255,   0));
    assign_fixed_color(BLUE,    _get_lookup_for_color(  0,   0, 255));
    assign_fixed_color(INDIGO,  _get_lookup_for_color(  0, 128, 255));
    assign_fixed_color(VIOLET,  _get_lookup_for_color(255,   0, 255));
    assign_fixed_color(BLACK,   _get_lookup_for_color(  0,   0,   0));
    assign_fixed_color(WHITE,   _get_lookup_for_color(255, 255, 255));
    assign_fixed_color(GRAY,    _get_lookup_for_color(175, 175, 175));
    assign_fixed_color(BROWN,   _get_lookup_for_color(180,  77,  25));
    assign_fixed_color(MAGENTA, _get_lookup_for_color(255,   0, 128));
    assign_fixed_color(AQUA,    _get_lookup_for_color(100, 128, 255));
    /**/

    XSetWindowColormap(dpy, grwin, fixedcmap);
    return 0;
}

int _get_lookup_for_color(int r, int g, int b)
{
    switch (use_visual->class)
    {
    case StaticGray:
    case GrayScale:
	return Gray[(r + g + b) / 3];
	break;

    case StaticColor:
    case PseudoColor:
	return Red[r] + Grn[g] + Blu[b];
	break;

    case DirectColor:
    case TrueColor:
	return find_color_rgb(r, g, b);
	break;

    default:
	G_fatal_error("Unknown visual class %d\n", use_visual->class);
	break;
    }
}

int get_table_type(void)
{
    return table_type;
}

/*** end Clr_table.c ***/

     
#include <stdio.h>
#include <stdlib.h>
#include "gis.h"     
#include "driverlib.h"
#include "colors.h"
#include "png.h"

static int table_type = FIXED;
static int Red[256], Grn[256], Blu[256];

#ifndef gdTrueColor
#define gdTrueColor(r,g,b) 0
#endif

int
reset_color(int number, int red, int grn, int blu)
{
    int ret;

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

    gdImageColorDeallocate(im, number);
    ret = gdImageColorAllocate(im, red, grn, blu);

    if (ret == -1)
    {
        G_warning("reset_color: can't set color %d\n", number);
        return 1;
    }

    if (ret != number)
    {
        G_warning("reset_color: mismatch: color %d changed to %d\n", number, ret);
        return 1;
    }

    return(0);
}

static unsigned long
find_color_indexed(unsigned int r, unsigned int g, unsigned int b)
{
    return xpixels[Red[r] + Grn[g] + Blu[b]];
}

static unsigned long
find_color_rgb(unsigned int r, unsigned int g, unsigned int b)
{
    return gdTrueColor(r, g, b);
}

unsigned long
find_color(unsigned int r, unsigned int g, unsigned int b)
{
    return true_color
	? find_color_rgb(r, g, b)
	: find_color_indexed(r, g, b);
}

static void
init_colors_rgb(void)
{
    NCOLORS = 1<<24;
}

static void
init_colors_indexed(void)
{
    int n_pixels;
    int r, g, b;
    int i;

    NCOLORS = gdMaxColors;

    xpixels = (unsigned long *) G_realloc(xpixels,
					  NCOLORS * sizeof(unsigned long));
    n_pixels = 0;

    for (r = 0; r < 6; r++)
    {
	for (g = 0; g < 6; g++)
	{
	    for (b = 0; b < 6; b++)
	    {
		int red = r * 0xFF / 5;
		int grn = g * 0xFF / 5;
		int blu = b * 0xFF / 5;
		xpixels[n_pixels++] = gdImageColorAllocate(im, red, grn, blu);
	    }
	}
    }

    while (n_pixels < NCOLORS)
	xpixels[n_pixels++] = gdImageColorAllocate(im, 0, 0, 0);

    for (i = 0; i < 256; i++)
    {
	Red[i] = (i * 6 / 256) * 6 * 6;
	Grn[i] = (i * 6 / 256) * 6;
	Blu[i] = (i * 6 / 256);
    }
}

void
InitColorTableFixed(void)
{
    if (true_color)
	init_colors_rgb();
    else
	init_colors_indexed();

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

int Color_table_fixed(void)
{
    table_type = FIXED;

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

    return 0;
}

static int get_color_rgb(int r, int g, int b)
{
    return gdTrueColor(r, g, b);
}

static int get_color_indexed(int r, int g, int b)
{
    return Red[r] + Grn[g] + Blu[b];
}

int _get_lookup_for_color(int r, int g, int b)
{
    return true_color
	? get_color_rgb(r, g, b)
	: get_color_indexed(r, g, b);
}

int get_table_type(void)
{
    return table_type;
}


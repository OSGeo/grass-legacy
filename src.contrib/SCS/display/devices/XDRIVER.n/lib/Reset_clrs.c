#include <stdio.h>
#include "colors.h"

Reset_colors(min, max, red, grn, blu)
int min, max;
unsigned char *red, *grn, *blu;
{
    if (get_table_type() == FLOAT)
        _float_Reset_colors(min, max, red, grn, blu);
    else
        _fixed_Reset_colors(min, max, red, grn, blu);
}

Reset_color(red, grn, blu, num)
unsigned char red, grn, blu;
int num;
{
    if (get_table_type() == FLOAT)
        _float_Reset_color(red, grn, blu, num);
    else
        _fixed_Reset_color(red, grn, blu, num);
}

static _fixed_Reset_colors(min, max, red, grn, blu)
int min, max;
unsigned char *red, *grn, *blu;
{
    int incr, n;

    for (incr = min, n = 0; incr <= max; incr++, n++)
        assign_fixed_color(incr, _get_lookup_for_color((int) red[n],
                        (int) grn[n], (int) blu[n]));
}

static _fixed_Reset_color(red, grn, blu, number)
unsigned char red, grn, blu;
int number;
{
    assign_fixed_color(number, _get_lookup_for_color((int) red,
                    (int) grn, (int) blu));
}

static _float_Reset_colors(min, max, red, grn, blu)
int min, max;
unsigned char *red, *grn, *blu;
{
    int incr, n;

    for (incr = min, n = 0; incr <= max; incr++, n++)
        _float_Reset_color(red[n], grn[n], blu[n], incr);
}

static _float_Reset_color(red, grn, blu, incr)
unsigned char red, grn, blu;
int incr;
{
    reset_color(incr + get_color_offset() + get_max_std_colors(), red,
            grn, blu);
}

/*** end Reset_clrs.c ***/

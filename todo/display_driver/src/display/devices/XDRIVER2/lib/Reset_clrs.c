#include <stdio.h>
#include "../XDRIVER.h"
#include "colors.h"

static int _fixed_Reset_colors(int,int,
                  unsigned char *,unsigned char *,unsigned char *);
static int _fixed_Reset_color(unsigned char,unsigned char,unsigned char,int);
static int _float_Reset_colors(int,int,
                  unsigned char *,unsigned char *,unsigned char *);
static int _float_Reset_color(unsigned char,unsigned char,unsigned char,int);

int Reset_colors(int min,int max,
unsigned char *red,unsigned char *grn,unsigned char *blu)
{
    if (get_table_type() == FLOAT)
        _float_Reset_colors(min, max, red, grn, blu);
    else
        _fixed_Reset_colors(min, max, red, grn, blu);

    return 0;
}

int Reset_color(unsigned char red,unsigned char grn,unsigned char blu,int num)
{
    if (get_table_type() == FLOAT)
        _float_Reset_color(red, grn, blu, num);
    else
        _fixed_Reset_color(red, grn, blu, num);

    return 0;
}

static int _fixed_Reset_colors(
    int min,int max,unsigned char *red,unsigned char *grn,unsigned char *blu)
{
    int incr, n;

    for (incr = min, n = 0; incr <= max; incr++, n++)
        assign_fixed_color(incr, _get_lookup_for_color((int) red[n],
                        (int) grn[n], (int) blu[n]));

    return 0;
}

static int _fixed_Reset_color(
unsigned char red,unsigned char grn,unsigned char blu,
int number)
{
    assign_fixed_color(number, _get_lookup_for_color((int) red,
                    (int) grn, (int) blu));

    return 0;
}

static int _float_Reset_colors(
int min,int max,
unsigned char *red,unsigned char *grn,unsigned char *blu)
{
    int incr, n;

    for (incr = min, n = 0; incr <= max; incr++, n++)
        _float_Reset_color(red[n], grn[n], blu[n], incr);

    return 0;
}

static int _float_Reset_color(
unsigned char red,unsigned char grn,unsigned char blu,
int incr)
{
    reset_color(incr + get_color_offset() + get_max_std_colors(), red,
            grn, blu);

    return 0;
}

/*** end Reset_clrs.c ***/

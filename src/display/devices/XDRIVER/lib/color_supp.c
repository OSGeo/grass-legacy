/***********************************************************************
 * The assign_fixed_color and get_fixed_color manage lookup tables
 * between map colors and real colors.
 * The assign_standard_color and get_standard_color manage lookup tables
 * between the standard (always available) colors and real colors.
 *
 ***********************************************************************
 */

#include <stdio.h>
#include <stdlib.h>

#define MAX_STD_COLORS  15

static int *pos_lookup, *neg_lookup;
static unsigned posLookAlloc = 0, negLookAlloc = 0;
static int standard_colors[MAX_STD_COLORS];
static int check_alloc_color(int);

/* assign_fixed_color(user_color, real_color) int user_color The color
 * number the user wants associated with int real_color  this real (or
 * device dependent) color.
 * 
 * Maintains two tables, one for colors greater than or equal to zero, and
 * the other for colors less than zero.  (With GRASS 3.0 negative
 * category numbers are accommodated in the data base.)
 * 
 * On allocation error the current program ungracefully exits. */
int assign_fixed_color( int user_color,int real_color)
{
    check_alloc_color(user_color);
    if (user_color >= 0)
        pos_lookup[user_color] = real_color;
    else
        neg_lookup[-user_color] = real_color;

    return 0;
}


/* get_fixed_color(user_color) int user_color   The color referenced by
 * the user
 * 
 * returns the real device color as set in an assign_fixed_color call, or
 * -1 if not set. */
int get_fixed_color (int user_color)
{
    if (user_color >= 0) {
        if (posLookAlloc > user_color)
            return (pos_lookup[user_color]);
        else
            return 0;
    } else {
        user_color = -user_color;
        if (negLookAlloc > user_color)
            return (neg_lookup[user_color]);
        else
            return 0;
    }
}


int get_fixed_color_array (register int *a, register int num)
{
    register int i, j;

    for (i = 0; i < num; i++, a++) {
        if (*a >= 0) {
            if (posLookAlloc > *a)
                *a = pos_lookup[*a];
            else
                *a = 0;
        } else {
            j = -*a;
            if (negLookAlloc > j)
                *a = neg_lookup[j];
            else
                *a = 0;
        }
    }

    return 0;
}


static int check_alloc_color(int n)
{
    unsigned to_alloc;

    if (n >= 0) {
        if (++n < posLookAlloc)
            return 0;
        to_alloc = posLookAlloc;
        while (n >= to_alloc)
            to_alloc += 512;

        if (posLookAlloc)
            pos_lookup = (int *)G_realloc((void *) pos_lookup,
                    (size_t) (to_alloc * sizeof(int)));
        else
            pos_lookup = (int *) G_malloc((size_t) (to_alloc * sizeof(int)));
        if (!pos_lookup) {
            perror("ERROR: can't alloc in check_alloc_color.");
            exit(-1);
        }
        posLookAlloc = to_alloc;
        return 0;
    } else {
        n = -n + 1;
        if (n < negLookAlloc)
            return 0;
        to_alloc = negLookAlloc;
        while (n >= to_alloc)
            to_alloc += 512;

        if (negLookAlloc)
            neg_lookup = (int *)G_realloc((void *) neg_lookup,
                    (size_t) (to_alloc * sizeof(int)));
        else
            neg_lookup = (int *) G_malloc((size_t) (to_alloc * sizeof(int)));
        if (!neg_lookup) {
            perror("ERROR: can't alloc in check_alloc_color.");
            exit(-1);
        }
        negLookAlloc = to_alloc;
    }
    return 0;
}


/* assign_standard_color(user_color, real_color) int user_color    The
 * color number the user wants associated with int real_color    this
 * real (or device dependent) color.
 * 
 * On allocation error the current program ungracefully exits. */
int assign_standard_color (int user_color, int real_color)
{
    if ((user_color >= 0) && (user_color < MAX_STD_COLORS))
        standard_colors[user_color] = real_color;

    return 0;
}


/* get_standard_color(user_color) int user_color       The color
 * referenced by the user
 * 
 * returns the real device color as set in an assign_fixed_color call, or
 * zero if not set. */
int get_standard_color (int user_color)
{
    if ((user_color >= 0) && (user_color < MAX_STD_COLORS))
        return standard_colors[user_color];
    else
        return -1;
}


/* get_max_std_colors()
 * 
 * returns the number of colors always reserved in floating color tables
 * for the standard colors */
int get_max_std_colors (void)
{
    return (MAX_STD_COLORS);
}

/*** end color_supp.c ***/

#include "../XDRIVER.h"
#include "colors.h"
static int color_offset = 0;
static int first_time = 1;
static int max_std_colors;
static int n_colors;

int Color(int number)
{
    SetXColor(_get_color_index(number));
    return 0;
}

int RGB_color(unsigned char r,unsigned char g,unsigned char b)
{
    /* assumes fixed color mode */
    SetXColor( _get_lookup_for_color((int) r, (int) g, (int) b));
    return 0;
}

int _get_color_index(int number)
{
    int num;

    if (first_time) {
        max_std_colors = get_max_std_colors();
        Get_num_colors(&n_colors);
        first_time = 0;
    }
    if (get_table_type() == FIXED) {
        return (get_fixed_color(number));
    } else {                    /* table_type == FLOAT */
        if (number >= 0) {
            /* silently wrap colors in float mode */
            num = max_std_colors + color_offset + number;
            if (num > n_colors)
                num = num % n_colors;
            return (num);
        }
        /* else Ignore negative color requests in float mode */
    }
    return (0);
}

int _get_color_index_array(int *a,int num)
{
    int i;

    if (first_time) {
        max_std_colors = get_max_std_colors();
        Get_num_colors(&n_colors);
        first_time = 0;
    }
    if (get_table_type() == FIXED) {
        get_fixed_color_array(a, num);
        return 1;
    } else {                    /* table_type == FLOAT */
        for (i = 0; i < num; i++) {
            if (*a >= 0) {
                /* silently wrap colors in float mode */
                *a = max_std_colors + color_offset + *a;
                if (*a > n_colors)
                    *a = *a % n_colors;
            } else {            /* clamp negative color requests to
                                 * zero */
                *a = 0;
            }
            a++;
        }
    }

    return 0;
}

int Standard_color(int number)
{
    if (get_table_type() == FIXED) {
		SetXColor(get_standard_color(number));
    } else {
        SetXColor(number);
	}
    return 0;
}

int Color_offset(int n)
{
    color_offset = n;
    return 0;
}

int get_color_offset(void)
{
    return color_offset;
}


/*** end Color.c ***/

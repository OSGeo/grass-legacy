#include "global.h"


print_unit(i,ns,nl)
{
    char num[50];
    double area_sum();
    long  count_sum();
    int k;
    double area;

    if (unit[i].type == CELL_COUNTS)
    {
	sprintf (num, "%*ld", unit[i].len, count_sum (&ns, nl));
    }
    else if (unit[i].type == PERCENT_COVER)
    {
	k = ns-1;
	while (k >= 0 && same_cats (k,ns,nl-1))
	    k--;
	k++;
	area = area_sum (&k, nl-1);
	format_double (100.0 * area_sum (&ns, nl) / area,
	    num, unit[i].len, unit[i].dp);

    }
    else
    {
	format_double (area_sum (&ns, nl) * unit[i].factor,
	    num, unit[i].len, unit[i].dp);
    }
    printf ("|%s", num);
}

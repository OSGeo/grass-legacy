#include "global.h"


print_unit(i,ns,nl)
{
    char num[50];
    double area_sum();
    double len_sum();
    long  count_sum();
    int k;
    double area;

    if (unit[i].type == COUNTS)
    {
  	sprintf (num, "%*ld", unit[i].len, count_sum (&ns, nl));
    }
    else
    {
    if (unit[i].type == LN_METERS ||
        unit[i].type == LN_FEET ||
        unit[i].type == LN_KILOMETERS ||
        unit[i].type == LN_MILES ) 
  	format_double (len_sum (&ns, nl) * unit[i].factor,
	    num, unit[i].len, unit[i].eformat, unit[i].dp);
    else
  	format_double (area_sum (&ns, nl) * unit[i].factor,
	    num, unit[i].len, unit[i].eformat, unit[i].dp);
    }
    printf ("|%s", num);
}

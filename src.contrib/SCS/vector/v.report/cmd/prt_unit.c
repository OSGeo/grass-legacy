#include "global.h"


int 
print_unit (int i, int ns, int nl)
{
    char num[50];
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
    fprintf (stdout,"|%s", num);

    return 0;
}

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
        {
         if (e_format)
            scient_format (len_sum (&ns, nl) * unit[i].factor,
               num, unit[i].len, unit[i].dp);
         else
  	    format_double (len_sum (&ns, nl) * unit[i].factor,
	       num, unit[i].len, unit[i].dp);
	}
    else
        {
         if (e_format)
            scient_format (area_sum (&ns, nl) * unit[i].factor,
               num, unit[i].len, unit[i].dp);
         else
    	    format_double (area_sum (&ns, nl) * unit[i].factor,
	       num, unit[i].len, unit[i].dp);
	}
    }
    fprintf (stdout,"|%s", num);

    return 0;
}

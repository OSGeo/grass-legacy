#include "table.h"

#define F_CTOK(C)	((C)/1000000.0)
#define F_CTOM(C)	F_CTOK(C) *   0.3861
#define F_CTOA(C)	F_CTOK(C) * 247.1000
#define F_CTOH(C)	F_CTOK(C) * 100.0000

char *
conv(key,count,cat,area)
    long count;
    long cat;
    double area;
{
    static char entry[40];  /* this MUST be static! */
    double buf[40];
    double value;
    long x;

    switch(key)
    {
    case 0:
	    sprintf(entry,"%9ld |",count);
	    break;
    case 1:
	    value = count*100.0/ncells;
	    sprintf(entry,"%9.2lf |",value);
	    break;
    case 2:
	    x = ncells - count0;
	    if (x <= 0)
		sprintf(entry,"%9s |"," ???");
	    else
	    {
		if(cat)
		    value = count * 100.0 / x;
		else
		    value = (count-count0) * 100.0 / x;
		sprintf(entry,"%9.2lf |",value);
	    }
	    break;
    case 3:
	    value = F_CTOA(area);
	    format_double (value, buf, 9);
	    sprintf(entry,"%9s |",buf);
	    break;
    case 4:
	    value = F_CTOH(area);
	    format_double (value, buf, 9);
	    sprintf(entry,"%9s |",buf);
	    break;
    case 5:
	    value = F_CTOK(area);
	    format_double (value, buf, 9);
	    sprintf(entry,"%9s |",buf);
	    break;
    case 6:
	    value = F_CTOM(area);
	    format_double (value, buf, 9);
	    sprintf(entry,"%9s |",buf);
	    break;
    default:
	    sprintf(entry,"---------|");
	    break;
    }
    return(entry);
}

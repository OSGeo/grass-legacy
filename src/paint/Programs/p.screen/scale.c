#include <stdio.h>
scan_scale(s, scale, units)
    char *s;
    double *scale;
    char *units;
{
    char u[2];

    *scale = 1.0;
    *units = 'p';
    if (s == NULL)
	return 1;

    *u = 0;
    switch (sscanf (s, "%lf%1s", scale, u))
    {
    case 2:  break;
    case 1:  if (*u != 0) return 0;
	     break;
    default: return 0;
    }
    if (*scale <= 0.0)
	return 0;
    switch (*u)
    {
	case 0:   *units = ' ';
		  break;
	case 'I':
	case 'i': *units = 'i';
		  break;
	case 'P':
	case 'p': *units = 'p';
		  break;
	default:  return 0;
    }
    return 1;
}

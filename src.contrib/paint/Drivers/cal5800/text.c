/* %W% %G% */
#include <string.h>
#include "P.h"
Ptext (s)
    char *s;
{
    float zerof = 0.0;
    int neg_3 = (-3);
    int nchar;
    float width = .10;	/* You can play with these values to get the */
    float move  = .20;	/* size text you want */
    float angle = 90.0;
    
    nchar = strlen (s);
    plot_ (&move, &zerof, &neg_3);
    symbol_ (&zerof, &zerof, &width, s, &zerof, &angle, &nchar);
    plot_ (&move, &zerof, &neg_3);
}

/*
 * print_lut.c
 *
 */

#include <stdio.h>
#include "gis.h"
#include "specs.h"

print_lut(colors)
struct Colors colors;
{
int i;
uchar *R, *G, *B;

R = colors.red;
G = colors.grn;
B = colors.blu;
for (i=colors.min ; i<=colors.max; i++)
   {
   printf("%d %d %d %d\n",i,*R,*G,*B);
   R++; G++; B++;
   }
}

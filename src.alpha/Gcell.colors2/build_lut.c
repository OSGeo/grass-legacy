/*
 * build_lut.c
 *
 * Subroutine of the GRASS Gcell.colors.new program
 *
 * Accepts a linked list of color-table specifications and a
 * GRASS color-table structure and returns the GRASS color-table,
 * modified according to the color-table specs.
 *
 * returns 0 on succesful completion, -1 on error
 *
 */ 

#include <stdio.h>
#include "gis.h"
#include "specs.h"

build_lut(specs,colors)
struct SpecList *specs; 
struct Colors *colors;
{
struct Spec *spec;
float  Rincr,Gincr,Bincr,
       Iincr,
       Rsum,Gsum,Bsum;
int    i=0,
       R,G,B;
CELL   j=0;

if (specs->first == NULL)
   return(-1); 

spec = specs->first;

/* just one spec - put it in table */
if (specs->NumSpecs == 0)
   {
   G_set_color(spec->index,spec->R+Rsum,spec->G+Gsum,spec->B+Bsum,colors);
   return(0); 
   }

for (i=0; i<specs->NumSpecs; i++)
   {
   Iincr = (spec->next->index - spec->index);
   Rincr = (spec->next->R - spec->R) / Iincr;
   Gincr = (spec->next->G - spec->G) / Iincr;
   Bincr = (spec->next->B - spec->B) / Iincr;
   Rsum  = Gsum = Bsum = 0.0;

   for (j=spec->index; j<=spec->next->index; j++)
      {
      R = spec->R+Rsum;
      G = spec->G+Gsum;
      B = spec->B+Bsum;
      G_set_color(j,R,G,B,colors);
      Rsum += Rincr;
      Gsum += Gincr;
      Bsum += Bincr;
      }
   spec = spec->next;
   }
return(0); /* no problem */
}

#include "gis.h"

process_inp(inp)
char * inp;
{
   G_remove_commas(inp);
   while(*inp)
   {
     if((*inp=='D')||(*inp=='d'))  *inp='e';
     inp++;
   }
}

#include "gis.h"

int process_inp (char *inp)
{
   G_remove_commas(inp);
   while(*inp)
   {
     if((*inp=='D')||(*inp=='d'))  *inp='e';
     inp++;
   }

   return 0;
}

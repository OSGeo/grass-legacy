#include "gis.h"

int process_inp (char *inp)
{
   while(*inp)
   {
     if((*inp=='D')||(*inp=='d'))
	 *inp='e';
     if (*inp==',')
	 *inp = ' ';
     inp++;
   }

   return 0;
}

#include "gis.h"
#include "globals.h"
#include "rim.h"


int get_max_seq()
{
   int i, status;

   i = 0;
   if ((status=crim(DATA_TABLE, "select from data"))==RIM_EOT)
       return(0);
   if (status!=0) rim_error(status);

   if (crimdm(DATA_TABLE,GET,Rim_buffer)!=0)
      return(0);
   i = Rim_buffer[0];
   while ((status=crimdm(DATA_TABLE,GET,Rim_buffer))==0) {
      if (i<Rim_buffer[0]) i = Rim_buffer[0];
   }
   if (status!=RIM_EOT) rim_error(status);
   return(i);
}

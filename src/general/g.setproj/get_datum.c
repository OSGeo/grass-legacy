#include "gis.h"
#include "local_proto.h"

int ask_datum(char *datum) 
{
 int answer;

 if(!G_yes("Do you want to specify a map datum for this location?", 0))
   return 0;

 answer = G_ask_datum_name(datum);
 if (answer)
   return 1;
 else
   return -1;
}



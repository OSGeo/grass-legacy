#include "distance.h"
int
END(map)
 struct Map_info *map;
 {

#ifdef DEBUG
fprintf(stderr,"z (END)\n");
#endif DEBUG
   R_close_driver();
   dig_P_fini(map);
   return(1);
 }

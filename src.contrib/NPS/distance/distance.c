#include <math.h>
#include "distance.h"
int
distance(x1,y1,x2,y2,dist)
 double x1,y1,x2,y2,*dist;
 {
  *dist = hypot((x1-x2),(y1-y2));
  return(1);
 }

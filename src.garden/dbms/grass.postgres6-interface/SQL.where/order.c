#include <math.h>

double distance_from_pointer(double point_east, double point_north, char *location_east, char *location_north)
{
  double diff_east, diff_north, result;
  double loc_east, loc_north;



  loc_north=atoi(location_north);
  loc_east=atoi(location_east);


  diff_east=point_east-(double)loc_east;
  diff_north=point_north-(double)loc_north;
  

  (double)result=sqrt((diff_east*diff_east)+(diff_north*diff_north));

return((double)result);
}


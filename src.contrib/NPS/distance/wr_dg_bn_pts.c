#include "distance.h"
int
wr_dg_bn_pts(itype)
 int itype;
 {
  int n_points;

  fwrite(&itype, sizeof(int), 1, dig_bin);
  n_points = total_points;
#ifdef DEBUG
fprintf(stderr,"wr_dg_bn_pts:  itype=%d\n",itype);
fprintf(stderr,"wr_dg_bn_pts:  total_points=%ld\n",total_points);
fprintf(stderr,"wr_dg_bn_pts:  n_points=%ld\n",n_points);
fprintf(stderr,"wr_dg_bn_pts:  x_counter=%ld\n",x_counter);
fprintf(stderr,"wr_dg_bn_pts:  y_counter=%ld\n",y_counter);
#endif DEBUG
  fwrite(&n_points,sizeof(int),1,dig_bin);
  fwrite(x_array,sizeof(double),total_points,dig_bin);
  fwrite(y_array,sizeof(double),total_points,dig_bin);
  fflush (dig_bin);
  return(1);
 }

#include "usgs.h"

get_profile()
{
  double junk;

  next_record();        /* advance "buffer" to the beginning of next record */
  buffer += get_int(&bas_n);
/*  if(filestat != blocksize)
    return(0);
    */
  buffer += get_int(&bas_e);
  buffer += get_int(&rows);
  buffer += get_int(&cols);
  buffer += get_double(&P_col);
  buffer += get_double(&P_row);
  buffer += get_double(&bas_elev);

  buffer += get_double(&junk);
  buffer += get_double(&junk);

#ifdef DEBUG 
  printf("bas_n %d, bas_e %d\n", bas_n,bas_e);
  printf("rows %d, cols %d\n", rows,cols);
  printf("P_col %f, P_row %f\n", P_col, P_row);
  printf("bas_elev %f\n", bas_elev);
#endif 

  return(1);
}

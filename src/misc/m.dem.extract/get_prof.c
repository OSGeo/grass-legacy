#include "usgs.h"

int 
get_profile (void)
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
  fprintf (stdout,"bas_n %d, bas_e %d\n", bas_n,bas_e);
  fprintf (stdout,"rows %d, cols %d\n", rows,cols);
  fprintf (stdout,"P_col %f, P_row %f\n", P_col, P_row);
  fprintf (stdout,"bas_elev %f\n", bas_elev);
#endif 

  return(1);
}

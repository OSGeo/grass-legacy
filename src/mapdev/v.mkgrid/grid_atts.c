/*
  Added June 2000, DD Gray. To provide automatic area points and attributes for
  the grid, as an option 
*/

#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"
#include "grid_structs.h"
#include "local_proto.h"

void set_grid_area_points( double *xlist, double *ylist, struct grid_description *gd1 ) {
  
  /* Allocate arrays for x and y coordinates of area points and set values from
     information in the struct grid_description
  */

  double refx = gd1->origin_x;
  double refy = gd1->origin_y;
  double alpha = gd1->angle;

  double deltax = gd1->length;
  double deltay = gd1->width;

  int nr = gd1->num_rows;
  int nc = gd1->num_cols;

  int i,j;

  for( i = 0; i < nr; ++i ) {
    for( j = 0; j < nc; ++j ) {
      xlist[i*nc+j] = refx + ( 0.5 + j ) * deltax;
      ylist[i*nc+j] = refy + ( 0.5 + i ) * deltay;
      rotate( &xlist[i*nc+j], &ylist[i*nc+j], refx, refy, alpha );
    }
  }
}

void set_grid_attributes( int *gatts, struct Categories *cats, struct grid_description *gd1, AttributeType att_type ) {

  /* Fill out array of attribute values */

  /* Local variables */

  double refx = gd1->origin_x;
  double refy = gd1->origin_y;
  double alpha = gd1->angle;

  double deltax = gd1->length;
  double deltay = gd1->width;

  int nr = gd1->num_rows;
  int nc = gd1->num_cols;

  char buf[100], let;

  /* loop */
  int i,j;
  int rownum, colnum;

  /* Attribute value */
  int av;

  /* Retrieve the constant attribute value, which may be required */
  if(proc_const_attribute_value( GET_VAL, &av ) != 0)
    {
      av = 1;
    }


  switch( att_type ) {
  case ATT_CONSTANT:
    {
      for( i = 0; i < nr; ++i ) {
	for( j = 0; j < nc; ++j ) {
	  gatts[i*nc+j] = av;
	}
      }
      break;
    }
  case ATT_COLS:
    {
      for( i = 0; i < nr; ++i ) {
	for( j = 0; j < nc; ++j ) {
	  rownum = nr - i - 1;
	  colnum = j;
	  gatts[i*nc+j] = colnum * nr + rownum + 1;
	  let = (char) ( rownum + 'A' );
	  snprintf (buf, 100, "%c %d", let, colnum+1);
	  G_set_cat( gatts[i*nc+j], buf  , cats);	  
	}
      }
      break;
    }
  case ATT_ROWS:
    {
      for( i = 0; i < nr; ++i ) {
	for( j = 0; j < nc; ++j ) {
	  rownum = nr - i - 1;
	  colnum = j;
	  gatts[i*nc+j] = rownum * nc + colnum + 1;
	  let = (char) ( rownum + 'A' );
	  snprintf (buf, 100, "%c %d", let, colnum+1);
	  G_set_cat( gatts[i*nc+j], buf  , cats);	  
	}
      }
      break;
    }
  }
}

int proc_const_attribute_value( int MODE, int *value ) {

  /* Set or get the value of the constant attribute value re.
     a static storage location within this function
  */

  static int const_value = 1;

  if( MODE == GET_VAL ) {

    if( value ){
      *value = const_value;
      return (0);
    }
    else
      {
	return (1);
      }
      
  }
  else if( MODE == SET_VAL) {
    if(value) {
      const_value = *value;
      return (0);
    }
    else
      return(1);
  }
  else return(1);
    
}

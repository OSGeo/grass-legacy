/* Test the facility for adding, subtracting, multiplying matrices */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "la.h"

#define G_vector_free(x) G_matrix_free( (x) )
#define G_vector_print(x) G_matrix_print( (x) )

int main(int argc, char *argv[]) {

  int i, j;
  mat_struct *m1, *m2, *m_sum, *m_sub, *m_scale, *m3, *m4;
  vec_struct *v1, *v2;
  double maxval, minval, absval, euclid;

  double testmat1[5][3] = {  1.0, 3.4, 8.1,
			     1.5, 1.5, 2.3,
			     2.0, 2.2, 1.7,
			     2.5, 6.3, 6.1,
			     3.0, 1.6, 5.0
  };

  double testmat2[5][3] = {  7.3, 0.5, 2.6,
			     6.9, 1.2, 2.8,
			     5.5, 1.9, 5.1,
			     9.3, 7.7, 7.0,
			     0.5, 3.1, 3.8
  };

  double testvec1[5] = { 1., 2., -3., 4., 5. };
  
  double testc = 4.3;

  
  /* Initialise the matrix structures */

  m1 = G_matrix_init(5, 3, 5);
  m2 = G_matrix_init(5, 3, 5);

  for( i = 0; i < 5; i++ )
    for( j = 0; j < 3; j++ ) {

      G_matrix_set_element(m1, i, j, testmat1[i][j]);
      G_matrix_set_element(m2, i, j, testmat2[i][j]);
    }

  /* Add the matrices */

  m_sum = G_matrix_add(m1, m2);

  /* Subtract */

  m_sub = G_matrix_subtract(m1, m2);

  /* Scale the matrix by a given scalar value */

  m_scale = G_matrix_scale(m1, testc);

  /* Multiply two matrices */

  m3 = G_matrix_transpose(m1);

  m4 = G_matrix_product(m3, m2);



  /* Get vector from Matrix column */
  
  v1 = G_matvect_get_column(m1, 1);

  /* Init and set new vector */
  
  v2 = G_vector_init(5, 5, CVEC);

  for( i = 0; i < 5; i++ ) {
            G_matrix_set_element(v2, i, 0, testvec1[i]);
  }
  
  maxval = G_vector_norm_maxval(v2, 1);
  minval = G_vector_norm_maxval(v2, -1);
  absval = G_vector_norm_maxval(v2, 0);

  /* Euclidean vector norm */
  
  euclid = G_vector_norm_euclid(v2);

  /* Print out the results */

  printf("*** TEST OF MATRIX WRAPPER FUNCTIONS ***\n\n");

  printf("1. Simple matrix manipulations\n\n");

  printf("    Matrix 1:\n");
  G_matrix_print(m1);

  printf("    Matrix 2:\n");
  G_matrix_print(m2);

  printf("    Sum (m1 + m2):\n");
  G_matrix_print(m_sum);

  printf("    Difference (m1 - m2):\n");
  G_matrix_print(m_sub);

  printf("    Scale (c x m1):\n");
  G_matrix_print(m_scale);

  printf("    Multiply transpose of M1 by M2 (m1~ x m2):\n");
  G_matrix_print(m4);
  
  printf("*** TEST OF VECTOR WRAPPER FUNCTIONS ***\n\n");

  printf("    Get vector from Matrix M1 column 1:\n");
  G_vector_print(v1);

  printf("    Vector v2:\n");
  G_vector_print(v2);
  
  printf("    Get maximum of vector v2:\n");
  printf("    %g\n", maxval);
  
  printf("    Get minimum of vector v2:\n");
  printf("    %g\n", minval);

  printf("    Get abs val of vector v2:\n");
  printf("    %g\n", absval);

  printf("    Euclidean vector norm of v2:\n");
  printf("    %g\n", euclid);
  
  G_matrix_free(m1);
  G_matrix_free(m2);
  G_matrix_free(m_sum);
  G_matrix_free(m_sub);
  G_matrix_free(m_scale);
  G_matrix_free(m3);
  G_matrix_free(m4);
  G_vector_free(v1);
  G_vector_free(v2);

  return 0;

}

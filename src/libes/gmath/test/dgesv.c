/* Test the functionality of the Fortran dgesv routine for solving linear systems
   for a general real-valued matrix
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "la.h"


int main(int argc, char *argv[]) {

  /* local */

  int i, j;  /* loop */
  mat_struct *mt1 = NULL, *mt2 = NULL, *mt3 = NULL;

  double testmat1[3][3] = {  1.0, 3.4, 8.1,
			     1.5, 1.5, 2.3,
			     2.0, 2.2, 1.7,
  };

  mt1 = G_matrix_init(3, 3, 3);

  for( i = 0; i < 3; i++ ) {

    for( j = 0; j < 3; j++ ) {
      G_matrix_set_element( mt1, i, j, testmat1[i][j] );
    }
  }

  printf("*** TEST OF LU SOLVER ROUTINE DGESV_ ***\n\n");

  printf("    Original Matrix:\n");
  G_matrix_print(mt1);

  /* Find the inverse. This calls dgesv, and so tests its function */

  mt2 = G_matrix_inverse(mt1);

  
  /* Multiply the original by the calculated inverse to check its accuracy */

  mt3 = G_matrix_product(mt1, mt2);


  /* Print out results */

  printf("    Inverse:\n");
  G_matrix_print(mt2);

  printf("    Multiply original by inverse:\n");
  G_matrix_print(mt3);

  G_matrix_free(mt1);
  G_matrix_free(mt2);
  G_matrix_free(mt3);

  return 0;
}

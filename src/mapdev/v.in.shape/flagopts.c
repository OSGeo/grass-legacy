
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "shapefil.h"


int
listbbox(char *infile) {

  SHPHandle hS;

  double *bb1 = NULL, *bb2 = NULL;
  int dummy1, dummy2;

  if( (hS = SHPOpen(infile, "r")) == NULL ) {
    fprintf(stderr, "Unable to open shapefile database.\n");
    return -1;
  }

  if( (bb1 = (double *)malloc( 4 * sizeof(double) )) == NULL ) {
    fprintf(stderr, "Unable to allocate space for bbox info.\n");
    SHPClose(hS);
    return -1;
  }

  if( (bb2 = (double *)malloc( 4 * sizeof(double) )) == NULL ) {
    fprintf(stderr, "Unable to allocate space for bbox info.\n");
    SHPClose(hS);
    if(bb1) free(bb1);
    return -1;
  }

  SHPGetInfo(hS, &dummy1, &dummy2, bb1, bb2);

  fprintf(stdout, "\nBOUNDS OF REGION ARE: \n\n");
  fprintf(stdout, "  West:  %.6f\n  North: %.6f\n  East:  %.6f\n  South: %.6f\n",
	  bb1[0], bb2[1], bb1[1], bb2[0] );

  if(bb1) free(bb1);
  if(bb2) free(bb2);

  SHPClose(hS);

  return 0;
}

#include <stdio.h>

int **a;
main() {

  int k;

  a = (int **) malloc(sizeof(int)*5);

  for(k=0;k < 5;k++) {
	a[k] = (int *) malloc(sizeof(int)*5);
        a[k][0] = 5;
  } 
  lawntobed();

}

#include <stdio.h>

main() {
int *x, y[10];

	x = (int *) malloc(sizeof(int)*10);
	y[0] = 10; y[1] = 11; y[2] = 12; y[3] = 13;

	x = y;

	printf("%d\n",x[3]);
	
	x[3] = -10;

	printf("%d\n",y[3]);
}

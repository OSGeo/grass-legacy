#include <stdlib.h>
/* sort an array of ints */
static int descending (int *,int *);
static int ascending (int *,int *);

int sort_int (int array[], int n, int order)
{
	int ascending();
	int descending();

	if (order > 0)
		qsort (array, n, sizeof(int), ascending);
	else
		qsort (array, n, sizeof(int), descending);

    return 0;
}
	
static int ascending (int *a,int *b)
{
	return (*a - *b) ;
}
	
static int descending (int *a,int *b)
{
	return (*b - *a) ;
}

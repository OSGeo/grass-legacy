/* sort an array of ints */

sort_int (array, n, order)

	int array[];
{
	int ascending();
	int descending();

	if (order > 0)
		qsort (array, n, sizeof(int), ascending);
	else
		qsort (array, n, sizeof(int), descending);
}
static ascending (a ,b)
	
	int *a, *b;
{
	return (*a - *b) ;
}
static descending (a ,b)
	
	int *a, *b;
{
	return (*b - *a) ;
}

int maximum (register int *array, int size)
{
	register int n;
	register int max;
	register int a;
	int x;

	max = *array++;
	x = 0;

	for (n = 1; n < size; n++)
	{
		a = *array++;
		if (a > max)
		{
			max = a;
			x = n;
		}
	}
	return x;
}

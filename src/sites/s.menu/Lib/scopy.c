scopy (a, b ,n)

	char *a;
	char *b;
{
	while ((--n > 0) && (*a = *b++))
		a++;
	*a = 0;
}

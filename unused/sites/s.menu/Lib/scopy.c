int scopy (char *a, char *b, int n)
{
	while ((--n > 0) && (*a = *b++))
		a++;
	*a = 0;

    return 0;
}

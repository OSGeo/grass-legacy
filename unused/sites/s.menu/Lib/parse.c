/* break buf into tokens. delimiters are replaced by NULLs
   and items array will point to varous locations in buf
   buf must not have a new line
*/
int 
parse (char *buf, char *items[], int n, char *delim)
{
	int i;
	char *index();

	i = 0;

	while (i < n)
	{
		while (*buf == ' ' || *buf == '\t')
			buf++;
		if (*buf == 0)
			break;
		items[i++] = buf;

		while (*buf && (index(delim,*buf) == 0))
			buf++;
		if (*buf == 0)
			break;
		*buf++ = 0;
	}
	if (i < n)
		items[i] = buf;

	return (i);
}

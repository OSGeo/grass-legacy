/****************************************************************************
  read ascii and transfer it to integer
****************************************************************************/
atoint (s)

char	*s;
{
	int	n = 0, sign = 1;
	if (*s == '-') {
		sign = -1;
		s++;   
	}
	if (*s == ' ')
		s++;
	if (!(*s >= '0' && *s <= '9'))
		err ("ERROR bad argument .... integer expected\n");

	for ( ; *s >= '0' && *s <= '9' ; s++)
		n = 10 * n + (*s - '0');
	return (n * sign);
}



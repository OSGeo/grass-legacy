char *ftoa (i)

int i;
{
	char s[20], *hi;
	char *reverse();
	int j,sign,k;

	k = i;

	if ((sign = i)<0)
		i = -i;
	j = 0;
	do{
		if(j == 2) s[j++] = '.';
		s[j++] =  i % 10 +'0';
	} while ((i  /= 10) > 0);

	if(k < 100) s[j++] = '.';

	if (sign < 0)
		s[j++] = '-';
	s[j] = '\0';
	hi = reverse(s);
	return(hi);
}


char *reverse (s)
char s[];
{
	int c,i,j;


	for (i = 0, j = strlen(s)-1; i<j;i++,j--){
		c = s[i];
		s[i] = s[j];
		s[j] = c;
		}
	return (s);
}

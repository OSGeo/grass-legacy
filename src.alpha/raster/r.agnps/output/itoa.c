char *itoa (i)

int i;
{
	char s[20], *hi;
	char *reverse_i();
	int j,sign;

	if ((sign = i)<0)
		i = -i;
	j = 0;
	do{
		s[j++] =  i % 10 +'0';
	} while ((i  /= 10) > 0);


	if (sign < 0)
		s[j++] = '-';
	s[j] = '\0';
	hi = reverse_i(s);
	return(hi);
}


char *reverse_i (s)
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

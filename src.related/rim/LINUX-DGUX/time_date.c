# include	<stdio.h>
# include	<time.h>
# include	<ctype.h>

void	itime_(v)
	int	*v;
{
	time_t		t;
	struct tm	*p;

	(void)time(&t);
	p = localtime(&t);
	v[0] = p->tm_hour;
	v[1] = p->tm_min;
	v[2] = p->tm_sec;
}

void	idate_(v)
	int	*v;
{
	time_t		t;
	struct tm	*p;

	(void)time(&t);
	p = localtime(&t);
	v[0] = p->tm_mday;
	v[1] = p->tm_mon+1;
	v[2] = p->tm_year+1900;
}


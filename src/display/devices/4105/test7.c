main()
{
	int num ;
	int color ;
	int n ;
	int r[64], c[64] ;
	char buff[80] ;
	Graph_Set() ;
	Color(1) ;
	for(;;)
	{
		fprintf (stdout,"n points > ") ;
		gets(buff) ;
		sscanf(buff, "%d", &num) ;
		if (num == 0)
			exit() ;

		fprintf (stdout,"color > ") ;
		gets(buff) ;
		sscanf(buff, "%d", &color) ;

		Color(color) ;

		for(n=0; n<num; n++)
		{
			fprintf (stdout,"point %d > ", n+1) ;
			gets(buff) ;
			sscanf(buff, "%d %d", &c[n], &r[n]) ;
fprintf (stdout,"        %d,%d\n", c[n], r[n]) ;
		}

		Polygon_abs(c, r, num) ;
	}
}

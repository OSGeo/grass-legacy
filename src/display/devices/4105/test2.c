main()
{
	int row ;
	int r, c ;
	Graph_Set() ;
	Color(1) ;
	for(;;)
	{
		scanf("%d %d", &r, &c) ;
		if (r+c == 0)
			exit() ;
		Move_abs(c,r) ;
		Cont_abs(c+1,r) ;
	}
}

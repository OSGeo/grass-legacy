main()
{
	int num ;
	int color ;
	int n ;
	int r[64], c[64] ;
	char buff[80] ;
	Graph_Set() ;

	Color(4) ;

	c[0] =  10 ; r[0] =  10 ;
	c[1] =  10 ; r[1] = 100 ;
	c[2] = 100 ; r[2] = 100 ;
	c[3] = 100 ; r[3] =  10 ;
	c[4] =  10 ; r[4] =  10 ;

	Polygon_abs(c, r, 5) ;
}


Get_color (n, red, grn, blu)
    float *red, *grn, *blu;
{
	int r, g, b ;

	if (n<0)
	{
		*red = 0.0 ;
		*grn = 0.0 ;
		*blu = 0.0 ;
		return ;
	}

	r = n / 25 ;
	g = (n % 25) / 5 ;
	b = (n % 25) % 5 ;

	*red = .1 + .2 * r ;
	*grn = .1 + .2 * g ;
	*blu = .1 + .2 * b ;
}

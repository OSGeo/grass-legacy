get_mouse(x, y)
	int *x, *y ;
{
	put_chr('U') ;
	*x = get_int() ;
	*y = get_int() ;
}

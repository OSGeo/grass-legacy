/*  @(#)add_sub.c	2.1  6/26/87  */
add_int(num, size, bufptr)
	int num ;
	int *size ;
	int **bufptr ;
{
	char *realloc() ;


	(*size)++ ;
	*bufptr = (int *)realloc(*bufptr, (*size) * sizeof (int)) ;
	*(*bufptr + *size - 1) = num ;

}

sub_int(num, size, bufptr)
	int num ;
	int *size ;
	int **bufptr ;
{
	int i ;
	int *oldbuf, *newbuf ;

	oldbuf = *bufptr ;
	newbuf = *bufptr ;

	for (i=0; i < *size; i++)
	{
		if ( *oldbuf == num )
			break ;
		newbuf++ ;
		oldbuf++ ;
	}
	if (i < *size)
	{
		oldbuf++ ;
		while (++i < *size)
		{
			*newbuf++ = *oldbuf++ ;
		}

		(*size)-- ;

		*bufptr = (int *)(realloc(*bufptr, *size * sizeof (int))) ;
		return(1) ;
	}

	return(0) ;
}

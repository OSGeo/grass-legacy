/*  @(#)save.c	2.1  6/26/87  */
/*******************************************************************
 * G_save (new, old, n_elem, size))
 *
 * allocates permanent memory to hold old data;
 * copies old-data to this memory, and returns the address
 * of the allocated memory
 *******************************************************************/

D_save (new, old, n_elem, size)
	char **new, *old ;
	int n_elem, size ;
{
	char *ptr ;
	char *falloc() ;
	int n ;

	n = n_elem * size ;
	*new = falloc (n_elem, size) ;
	ptr = *new ;
    while (n-- > 0)
		*ptr++ = *old++;
}

#include <stdio.h>
getrecord(record)
	char **record ;
{
	static int bufsize = 0 ;
	static char *buffer = NULL ;
	char *malloc() ;
	int length ;
	int num ;
	int nextchar ;
	char buf[5] ;

/* Remove undocumented ^ characters in the file */
	while('^' == (nextchar = getchar()))
		if (nextchar == EOF)
		{
			*record = NULL ;
			return(-1) ;
		}
	ungetc(nextchar, stdin) ;

	fread(buf, 1, 4, stdin) ;
	num = sscanf(buf, "%4d", &length ) ;
	if (num == EOF || num == 0)
	{
		*record = NULL ;
		return(-1) ;
	}

	if (bufsize < length) 
	{
		if (bufsize != 0)
			free(buffer) ;
		if (NULL == (buffer = malloc(length)))
		{
			fprintf(stderr,"Insufficient memory in getrecord: %d\n", length) ;
			exit(-1) ;
		}
		bufsize = length ;
	}

	fread(buffer, 1, length-4, stdin) ;
	buffer[length-4] = NULL ;

	*record = buffer ;
	return(length-4) ;
}

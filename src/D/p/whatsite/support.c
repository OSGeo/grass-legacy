#include <stdio.h>

remove_new_line(buf) char *buf ;
{
	for( ; *buf ; buf++) ;
	if (*(--buf) == '\n')
		*buf = 0 ;
}

strip (buf)	char *buf;
{
	char *a, *b;

	for (a = b = buf; *b == ' '; b++)
		;

	if (a != b)
		while (*a++ = *b++)
			;

	for (a = 0, b = buf; *b; b++)
		if (*b != ' ')
			a = b;

	if (a++)
		*a = 0;
}

usage (pgm)	char *pgm;
{
	fprintf(stderr,"%s: illegal usage\n", pgm);
	fprintf(stderr,"\t%s format_file data_file\n", pgm);
}

#define NULL	0
#define TAB	011
parse_fields(buf, nfields, fields)
	char *buf ;
	int nfields ;
	char *fields[] ;
{
	int n ;

	fields[0] = buf ;
	buf++ ;
	n = 1 ;

	for(; n<nfields; buf++)
	{
		if (*buf == NULL)
			return(n) ;
		if (*buf == TAB)
		{
			*buf = NULL ;
			buf++ ;
			if (*buf == NULL)
				return(n) ;
			fields[n++] = buf ;
		}
	}
	return(n) ;
}

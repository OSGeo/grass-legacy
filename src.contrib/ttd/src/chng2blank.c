#include <stdio.h>
change(record, length, from, to)
	char *record ;
	int length ;
	char from , to ;
{
	char *rec ;
	rec = record ;

	while(length--)
	{
		if (*rec == NULL)
			return ;
		if(*rec == from)
			*rec = to ;
		rec++ ;
	}
}

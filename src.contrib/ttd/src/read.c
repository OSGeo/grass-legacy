#include <stdio.h>
main()
{
	char *record ;
	int length ;
	while(-1 != (length = getrecord(&record)))
	{
			change(record, length, 037, 040)  ;
			fwrite(record, 1, length, stdout) ;
			printf("\n") ;
	}
}

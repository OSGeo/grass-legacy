#include <stdio.h>
main()
{
	char *record ;
	int length ;
	int type ;
	CC_u2ll_spheroid ("wgs72") ;
	while(-1 != (length = getrecord(&record)))
	{
		if(strncmp(record, "****", 4) == 0)
			continue ;
		if(strncmp(record, "0001", 4) == 0)
			continue ;
		if (strncmp(record, "DSIG", 4) == 0)
		{
			read_header_record(record) ;
			digit_header() ;
		}
		else if (strncmp(record, "EDGI", 4) == 0)
		{
			read_EDGE(record, length) ;
			convert_EDGE(record, length) ;
			digit_EDGE(record, length) ;
		}
		else if (strncmp(record, "NODI", 4) == 0)
		{
			break ;
		}
		else if (strncmp(record, "FACI", 4) == 0)
		{
			break ;
		}
		else if (strncmp(record, "FCIN", 4) == 0)
		{
			break ;
		}
		else
		{
			printf("Unknown record:\n%s\n", record) ;
			exit(-1) ;
		}
	}
}

#include <stdio.h>
main()
{
	char *record ;
	int length ;
	int type ;
	while(-1 != (length = getrecord(&record)))
	{
		if(strncmp(record, "****", 4) == 0)
			continue ;
		if(strncmp(record, "0001", 4) == 0)
			continue ;
		if (strncmp(record, "DSIG", 4) == 0)
		{
			read_header_record(record) ;
		}
		else if (strncmp(record, "EDGI", 4) == 0)
		{
			read_EDGE(record, length) ;
		}
		else if (strncmp(record, "NODI", 4) == 0)
		{
			read_NODI(record, length) ;
		}
		else if (strncmp(record, "FACI", 4) == 0)
		{
			read_FACI(record, length) ;
		}
		else if (strncmp(record, "FCIN", 4) == 0)
		{
			read_FCIN(record, length) ;
		}
		else
		{
			printf("Unknown record:\n%s\n", record) ;
			exit(-1) ;
		}
	}
}

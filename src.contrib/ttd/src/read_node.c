#include <stdio.h>
static int record_type ;
static int record_id ;
static int containing_face ;
static int x, y, z ;
static int point_count ;
static int *point_list ;
static int n_points = 0 ;

read_NODI(record, length)
	char *record ;
	int length ;
{
	int i ;
	char *ptr ;
	char *new_record ;

	change(record, length, 037, 040)  ;

	i = sscanf(record, "%*9c%d%d%d%d%d%d%d",
		&record_type,
		&record_id,
		&containing_face,
		&x, &y, &z,
		&point_count) ;

	if(i!=7)
	{
		printf("Error parsing NODI record:\n") ;
		printf("%s\n", record) ;
		exit(-1) ;
	}
    
	check_alloc(&n_points, point_count, sizeof(int), &point_list) ;

/* Read IDLS record */
	length = getrecord(&new_record) ;
	if (strncmp(new_record, "IDLS", 4) != 0)
	{
		printf("Expecting IDLS record for NODI data. Got:\n") ;
		printf("%s\n", new_record) ;
		exit(-1) ;
	}

	ptr = new_record+9 ;
	for(i=0; ; i++)
	{
		sscanf(ptr,"%d", &point_list[i]) ;
		if (i<point_count)
			while (*(++ptr) != ' ')
				;
		else
			break ;
	}
#ifdef DEBUG
	write_NODI() ;
#endif
}

write_NODI()
{
	int i, j ;
	printf("NODE %d %d:\n", record_type, record_id) ;
	printf("  Containing-face:%d:\n", containing_face) ;
	printf("  Coordinate:\n") ;
	printf("     x:%d y:%d z:%d\n", x, y, z) ;
	printf("  Point-count:%d:\n", point_count) ;
	printf("  Point ID list:\n       ") ;
	for(i=0, j=0; i<point_count; i++, j++)
	{
		if (j==10)
		{
			printf("\n       ") ;
			j=0 ;
		}
		printf("%d  ", point_list[i]) ;
	}
	printf("\n") ;
}

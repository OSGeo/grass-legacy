#include <stdio.h>
static int record_type ;
static int record_id ;
static int xmin, ymin, zmin ;
static int xmax, ymax, zmax ;
static int area_count ;
static int *area_list ;
static int n_areas = 0 ;

read_FACI(record, length)
	char *record ;
	int length ;
{
	int i ;
	char *new_record ;
	char *ptr ;

/* Read information out of FACI record */
	change(record, length, 037, 040)  ;
	i = sscanf(record, "%*9c%d%d%d%d%d%d%d%d%d",
		&record_type,
		&record_id,
		&xmin, &ymin, &zmin,
		&xmax, &ymax, &zmax,
		&area_count) ;

	if(i!=9)
	{
		printf("Error parsing FACI record:\n") ;
		printf("%s\n", record) ;
		exit(-1) ;
	}
    
#ifdef DEBUG
	write_FACI() ;
#endif
	check_alloc(&n_areas, area_count, sizeof(int), &area_list) ;

/* Read IDLS record */
	length = getrecord(&new_record) ;
	if (strncmp(new_record, "IDLS", 4) != 0)
	{
		printf("Expecting IDLS record for FACI data. Got:\n") ;
		printf("%s\n", new_record) ;
		exit(-1) ;
	}
	change(new_record, length, 037, 040) ;

	ptr = new_record+9 ;
	for(i=0; ; i++)
	{
		sscanf(ptr,"%d", &area_list[i]) ;
		if (i<area_count)
			while (*(++ptr) != ' ')
				;
		else
			break ;
	}
}

write_FACI()
{
	int i, j ;
	printf("FACE %d %d:\n", record_type, record_id) ;
	printf("  Bounding rectangle:\n") ;
	printf("     xmin:%d ymin:%d zmin:%d\n", xmin, ymin, zmin) ;
	printf("     xmax:%d ymax:%d zmax:%d\n", xmax, ymax, zmax) ;
	printf("  Area-count:%d:\n   Areas:\n", area_count) ;
	printf("     ") ;
	for(i=0, j=0; i<area_count; i++, j++)
	{
		if (j==10)
		{
			printf("\n     ") ;
			j=0 ;
		}
		printf("%d  ", area_list[i]) ;
	}
	printf("\n") ;
}

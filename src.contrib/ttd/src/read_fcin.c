#include <stdio.h>
static int record_type ;
static int record_id ;
static int containing_face ;
static int xmin, ymin, zmin ;
static int xmax, ymax, zmax ;
char feature_code[7] ;
static int attribute_count ;
static int relation_count ;
static char *attribute_list;
static int attribute_length = 0 ;
static char *relations_list;
static int relations_length = 0 ;

read_FCIN(record, length)
	char *record ;
	int length ;
{
	int i ;
	char *new_record ;
	char *malloc(), *realloc() ;

	change(record, length, 037, 040) ;

	i = sscanf(record, "%*9c%d%d%d%d%d%d%d%d %5c%d%d",
		&record_type,
		&record_id,
		&xmin, &ymin, &zmin,
		&xmax, &ymax, &zmax,
		feature_code,
		&attribute_count,
		&relation_count) ;

	if(i!=11)
	{
		printf("Error parsing FCIN (11:%d) record:\n", i) ;
		printf("%s\n", record) ;
		exit(-1) ;
	}
    
#ifdef DEBUG
	write_FCIN() ;
#endif

/* Read ATTL record */
	length = getrecord(&new_record) ;
	if (strncmp(new_record, "ATTL", 4) != 0)
	{
		printf("Expecting ATTL record for FCIN data. Got:\n") ;
		printf("%s\n", new_record) ;
		exit(-1) ;
	}

/*
For now we won't parse this, but here is a sample record:
ATTL  108ARA 3 0.000000 WID 2 0 pua 1 4 pcm 1 11 bpc 1 2 usp 1 1 ph3 1 2 ph2 1 1 PHT 2 7 NAM 4 HARKER HEIGHTS ar1 1 1
*/
	if (length > attribute_length)
	{
		if (attribute_length == 0)
			attribute_list = malloc(length) ;
		else
			attribute_list = realloc(attribute_list, length) ;
		attribute_length = length ;
	}
	strncpy(attribute_list, new_record+9, length-9) ;
	attribute_list[length-9] = NULL ;

/* Read RELH record */
	length = getrecord(&new_record) ;
	if (strncmp(new_record, "RELH", 4) != 0)
	{
		printf("Expecting RELH record for FCIN data. Got:\n") ;
		printf("%s\n", new_record) ;
		exit(-1) ;
	}
/* For now we won't parse this, but here is a sample record.
RELH    9221 1 0 3
*/
	if (length > relations_length)
	{
		if (relations_length == 0)
			relations_list = malloc(length) ;
		else
			relations_list = realloc(relations_list, length) ;
		relations_length = length ;
	}
	strncpy(relations_list, new_record+9, length-9) ;
	relations_list[length-9] = NULL ;
}

write_FCIN()
{
	printf("FCIN %d %d:\n", record_type, record_id) ;
	printf("  Containing-face:%d:\n", containing_face) ;
	printf("  Bounding-rectangle:\n") ;
	printf("     xmin:%d ymin:%d zmin:%d\n", xmin, ymin, zmin) ;
	printf("     xmax:%d ymax:%d zmax:%d\n", xmax, ymax, zmax) ;
	printf("  Feature-code   : %s\n", feature_code) ;
	printf("  Attribute-count:%d\n", attribute_count) ;
	/*
	change(attribute_list, strlen(attribute_list), 037, 040) ;
	*/
	printf("   %s\n", attribute_list) ;
	printf("  Relation-count: %d\n", relation_count) ;
	/*
	change(relations_list, strlen(relations_list), 037, '~') ;
	*/
	printf("   %s\n", relations_list) ;
}

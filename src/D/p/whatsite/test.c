/*
add_new_record(db_name, table_name, record, n_fields)
	char *db_name ;
	char *table_name ;
	char **record ;
	int n_fields ;

get_field_list(db_name, table_name, list, n_fields)
	char *db_name ;
	char *table_name ;
	char ***list ;
	int *n_fields ;

get_num_fields(db_name, table_name)
	char *db_name ;
	char *table_name ;

make_new_field(db_name, table_name, field_name)
	char *db_name ;
	char *table_name ;
	char *field_name ;

open_table(db_name, table_name, request)
	char *db_name ;
	char *table_name ;
	char *request ;

get_table_entry(table, fields)
	int table ;
	char ***fields ;

remove_field(db_name, table_name, field_name)
	char *db_name ;
	char *table_name ;
	char *field_name ;

remove_record(db_name, table_name, record_num)
	char *db_name ;
	char *table_name ;
	int record_num ;

remove_table(db_name, table_name)
	char *db_name ;
	char *table_name;
*/

#include <stdio.h>
main()
{
	int n_fields ;
	char *list[128] ;
	int i ;
	int err ;
	char *record[20] ;
	int table ;

	remove_table(".", "t1") ;
	make_table(".", "t1") ;

	make_new_field(".", "t1", "field one") ;
	make_new_field(".", "t1", "field two") ;
	make_new_field(".", "t1", "field three") ;
	make_new_field(".", "t1", "field four") ;

	get_field_list(".", "t1", list, &n_fields) ;
	for(i=0; i<n_fields; i++)
		fprintf(stderr,"%s\n", list[i]) ;

	record[0] = "rec1-1" ;
	record[1] = "rec1-2" ;
	record[2] = "rec1-3" ;
	record[3] = "rec1-4" ;
	err = add_new_record(".", "t1", record, n_fields) ;
		printf("err=%d\n", err) ;

	record[0] = "rec2-1" ;
	record[1] = "rec2-2" ;
	record[2] = "rec2-3" ;
	record[3] = "rec2-4" ;
	err = add_new_record(".", "t1", record, n_fields) ;
		printf("err=%d\n", err) ;

	if ((table = open_table(".", "t1", 0)) < 0)
	{
		printf("%d error in opening table\n", table) ;
		exit(-1) ;
	}

	while(0 < get_table_entry(table, record))
		for(i=0; i<n_fields; i++)
			printf("F%d: %s\n", i, record[i]) ;

	close_table(table) ;
}

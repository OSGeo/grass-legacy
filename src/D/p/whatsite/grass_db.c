#include <stdio.h>
#define MAXLEN	1024
#define MAXFIELDS	128

static char buffer[MAXLEN] ;
static char *bufptr[MAXFIELDS] ;
static char field_list[MAXFIELDS][128] ;

/*
Add a new record to a table.
0 is returned if successful, -1 otherwise.
*/
add_new_record(db_name, table_name, record, n_fields)
	char *db_name ;
	char *table_name ;
	char *record[] ;
	int n_fields ;
{
	FILE *fp ;
	FILE *fopen() ;
	char file[128] ;
	char *fgets() ;
	int i ;

	if (n_fields != get_num_fields(db_name, table_name))
		return(-1) ;

	sprintf(file, "%s/%s", db_name, table_name) ;
	if (NULL == (fp = fopen(file, "a")))
		return(-2) ;
	
	fprintf(fp, "%s", record[0]) ;
	for(i=1; i<n_fields; i++)
		fprintf(fp, "	%s", record[i]) ;
	fprintf(fp, "\n") ;

	fclose(fp) ;

	return(0) ;
}

/*
Get a list of data base tables available in a data base area
0 is returned if successful, -1 otherwise.
*/
get_db_list(db_name, table_list, n_tables)
	char *db_name ;
	char ***table_list ;
	int *n_tables ;
{
}

/*
Get a list of fields in a data base table
0 is returned if successful, -1 otherwise.
*/
get_field_list(db_name, table_name, list, n_fields)
	char *db_name ;
	char *table_name ;
	char *list[] ;
	int *n_fields ;
{
	FILE *fp ;
	FILE *fopen() ;
	char file[128] ;
	char *fgets() ;

	sprintf(file, "%s/.%s", db_name, table_name) ;
	if (NULL == (fp = fopen(file, "r")))
		return(-1) ;

	for(*n_fields=0; fgets(buffer, MAXLEN, fp); (*n_fields)++)
	{
		remove_new_line(buffer) ;
		strcpy(field_list[*n_fields], buffer) ;
		list[*n_fields] = field_list[*n_fields] ;
	}

	fclose(fp) ;

	return ;
}

/*
Return the number of fields in a table
a number is returned if successful, 0 otherwise.
*/
get_num_fields(db_name, table_name)
	char *db_name ;
	char *table_name ;
{
	FILE *fp ;
	FILE *fopen() ;
	char file[128] ;
	char *fgets() ;
	int n_fields ;

	sprintf(file, "%s/.%s", db_name, table_name) ;
	if (NULL == (fp = fopen(file, "r")))
		return(-1) ;

	for(n_fields=0; fgets(buffer, MAXLEN, fp); n_fields++) ;

	fclose(fp) ;

	return(n_fields) ;
}

/*
Make a new field in a table.
0 is returned if successful, -1 otherwise.
*/
make_new_field(db_name, table_name, field_name)
	char *db_name ;
	char *table_name ;
	char *field_name ;
{
	FILE *fp ;
	FILE *fopen() ;
	char file[128] ;

	sprintf(file, "%s/.%s", db_name, table_name) ;
	if (NULL == (fp = fopen(file, "a")))
		return(-1) ;

	fprintf(fp, "%s\n", field_name) ;
	fclose(fp) ;

	return(0) ;
}

/*
Make a table in a data base.
0 is returned if successful, -1 otherwise.
*/
make_table(db_name, table_name)
	char *db_name ;
	char *table_name;
{
	char file[128] ;
	int fd ;

	sprintf(file, "%s/%s", db_name, table_name) ;
	fd = creat(file, 0664) ;
	if (fd < 0)
		return(-1) ;
	close(fd) ;

	sprintf(file, "%s/.%s", db_name, table_name) ;
	fd = creat(file, 0664) ;
	if (fd < 0)
		return(-1) ;
	close(fd) ;
	return(0) ;
}

#define MAX_OPEN	10
static struct data_base
{
	int used ;
	FILE *fptr ;
	int n_fields ;
	char request[512] ;
} data_base[MAX_OPEN] ;

static int inited = 0 ;

/*
Open a listing of records in a table in a data base. If request is NULL all
records will be returned via the read_table_list() routine.  The request string
may contain a set of conditions which must be met for the record to be returned
in subsequent read_table_list() calls.  (Initial implementations of the
library will not provide this option.  Syntax will be defined later.)
A positive integer is returned if successful, -1 otherwise.
*/
open_table(db_name, table_name, request)
	char *db_name ;
	char *table_name ;
	char *request ;
{
	FILE *fopen() ;
	char file[128] ;
	char *fgets() ;
	int i ;

	if (inited)
	{
		for(i=0; i<MAX_OPEN; i++)
			data_base[i].used = 0 ;
		inited = 0 ;
	}

	for(i=0; i< MAX_OPEN; i++)
		if (data_base[i].used == 0)
			break ;
	if (i == MAX_OPEN)
		return(-1) ;
	
	sprintf(file, "%s/%s", db_name, table_name) ;
	if (NULL == (data_base[i].fptr = fopen(file, "r")))
		return(-2) ;
	
	data_base[i].used = 1 ;
	data_base[i].n_fields = get_num_fields(db_name, table_name) ;

	return(i) ;
}

/*
Close an opened table.
*/
close_table(table)
	int table ;
{

	fclose(data_base[table].fptr) ;
	data_base[table].used = 0 ;
}

/*
Reads the fields of the next record in a table.  The table was opened by 
open_table().  -1 is returned on failure, the offset is returned otherwise.
*/
long
get_table_entry(table, fields, offset)
	int table ;
	char *fields[] ;
	long offset ;
{
	int nfields ;
	long ftell() ;

	if (offset)
		fseek(data_base[table].fptr, 0L, 1) ;

	offset = ftell(data_base[table].fptr) ;

	if (NULL == fgets(buffer, MAXLEN, data_base[table].fptr))
		return(-1) ;

	nfields = parse_fields(buffer, data_base[table].n_fields, fields) ;

	return(offset) ;
}

/*
Remove a field in a table.
0 is returned if successful, -1 otherwise.
*/
remove_field(db_name, table_name, field_name)
	char *db_name ;
	char *table_name ;
	char *field_name ;
{
	return(-1) ;
}

/*
Remove a record in a table in a data base.
0 is returned if successful, -1 otherwise.
*/
remove_record(db_name, table_name, record_num)
	char *db_name ;
	char *table_name ;
	int record_num ;
{
	return(-1) ;
}

/*
Remove a table in a data base.
0 is returned if successful, -1 otherwise.
*/
remove_table(db_name, table_name)
	char *db_name ;
	char *table_name;
{
	char file[128] ;

	sprintf(file, "%s/%s", db_name, table_name) ;
	if (-1 == unlink(file))
		return(-1) ;

	sprintf(file, "%s/.%s", db_name, table_name) ;
	if (-1 == unlink(file))
		return(-1) ;
	
	return(0) ;
}

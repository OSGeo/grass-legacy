#include <grass/dbmi.h>
#include <stdio.h>
#include <string.h>
#include <grass/gis.h>
#include "odbc.h"
#include "globals.h"
#include "proto.h"


/* NAME: db_driver_create_table 
 * INPUT:
 * OUTPUT:
 * PROCESSING: issue a CREATE TABLE tableName (column data_type [ DEFAULT
 *   {NULL | CURRENT | USER} ] [ NOT NULL ], ... ) via EXECUTE IMMEDIATE
 */
/*db_driver_create_table (table)
dbTable *table;
{
  dbString cmd;
  $char *sqlcmd;
  dbColumn *col;
  dbValue *defaultValue;
  int i;

  db_init_string(&cmd);
  db_append_string(&cmd, "CREATE TABLE ");
  db_append_string(&cmd, db_get_table_name(table));
  db_append_string(&cmd, " (");
  for (i = 0; i < db_get_table_number_of_columns(table); i++) {
    if (i) db_append_string(&cmd,",");
    col = db_get_table_column(table, i);
    db_append_string(&cmd, db_get_column_name(col));
    db_append_string(&cmd, " ");
    build_column_type_name(col, &cmd);
    build_column_default(col, &cmd);
    if (!db_test_column_null_allowed(col)) {
      db_append_string(&cmd, " NOT NULL ");
    }
  }
  db_append_string(&cmd, ")");

  sqlcmd = db_get_string(&cmd);
  $EXECUTE IMMEDIATE $sqlcmd;
  db_free_string(&cmd);
  if (sql_error(NULL)) {
    return DB_FAILED;
  }
  return DB_OK;
}*/

/*
build_column_type_name(col, cmd)
dbColumn *col;
dbString *cmd;
{
  int sql_type;
  char buf[50];

  sql_type = db_get_column_sqltype(col);
  db_append_string(cmd, db_sqltype_name(sql_type));
*/
  /* append the (precision, scale) or (length) if necessary */
/*  switch (sql_type) {
    case DB_SQL_TYPE_CHARACTER:
	sprintf(buf, "(%d)", db_get_column_length(col));
	db_append_string(cmd, buf);
	break;
    case DB_SQL_TYPE_DECIMAL:
    case DB_SQL_TYPE_NUMERIC:
	sprintf(buf, "(%d,%d)", db_get_column_precision(col),
	db_get_column_scale(col));
	db_append_string(cmd, buf);
	break;
    default: break;
  }
}
*/ 
/*
build_column_default(col, cmd)
dbColumn *col;
dbString *cmd;
{
  dbString defaultString;

  if(db_test_column_has_default_value(col)) {
    db_append_string(cmd, " DEFAULT ");
    db_init_string(&defaultString);
    db_convert_column_default_value_to_string(col, &defaultString);
    if (db_get_column_sqltype(col) == DB_SQL_TYPE_CHARACTER)
      db_append_string(cmd, "\"");
    db_append_string(cmd, db_get_string(&defaultString));
    if (db_get_column_sqltype(col) == DB_SQL_TYPE_CHARACTER)
      db_append_string(cmd, "\"");
    db_free_string(&defaultString);
    db_append_string(cmd, " ");
  }
}*/

int db__driver_drop_table (name)
dbString *name;
{
    char        cmd[200];
    cursor      *c;
    SQLRETURN   ret;
    char        msg[OD_MSG];
    char        *emsg = NULL;    
    SQLINTEGER  err;  
    SQLCHAR     ttype[50], *tname;
    SQLINTEGER  nrow=0;


    /* allocate cursor */
    c = alloc_cursor();
    if (c == NULL)
        return DB_FAILED;
	
    tname = db_get_string(name);

    ret = SQLTables( c->stmt, NULL, 0, NULL, 0, tname, sizeof(tname), NULL, 0 ); 
    if ( ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO )
    {
        report_error("SQLTables()");
        return DB_FAILED;
    }

    /* Get number of rows */
    ret = SQLRowCount(c->stmt, &nrow);
    if ( ret != SQL_SUCCESS && ret != SQL_SUCCESS_WITH_INFO)
    {
        report_error("SQLRowCount()");
        return DB_FAILED;
    } 

    if ( nrow == 0 )
    {
        G_asprintf(&emsg, "Table %s doesn't exist\n", tname);
        report_error(emsg);
        G_free(emsg);

        return DB_FAILED;
    }
    
    ret = SQLFetchScroll( c->stmt, SQL_FETCH_NEXT, 0 );
    ret = SQLGetData( c->stmt, 4, SQL_C_CHAR, ttype, sizeof(ttype), NULL );

    if ( strcmp(ttype, "TABLE") == 0 )
    {
	sprintf(cmd, "DROP TABLE %s", tname);
    }
    else if ( strcmp(ttype, "VIEW") == 0 )
    {
	sprintf(cmd, "DROP VIEW %s", tname);
    }
    else
    {
        G_asprintf(&emsg, "Table %s isn't 'TABLE' or 'VIEW' but %s\n",tname, ttype);
        report_error(emsg);
        G_free(emsg);

        return DB_FAILED;
    }
    
    SQLCloseCursor( c->stmt );
    
    ret = SQLExecDirect(c->stmt,cmd,SQL_NTS);
    if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
    {
        SQLGetDiagRec(SQL_HANDLE_STMT, c->stmt,1, NULL, &err,msg,sizeof(msg),NULL);
        G_asprintf(&emsg, "SQLExecDirect():\n%s\n%s (%d)\n", cmd, msg, (int) err);
        report_error(emsg);
        G_free(emsg);

        return DB_FAILED;
    }
			 
    free_cursor (c);
					     
    return DB_OK; 				
}

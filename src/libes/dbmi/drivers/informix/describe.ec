
#include <stdio.h>
#include <sqlca.h>
#include <sqlda.h>
#include <decimal.h>
#include <dbmi.h>
#include <datetime.h>

$include sqltypes;
#include "globals.h"

extern char *strchr();

/* NAME: db_driver_describe_table
 * INPUT: dbCursor, table name
 * OUTPUT: error code
 * PROCESSING: describe the table, particularly default values and permissions
 *  using DESCRIBE and several SELECT statements
 */
db_driver_describe_table(table_name, table)
dbString *table_name;
dbTable **table;
{
  $int tid;
  $string owner[9];
  $char tbname[50], tbcreator[50];
  char *user_name;
  $char *namebuf;

  if (describe_table(table, table_name) != DB_OK) return DB_FAILED;

  /* get table id */
  namebuf = db_get_string(table_name);
  decompose_tablename(namebuf, tbcreator, tbname);
  $SELECT tabid, owner 
     INTO $tid, $owner 
     FROM systables 
    WHERE tabname = $tbname
      AND owner = $tbcreator;

  /* get defaults for table */
  if (get_defaults(*table, tid) != DB_OK) return DB_FAILED;

  return get_auths(*table, table_name);
}

describe_table(table, table_name)
dbTable **table;
dbString *table_name;
{
  int rc = DB_FAILED;
  int ncols, colno;
  char *tname;
  dbString *colNames;
  dbColumn *column;
  $char *dname = "DTdname";
  $char selstatement[100];

  tname = db_get_string(table_name);

  $ALLOCATE DESCRIPTOR "DTdname" WITH MAX 1;

  if (sql_error(NULL)) return DB_FAILED;

  if (list_columns(tname, &colNames, &ncols) != DB_OK) return DB_FAILED;
  if (ncols <= 0) {
    db_error("Specified table not found");
    return DB_FAILED;
  }

  if (!(*table = db_alloc_table(ncols))) goto end2;

  for (colno = 0; colno < ncols; colno++) {
    column = db_get_table_column(*table, colno);
    sprintf(selstatement,"SELECT %s FROM %s", db_get_string(&colNames[colno]), 
      tname);
    $PREPARE DTsname FROM $selstatement;

    /* if we don't have select privilege on this column, set the name of the */
    /* column, mark the type as unknown, and continue */
    if (sql_error_code() == -272) {
      db_set_column_name(column, db_get_string(&colNames[colno]));
      db_set_column_sqltype(column, DB_SQL_TYPE_UNKNOWN);
      continue;
    }

    $DESCRIBE DTsname USING SQL DESCRIPTOR "DTdname";
    /* the 2nd parameter below, colno, is always 1, since columns are */
    /* handled one at a time */
    describe_column(dname, 1, column);
    if (sql_error(NULL)) goto end1;
  }

  rc = DB_OK;

 end1:
  db_free_string_array(colNames, ncols);
  $FREE DTsname;
 end2:
  $DEALLOCATE DESCRIPTOR $dname;
  return rc;
}

get_defaults(table, tid)
dbTable *table;
$parameter int tid;
{
  $char dtype, dfault[256];
  $char *column_name;
  int i;

  for (i = 0; i < db_get_table_number_of_columns(table); i++) {
    column_name = db_get_column_name(db_get_table_column(table, i));
    $SELECT D.type, D.default
       INTO $dtype, $dfault
       FROM sysdefaults D, syscolumns C
      WHERE C.tabid = $tid
        AND C.colname = $column_name
	AND D.tabid = C.tabid
	AND D.colno = C.colno;
    if (sql_error(NULL)) break;
    if (sql_eof()) continue;
    set_default(db_get_table_column(table, i), dtype, dfault);
  }
  if (sql_error(NULL)) return DB_FAILED;
  return DB_OK;
}


/* NAME: set_default
 * INPUT: dbColumn, type, and default value
 * OUTPUT: error code
 * PROCESSING: set the default value for column, based upon the type and the
 *  default value
 */
set_default(column, type, dfault)
dbColumn *column;
char type;
char *dfault;
{
  dbValue *value;
  int sqltype;

  db_set_column_has_defined_default_value(column);
  value = db_get_column_default_value(column);
  sqltype = db_get_column_sqltype(column);
  switch (type) {
    case 'L':
      if (sqltype == DB_SQL_TYPE_CHARACTER)
        db_convert_Cstring_to_value(dfault, sqltype, value);
      else
        db_convert_Cstring_to_value(strchr(dfault, ' '), sqltype, value);
      break;
    case 'U':
      db_set_value_string(value, db_whoami());
      break;
    case 'C':
      db_set_value_datetime_current(value);
      break;
    case 'N':
      db_set_value_null(value);
      break;
    case 'T':
      db_set_value_datetime_current(value);
      break;
    default:
      db_set_column_has_undefined_default_value(column);
      break;
  }
  return DB_OK;
}

static int
get_auths(table, table_name)
dbTable *table;
dbString *table_name;
{
  $char *cur = "authcur";
  $string t_owner[9];
  $int t_id;
  $char *t_name;
  $string t_authstring[9];
  int col, ncols;
  int rc;
  dbColumn *column;
  int table_owner = 0;
  $char tbcreator[50], tbname[50];

  t_name = db_get_string(table_name);
  decompose_tablename(t_name, tbcreator, tbname);

  /* get table level privileges */
  $SELECT owner, tabid 
     INTO $t_owner, $t_id 
     FROM systables 
    WHERE tabname = $tbname
      AND owner = $tbcreator;

  /* if I am the owner of the table */
  if (db_nocase_compare(t_owner, db_whoami())) {
    /* I can do anything to the table I want to */
    db_set_table_delete_priv_granted(table);
    db_set_table_insert_priv_granted(table);
    table_owner++;
  }
  /* otherwise, check if I have been granted privileges */
  else {
    $DECLARE $cur cursor for
     SELECT tabauth 
     FROM   systabauth 
     WHERE  tabid = $t_id 
       AND  (grantee = USER OR grantee = "public");
    if (sql_error(NULL)) {
      return DB_FAILED;
    }

    $OPEN $cur;
    if (sql_error(NULL)) {
      $FREE $cur;
      return DB_FAILED;
    }

    /* default to no privileges */
    db_set_table_delete_priv_not_granted(table);
    db_set_table_insert_priv_not_granted(table);

    $FETCH $cur INTO $t_authstring;
    /* if privileges have been granted */
    while (!sql_eof()) {
      if (sql_error(NULL)) {
        $CLOSE $cur;
        $FREE $cur;
        return DB_FAILED;
      }
      /* determine what privileges there are */
      if (strchr(t_authstring, 'd') || strchr(t_authstring, 'D'))
        db_set_table_delete_priv_granted(table);
      if (strchr(t_authstring, 'i') || strchr(t_authstring, 'I'))
        db_set_table_insert_priv_granted(table);
      $FETCH $cur INTO $t_authstring;
    }
  }

  /* now set the privileges for all the columns */
  ncols = db_get_table_number_of_columns(table);
  rc = DB_OK;
  for (col = 0; col < ncols; col++) {
    column = db_get_table_column(table, col);
    if (table_owner) {
      db_set_column_select_priv_granted(column); 
      db_set_column_update_priv_granted(column);
    } 
    else {
      if ((rc = get_column_auths(t_id, column)) != DB_OK)
        break;
    }
  }

  $CLOSE $cur;
  $FREE $cur;
  return rc;
}

/* NAME: get_column_auths
 * INPUT: table id, and dbColumn
 * OUTPUT: error code
 * PROCESSING: set the column privileges for the column in the given table,
 * assuming that that no access is allowed by default
 */
get_column_auths(t_id, column)
$parameter int t_id;
dbColumn *column;
{
  $char *c_name;
  $string c_authstring[4];
  $int c_num;
  $char *cur = "colauthcur";

  /* default to no column privileges */
  db_set_column_select_priv_not_granted(column);
  db_set_column_update_priv_not_granted(column);

  /* get the column number for accessing syscolauth from syscolumns */
  c_name = db_get_column_name(column);
  $SELECT colno 
   INTO   $c_num 
   FROM   syscolumns 
   WHERE  tabid = $t_id 
     AND  colname = $c_name;

  if (sql_error(NULL)) return DB_FAILED;

  /* get the privileges, if any, for this user from syscolauth */
  $DECLARE $cur CURSOR FOR
   SELECT colauth 
   FROM   syscolauth 
   WHERE  tabid = $t_id 
     AND  colno = $c_num 
     AND  (grantee = USER OR grantee = "public");
  if (sql_error(NULL))
    return DB_FAILED;

  $OPEN $cur;
  if (sql_error(NULL)) {
    $FREE $cur;
    return DB_FAILED;
  }

  $FETCH $cur INTO $c_authstring;

  /* set the privileges granted */
  while (!sql_eof()) {
    if (sql_error(NULL)) {
      $CLOSE $cur;
      $FREE $cur;
      return DB_FAILED;
    }
    if (strchr(c_authstring, 's') ||strchr(c_authstring, 'S'))
      db_set_column_select_priv_granted(column); 
    if (strchr(c_authstring, 'u') ||strchr(c_authstring, 'U'))
      db_set_column_update_priv_granted(column);
    $FETCH $cur INTO $c_authstring;
  }

  $CLOSE $cur;
  $FREE $cur;
  return DB_OK;
}

int
list_columns(tableName, tlist, tcount)
char *tableName;
dbString **tlist;
int *tcount;
{
  int rc = DB_FAILED;
  int col;
  $int ncols, colno;
  $char *cur = "LCcur";
  $char tbname[256];
  $char tbcreator[9];
  $char columnName[19];

  *tcount = 0;
  *tlist = NULL;

  decompose_tablename(tableName, tbcreator, tbname);

  $SELECT COUNT(*)
     INTO $ncols
     FROM systables T, syscolumns C
    WHERE owner = $tbcreator
      AND tabname = $tbname
      AND T.tabid = C.tabid;

  if(sql_error(NULL) || ncols <= 0) return DB_FAILED;

  *tcount = ncols;

  if (!(*tlist = db_alloc_string_array(ncols))) return DB_FAILED;

  $DECLARE $cur CURSOR FOR
   SELECT C.colname, C.colno
     FROM systables T, syscolumns C
    WHERE owner = $tbcreator
      AND tabname = $tbname
      AND T.tabid = C.tabid
   ORDER BY C.colno;
  $OPEN $cur;

  if (sql_error(NULL)) {
    db_free_string_array(tlist, ncols);
    goto end;
  }

  for (col = 0; col < ncols; col++) {
    $FETCH $cur INTO $columnName, $colno;
    if (sql_error(NULL) || sql_eof() ||
        db_set_string(&((*tlist)[col]), columnName)) {
      db_free_string_array(tlist, ncols);
      goto end;
    }
  }

  rc = DB_OK;
    
 end:
  $CLOSE $cur;
  return rc;
}

#include <dbmi.h>

char *get_index_name();
/* NAME: db_create_index
 * INPUT: an index structure specifying the index name, table name, and the 
 *  column names
 * OUTPUT: a status code
 * PROCESSING: an SQL statement like: CREATE [ UNIQUE ] INDEX index_name ON
 *  table_name ( column_name [ , column_name ...] )
 */
db_driver_create_index (index)
dbIndex *index;
{
  dbString cmd;
  char *name;
  int status = DB_OK, i;
  $char *sqlcmd;

  name = get_index_name(index);
  if(name == NULL) return DB_FAILED;

  db_init_string(&cmd);
  db_append_string(&cmd, "CREATE ");
  if (db_test_index_type_unique(index))
    db_append_string(&cmd, "UNIQUE ");
  db_append_string(&cmd, "INDEX ");
  db_append_string(&cmd, name);
  db_append_string(&cmd, " ON ");
  db_append_string(&cmd, db_get_index_table_name(index));
  db_append_string(&cmd, " (");
  for (i = 0; i<db_get_index_number_of_columns(index); i++) {
    if (i) db_append_string(&cmd, ", ");
    db_append_string(&cmd, db_get_index_column_name(index, i));
  }
  db_append_string(&cmd, ")");
  sqlcmd = db_get_string(&cmd);
  $EXECUTE IMMEDIATE $sqlcmd;
  if (sql_error(name))
    status = DB_FAILED;
  db_free_string(&cmd);
  return status;
}

/* NAME: db_driver_drop_index
 * INPUT: an index structure giving the index name and table name of the index
 *  to be dropped
 * OUTPUT:
 * PROCESSING: an SQL statement something like: DROP INDEX name
 */
db_driver_drop_index(name)
dbString *name;
{
  $char *sqlcmd;
  dbString cmd;
  int status = DB_OK;

  db_init_string(&cmd);
  db_append_string(&cmd, "DROP INDEX ");
  db_append_string(&cmd, db_get_string(name));
  sqlcmd = db_get_string(&cmd);
  $EXECUTE IMMEDIATE $sqlcmd;
  if (sql_error(NULL))
    status = DB_FAILED;
  db_free_string(&cmd);
  return status;
}

/* NAME:
 * INPUT:
 * OUTPUT:
 * PROCESSING:
 */
db_driver_list_indexes (tableName, indexes, count)
dbString *tableName;
dbIndex **indexes;
int *count;
{
  int i, j;
  $char *cur = "ucurs";
  $char *tabName;
  $char colNames[8][19], idxname[19], idxtype[2];
  $char tbname[50], tbcreator[50];
  $int sCount;

  tabName = db_get_string(tableName);
  decompose_tablename(tabName, tbcreator, tbname);
  $SELECT COUNT(*)
     INTO $sCount
     FROM sysindexes I, systables T
    WHERE T.tabname = $tbname
      AND T.owner = $tbcreator
      AND I.tabid = T.tabid;



  *count = sCount;
  if(!(*indexes = db_alloc_index_array(*count))) {
    db_error("couldn't allocate index array");
    return DB_FAILED;
  }

  $DECLARE $cur CURSOR FOR
   SELECT I.idxname, I.idxtype,
	  C1.colname, C2.colname, C3.colname, C4.colname,
	  C5.colname, C6.colname, C7.colname, C8.colname
     FROM sysindexes I, systables T,
	  OUTER syscolumns C1, OUTER syscolumns C2,
	  OUTER syscolumns C3, OUTER syscolumns C4,
	  OUTER syscolumns C5, OUTER syscolumns C6,
	  OUTER syscolumns C7, OUTER syscolumns C8
    WHERE T.tabname = $tbname
      AND T.owner = $tbcreator
      AND I.tabid = T.tabid
      AND T.tabid = C1.tabid AND C1.colno = I.part1
      AND T.tabid = C2.tabid AND C2.colno = I.part2
      AND T.tabid = C3.tabid AND C3.colno = I.part3
      AND T.tabid = C4.tabid AND C4.colno = I.part4
      AND T.tabid = C5.tabid AND C5.colno = I.part5
      AND T.tabid = C6.tabid AND C6.colno = I.part6
      AND T.tabid = C7.tabid AND C7.colno = I.part7
      AND T.tabid = C8.tabid AND C8.colno = I.part8;
  if (sql_error(NULL)) {
    db_free_index_array(*indexes, *count);
    return DB_FAILED;
  }
  $OPEN $cur;
  if (sql_error(NULL)) {
    db_free_index_array(*indexes, *count);
    $FREE $cur;
    return DB_FAILED;
  }

  for (i = 0; i < *count; i++) {
    $FETCH $cur INTO $idxname, $idxtype, $colNames[0], $colNames[1], 
		     $colNames[2], $colNames[3], $colNames[4], $colNames[5], 
		     $colNames[6], $colNames[7];
    if (sql_error(NULL)) {
      db_free_index_array(*indexes, *count);
      $CLOSE $cur;
      $FREE $cur;
      return DB_FAILED;
    }
    db_set_index_name(&((*indexes)[i]), idxname);
    db_set_index_table_name(&((*indexes)[i]),  tabName);
    idxtype[0] == 'U' ? db_set_index_type_unique(&((*indexes)[i]))
                      : db_set_index_type_non_unique(&((*indexes)[i]));
    for (j = 0; j < 8; j++)
      if(!colNames[j][0]) break;
    db_alloc_index_columns(&((*indexes)[i]), j);
    for (j = 0; j < 8; j++) {
      if(colNames[j][0])
        db_set_index_column_name(&((*indexes)[i]), j, colNames[j]);
      else
        break;
    }
  }
  $CLOSE $cur;
  $FREE $cur;
  return DB_OK;
}

/* create a new index name. To do this make up sequential names
 * and see if they already exist in the sysindexes table
 * NULL is returned if there is an sql error
 */
char *
make_index_name()
{
    $int count;
    $char *idxname;
    int i;
    static char name[20];

    idxname = name;
    i = 0;
    do
    {
	sprintf (name, "idx_%d", ++i);
	$SELECT COUNT(*) INTO $count FROM sysindexes WHERE sysindexes.idxname = $idxname;
	if(sql_error(NULL)) return NULL;
    }while(count>0);
    return name;
}

char *
get_index_name(index)
    dbIndex *index;
{
    char dummy[2];
    char *name;

    name = db_get_index_name(index);
    if (sscanf(name, "%1s", dummy) == 1)
	    return name;
    return make_index_name();
}

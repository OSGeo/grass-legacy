#include <dbmi.h>
#include "globals.h"

/* NAME: db_driver_add_constraint 
 * INPUT: a dbConstraint specifying the constraint
 * OUTPUT: an added constraint to the database and a status code to the client
 * PROCESSING: add a constraint to the database using an SQL statement like:
 *  ALTER TABLE table_name ADD CONSTRAINT 
 *    {  UNIQUE ( column_list )
 *     | PRIMARY KEY ( column_list )
 *     | FOREIGN KEY ( column_list ) REFERENCES reftable_name 
 *       ( ref_column_list ) } CONSTRAINT constraint_name
 */
db_driver_add_constraint (constraint)
dbConstraint *constraint;
{
  $char *sqlcmd;
  dbString cmd;
  int status = DB_OK, i;

  db_init_string(&cmd);
  db_append_string(&cmd, "ALTER TABLE ");
  db_append_string(&cmd, db_get_constraint_table_name(constraint));
  switch (constraint->type) {
    case DB_UNIQUE:
      db_append_string(&cmd, " ADD CONSTRAINT UNIQUE (");
      for (i = 0; i < db_get_constraint_number_of_columns(constraint); i++) {
	if (i) db_append_string(&cmd, ",");
        db_append_string(&cmd, 
			 db_get_constraint_column_name(constraint, i));
      }
      db_append_string(&cmd, ") ");
      break;
    case DB_PRIMARY_KEY:
      db_append_string(&cmd, " ADD CONSTRAINT PRIMARY KEY (");
      for (i = 0; i < db_get_constraint_number_of_columns(constraint); i++) {
	if (i) db_append_string(&cmd, ",");
        db_append_string(&cmd, 
			 db_get_constraint_column_name(constraint, i));
      }
      db_append_string(&cmd, ") ");
      break;
    case DB_FOREIGN_KEY:
      db_append_string(&cmd, " ADD CONSTRAINT FOREIGN KEY (");
      for (i = 0; i < db_get_constraint_number_of_columns(constraint); i++) {
	if (i) db_append_string(&cmd, ",");
        db_append_string(&cmd, 
			 db_get_constraint_column_name(constraint, i));
      }
      db_append_string(&cmd, ") REFERENCES ");
      db_append_string(&cmd, db_get_constraint_reftable_name(constraint));
      db_append_string(&cmd, " (");
      for (i = 0; i < db_get_constraint_number_of_columns(constraint); i++) {
	if (i) db_append_string(&cmd, ",");
        db_append_string(&cmd, 
			 db_get_constraint_refcolumn_name(constraint, i));
      }
      db_append_string(&cmd, ") ");
      break;
    default:
      db_error("unknown constraint type");
      db_free_string(&cmd);
      status = DB_FAILED;
  }
  if (status == DB_OK) {
    if (*db_get_constraint_name(constraint))
    {
	db_append_string(&cmd, " CONSTRAINT ");
	db_append_string(&cmd, db_get_constraint_name(constraint));
    }
    sqlcmd = db_get_string(&cmd);
    $EXECUTE IMMEDIATE $sqlcmd;
    if (sql_error(NULL)) status = DB_FAILED;
  }
  db_free_string(&cmd);
  return status;
}

/* NAME: db_driver_drop_constraint
 * INPUT:
 * OUTPUT:
 * PROCESSING: drop a constraint using an SQL statement something like:
 *  ALTER TABLE table_name DROP CONSTRAINT constraint_name
 */
db_driver_drop_constraint(constraint)
dbConstraint *constraint;
{
  dbString cmd;
  int status = DB_OK;
  $char *sqlcmd;

  db_init_string(&cmd);
  db_append_string(&cmd, "ALTER TABLE ");
  db_append_string(&cmd, db_get_constraint_table_name(constraint));
  db_append_string(&cmd, " DROP CONSTRAINT ");
  db_append_string(&cmd, db_get_constraint_name(constraint));
  sqlcmd = db_get_string(&cmd);
  $EXECUTE IMMEDIATE $sqlcmd;
  if (sql_error(NULL)) status = DB_FAILED;
  db_free_string(&cmd);
  return status;
    
}

/* NAME: db_driver_list_constraints
 * INPUT: the type of constraints to list
 * OUTPUT: the constraints found, a count of how many there were, and a status
 *  code
 * PROCESSING: complicated. issue an SQL statement to find how many constraints
 *  there are.  Allocate an array of constraints of that size. Read them into
 *  the array.  A complicated SQL statement is also needed on sysindexes to
 *  determine the exact columns the constraint is on.
 */
db_driver_list_constraints(type, constraints, constraint_count)
int type;
dbConstraint **constraints;
int *constraint_count;
{
  $char *constraintString;
  $int count;

  switch(type) {
    case DB_UNIQUE:
      constraintString = "U";
      break;
    case DB_PRIMARY_KEY:
      constraintString = "P";
      break;
    case DB_FOREIGN_KEY:
      constraintString = "R";
      break;
    default:
      db_error("unknown constraint type");
      return DB_FAILED;
  }
  $SELECT COUNT(*)
   INTO   $count
   FROM   sysconstraints C, systables T
   WHERE  C.constrtype = $constraintString
   AND    T.tabid = C.tabid
   AND    C.owner = USER
   AND    C.tabid > 99;
  if (sql_error(NULL)) return DB_FAILED;
  *constraint_count = count;

  if(!(*constraints = db_alloc_constraint_array(*constraint_count))) {
    db_error("couldn't allocate constraint array");
    return DB_FAILED;
  }

  switch(type) {
    case DB_UNIQUE:
      return list_unique_constraints(*constraints, *constraint_count);
      break;
    case DB_PRIMARY_KEY:
      return list_primary_constraints(*constraints, *constraint_count);
      break;
    case DB_FOREIGN_KEY:
      return list_referential_constraints(*constraints, *constraint_count);
      break;
  }
  return DB_OK;
}

set_constraint_columns(constraint, idxname, tabid, type)
dbConstraint *constraint;
$parameter char *idxname;
$parameter int tabid;
int type;
{
  $char colNames[8][19];
  int i;

  $SELECT C1.colname, C2.colname, C3.colname, C4.colname,
	  C5.colname, C6.colname, C7.colname, C8.colname
     INTO $colNames[0], $colNames[1], $colNames[2], $colNames[3], 
          $colNames[4], $colNames[5], $colNames[6], $colNames[7] 
     FROM sysindexes I, OUTER syscolumns C1, OUTER syscolumns C2,
	  OUTER syscolumns C3, OUTER syscolumns C4,
	  OUTER syscolumns C5, OUTER syscolumns C6,
	  OUTER syscolumns C7, OUTER syscolumns C8
    WHERE I.idxname = $idxname
      AND I.tabid = $tabid
      AND I.owner = USER
      AND C1.tabid = $tabid AND C1.colno = I.part1
      AND C2.tabid = $tabid AND C2.colno = I.part2
      AND C3.tabid = $tabid AND C3.colno = I.part3
      AND C4.tabid = $tabid AND C4.colno = I.part4
      AND C5.tabid = $tabid AND C5.colno = I.part5
      AND C6.tabid = $tabid AND C6.colno = I.part6
      AND C7.tabid = $tabid AND C7.colno = I.part7
      AND C8.tabid = $tabid AND C8.colno = I.part8;
  
  if (sql_error(NULL)) return DB_FAILED;

  for (i = 0; i < 8; i++)
    if(!colNames[i][0]) break;
  db_alloc_constraint_columns(constraint, i, type);
  for (i = 0; i < 8; i++) {
    if(colNames[i][0])
      db_set_constraint_column_name(constraint, i, colNames[i]);
    else
      break;
  }
  return DB_OK;
}

set_constraint_refcolumns(constraint, idxname, tabid)
dbConstraint *constraint;
$parameter char *idxname;
$parameter int tabid;
{
  $char colNames[8][19];
  int i;

  $SELECT C1.colname, C2.colname, C3.colname, C4.colname,
	  C5.colname, C6.colname, C7.colname, C8.colname
     INTO $colNames[0], $colNames[1], $colNames[2], $colNames[3], 
          $colNames[4], $colNames[5], $colNames[6], $colNames[7] 
     FROM sysindexes I, OUTER syscolumns C1, OUTER syscolumns C2,
	  OUTER syscolumns C3, OUTER syscolumns C4,
	  OUTER syscolumns C5, OUTER syscolumns C6,
	  OUTER syscolumns C7, OUTER syscolumns C8
    WHERE I.idxname = $idxname
      AND I.tabid = $tabid
      AND I.owner = USER
      AND C1.tabid = $tabid AND C1.colno = I.part1
      AND C2.tabid = $tabid AND C2.colno = I.part2
      AND C3.tabid = $tabid AND C3.colno = I.part3
      AND C4.tabid = $tabid AND C4.colno = I.part4
      AND C5.tabid = $tabid AND C5.colno = I.part5
      AND C6.tabid = $tabid AND C6.colno = I.part6
      AND C7.tabid = $tabid AND C7.colno = I.part7
      AND C8.tabid = $tabid AND C8.colno = I.part8;
  
  if (sql_error(NULL)) return DB_FAILED;

  for (i = 0; i < 8; i++)
    if(!colNames[i][0]) break;
  for (i = 0; i < 8; i++) {
    if(colNames[i][0])
      db_set_constraint_refcolumn_name(constraint, i, colNames[i]);
    else
      break;
  }
  return DB_OK;
}

static int
list_unique_constraints(constraints, count)
dbConstraint *constraints;
int count;
{
  $char *cur = "uniqcur";
  $char constrname[19], tabname[19], idxname[19], owner[9];
  $int tabid;
  int status = DB_OK;
  int i;

  $DECLARE $cur CURSOR FOR 
  SELECT C.constrname, T.tabname, C.idxname, C.owner, C.tabid
    FROM sysconstraints C, systables T
   WHERE C.constrtype = 'U'
     AND T.tabid = C.tabid
     AND C.owner = USER
     AND C.tabid > 99;

  if (sql_error(NULL))
    return DB_FAILED;
  $OPEN $cur;
  if (sql_error(NULL)) {
    $FREE $cur;
    return DB_FAILED;
  }
  for (i = 0; i < count; i++)
  {
    $FETCH $cur INTO $constrname, $tabname, $idxname, $owner, $tabid;
    if (sql_eof())
      break;
    if (sql_error(NULL)) {
      status = DB_FAILED;
      break;
    }
    db_init_constraint(&constraints[i]);
    constraints[i].type = DB_UNIQUE;
    db_set_constraint_name(&constraints[i], constrname);
    db_set_constraint_table_name(&constraints[i], tabname);
    /* Informix SE doesn't support actions */
    db_set_constraint_delete_action(&constraints[i], DB_NOACTION);
    db_set_constraint_update_action(&constraints[i], DB_NOACTION);
    if ((status = set_constraint_columns(&constraints[i], idxname, tabid, 
					 DB_UNIQUE)) != DB_OK)
      break;;
  }
  $CLOSE $cur;
  $FREE $cur;
  return status;
}

static int
list_primary_constraints(constraints, count)
dbConstraint *constraints;
int count;
{
  $char *cur = "uniqcur";
  $char constrname[19], tabname[19], idxname[19], owner[9];
  $int tabid;
  int status = DB_OK;
  int i;

  $DECLARE $cur CURSOR FOR 
  SELECT C.constrname, T.tabname, C.idxname, C.owner, C.tabid
    FROM sysconstraints C, systables T
   WHERE C.constrtype = 'P'
     AND T.tabid = C.tabid
     AND C.owner = USER
     AND C.tabid > 99;

  if (sql_error(NULL))
    return DB_FAILED;
  $OPEN $cur;
  if (sql_error(NULL)) {
    $FREE $cur;
    return DB_FAILED;
  }
  for (i = 0; i < count; i++)
  {
    $FETCH $cur INTO $constrname, $tabname, $idxname, $owner, $tabid;
    if (sql_eof())
      break;
    if (sql_error(NULL)) {
      status = DB_FAILED;
      break;
    }
    db_init_constraint(&constraints[i]);
    constraints[i].type = DB_PRIMARY_KEY;
    db_set_constraint_name(&constraints[i], constrname);
    db_set_constraint_table_name(&constraints[i], tabname);
    /* Informix SE doesn't support actions */
    db_set_constraint_delete_action(&constraints[i], DB_NOACTION);
    db_set_constraint_update_action(&constraints[i], DB_NOACTION);
    if((status = set_constraint_columns(&constraints[i], idxname, tabid, 
				    DB_PRIMARY_KEY)) != DB_OK)
      break;
  }
  $CLOSE $cur;
  $FREE $cur;
  return status;
}

static int
list_referential_constraints(constraints, count)
dbConstraint *constraints;
int count;
{
  $char *cur = "uniqcur";
  $char constrname[19], tabname1[19], tabname2[19], idxname1[19], idxname2[19],
	owner[9];
  $int tabid1, tabid2;
  $int tabid;
  int status;
  int i;

  $DECLARE $cur CURSOR FOR 
   SELECT C1.constrname, C1.idxname, C2.idxname, T1.tabname, T2.tabname, 
	  T1.tabid, T2.tabid
     FROM sysconstraints C1, sysconstraints C2, systables T1,
	  systables T2, sysreferences R
    WHERE C1.owner = USER
      AND R.constrid = C1.constrid
      AND R.primary = C2.constrid
      AND T1.tabid = C1.tabid
      AND T2.tabid = C2.tabid;

  if (sql_error(NULL))
    return DB_FAILED;
  $OPEN $cur;
  if (sql_error(NULL)) {
    $FREE $cur;
    return DB_FAILED;
  }
  for (i = 0; i < count; i++)
  {
    $FETCH $cur INTO $constrname, $idxname1, $idxname2, $tabname1, $tabname2, 
		     $tabid1, $tabid2;
    if (sql_eof())
      break;
    if (sql_error(NULL)) {
      status = DB_FAILED;
      break;
    }
    db_init_constraint(&constraints[i]);
    constraints[i].type = DB_FOREIGN_KEY;
    db_set_constraint_name(&constraints[i], constrname);
    db_set_constraint_table_name(&constraints[i], tabname1);
    db_set_constraint_reftable_name(&constraints[i], tabname2);
    /* Informix SE doesn't support actions */
    db_set_constraint_delete_action(&constraints[i], DB_NOACTION);
    db_set_constraint_update_action(&constraints[i], DB_NOACTION);
    if ((status = set_constraint_columns(&constraints[i], idxname1, tabid1, 
					 DB_FOREIGN_KEY)) != DB_OK)
      break;
    if ((status = set_constraint_refcolumns(&constraints[i], idxname2, tabid2))
       != DB_OK)
      break;
  }
  $CLOSE $cur;
  $FREE $cur;
  return status;
}

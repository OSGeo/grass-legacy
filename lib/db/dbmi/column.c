#include "dbmi.h"

dbColumn *
db_get_table_column (table, n)
    dbTable *table;
    int n;
{
    if (n < 0 || n >= table->numColumns)
	return ((dbColumn *)NULL);
    return &table->columns[n];
}

dbValue *
db_get_column_value (column)
    dbColumn *column;
{
    return &column->value;
}

dbValue *
db_get_column_default_value (column)
    dbColumn *column;
{
    return &column->defaultValue;
}

void
db_set_column_sqltype (column, sqltype)
    dbColumn *column;
    int sqltype;
{
    column->sqlDataType = sqltype;
}

void
db_set_column_host_type (column, type)
    dbColumn *column;
    int type;
{
    column->hostDataType = type;
}

int
db_get_column_scale (column)
    dbColumn *column;
{
    return column->scale;
}

void
db_set_column_scale (column, scale)
    dbColumn *column;
    int scale;
{
    column->scale = scale;
}

int
db_get_column_precision (column)
    dbColumn *column;
{
    return column->precision;
}

void
db_set_column_precision (column, precision)
    dbColumn *column;
    int precision;
{
    column->precision = precision;
}

int
db_get_column_sqltype (column)
    dbColumn *column;
{
    return column->sqlDataType;
}

int
db_get_column_host_type (column)
    dbColumn *column;
{
    return column->hostDataType;
}

void
db_set_column_has_defined_default_value(column)
    dbColumn *column;
{
    column->hasDefaultValue = DB_DEFINED;
}

void
db_set_column_has_undefined_default_value(column)
    dbColumn *column;
{
    column->hasDefaultValue = DB_UNDEFINED;
}

void
db_unset_column_has_default_value(column)
    dbColumn *column;
{
    column->hasDefaultValue = 0;
}

int
db_test_column_has_default_value(column)
    dbColumn *column;
{
    return (column->hasDefaultValue != 0);
}

int
db_test_column_has_defined_default_value(column)
    dbColumn *column;
{
    return (column->hasDefaultValue == DB_DEFINED);
}

int
db_test_column_has_undefined_default_value(column)
    dbColumn *column;
{
    return (column->hasDefaultValue == DB_UNDEFINED);
}

void
db_set_column_use_default_value(column)
    dbColumn *column;
{
    column->useDefaultValue = 1;
}

void
db_unset_column_use_default_value(column)
    dbColumn *column;
{
    column->useDefaultValue = 0;
}

int
db_test_column_use_default_value(column)
    dbColumn *column;
{
    return (column->useDefaultValue != 0);
}

void
db_set_column_null_allowed(column)
    dbColumn *column;
{
    column->nullAllowed = 1;
}

void
db_unset_column_null_allowed(column)
    dbColumn *column;
{
    column->nullAllowed = 0;
}

int
db_test_column_null_allowed(column)
    dbColumn *column;
{
    return (column->nullAllowed != 0);
}

int
db_get_column_length (column)
    dbColumn *column;
{
    return column->dataLen;
}

void
db_set_column_length (column, length)
    dbColumn *column;
    int length;
{
    column->dataLen = length;
}

void
db_set_column_select_priv_granted (column)
    dbColumn *column;
{
    column->select = DB_GRANTED;
}

void
db_set_column_select_priv_not_granted (column)
    dbColumn *column;
{
    column->select = DB_NOT_GRANTED;
}

db_get_column_select_priv (column)
    dbColumn *column;
{
    return column->select;
}

void
db_set_column_update_priv_granted (column)
    dbColumn *column;
{
    column->update = DB_GRANTED;
}

void
db_set_column_update_priv_not_granted (column)
    dbColumn *column;
{
    column->update = DB_NOT_GRANTED;
}

db_get_column_update_priv (column)
    dbColumn *column;
{
    return column->update;
}

void
db_init_column (column)
    dbColumn *column;
{
    db_zero ((void *)column, sizeof(dbColumn));
    db_init_string (&column->columnName);
    db_init_string (&column->description);
    db_init_string (&column->value.s);
    db_init_string (&column->defaultValue.s);
}

int
db_set_column_name (column, name)
    dbColumn *column;
    char *name;
{
    return db_set_string (&column->columnName, name);
}

char *
db_get_column_name (column)
    dbColumn *column;
{
    return db_get_string (&column->columnName);
}

int
db_set_column_description (column, description)
    dbColumn *column;
    char *description;
{
    return db_set_string (&column->description, description);
}

char *
db_get_column_description (column)
    dbColumn *column;
{
    return db_get_string (&column->description);
}

void
db_free_column (column)
    dbColumn *column;
{
    db_free_string (&column->columnName);
    db_free_string (&column->value.s);
}

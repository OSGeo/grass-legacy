#include "gis.h"
#include "dbmi.h"
#include "glocale.h"

int
print_table_definition(table)
    dbTable *table;
{
    int ncols, col;
    dbColumn *column;

    G_message ( _("table:%s\n"), db_get_table_name(table));
    G_message ( _("description:%s\n"), db_get_table_description(table));
    print_priv ("insert", db_get_table_insert_priv(table));
    print_priv ("delete", db_get_table_delete_priv(table));

    ncols = db_get_table_number_of_columns(table);
    G_message ( _("ncols:%d\n"), ncols);
    for (col = 0; col < ncols; col++)
    {
	column = db_get_table_column (table, col);
	G_message ( _("\n"));
	print_column_definition (column);
    }
    
    return 0;
}

int
print_column_definition(column)
    dbColumn *column;
{
    dbString value_string;

    G_message ( _("column:%s\n"), db_get_column_name(column));
    G_message ( _("description:%s\n"), db_get_column_description(column));
    G_message ( _("type:%s\n"), db_sqltype_name(db_get_column_sqltype(column)));
    G_message ( _("len:%d\n"), db_get_column_length(column));
    G_message ( _("scale:%d\n"), db_get_column_scale(column));
    G_message ( _("precision:%d\n"), db_get_column_precision(column));
    G_message ( _("default:"));
    if (db_test_column_has_default_value(column))
    {
      db_init_string(&value_string);
      db_convert_column_default_value_to_string (column, &value_string);
      G_message ( _("%s"), db_get_string(&value_string));
    }
    G_message ("\n");
    G_message ( _("nullok:%s\n"), db_test_column_null_allowed(column) ? "yes" : "no");
    print_priv ("select", db_get_column_select_priv(column));
    print_priv ("update", db_get_column_update_priv(column));
    
    return 0;
}

int
print_priv (label, priv)
    char *label;
{
    G_message ("%s:", label);
    switch (priv)
    {
    case DB_GRANTED:     G_message ( _("yes")); break;
    case DB_NOT_GRANTED: G_message ( _("no")); break;
    default:             G_message ( _("?")); break;
    }
    G_message ("\n");
    
    return 0;
}

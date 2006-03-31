#include <grass/dbmi.h>
#include "global.h"

static int get_field_cat(struct Map_info *, char *, int *, int *);

static char **field_names = NULL;
static int num_fields = 0, *field_cat = NULL;
static struct field_info **fi = NULL;
static dbDriver *driver = NULL;
static dbString sql;
static char buf[1000];

void write_vect(struct Map_info *Map, char *layer_name, int arr_size, int type)
{
    struct line_cats *Cats;
    int field, cat;

    /* copy xyzpnts to Points */
    Vect_copy_xyz_to_pnts(Points, xpnts, ypnts, zpnts, arr_size);

    /* set field and cat numbers */
    Cats = Vect_new_cats_struct();
    if (!flag_table) {
	int i;

	i = get_field_cat(Map, layer_name, &field, &cat);
	db_init_string(&sql);
	sprintf(buf, "insert into %s (%s) values (%d)", fi[i]->table,
		fi[i]->key, cat);
	db_set_string(&sql, buf);
	if (db_execute_immediate(driver, &sql) != DB_OK)
	    G_fatal_error(_("Cannot execute: %s"), db_get_string(&sql));
	db_free_string(&sql);
    }
    else
	get_field_cat(Map, layer_name, &field, &cat);
    Vect_cat_set(Cats, field, cat);

    /* write */
    Vect_write_line(Map, type, Points, Cats);

    Vect_destroy_cats_struct(Cats);

    return;
}

void write_done(struct Map_info *Map)
{
    int i;

    if (!flag_table) {
	db_commit_transaction(driver);
	db_close_database_shutdown_driver(driver);
    }

    Vect_build(Map, stderr);

    fprintf(stderr, _("\nFollowing DXF layers found:\n"));
    for (i = 0; i < num_fields; i++) {
	fprintf(stderr, _("Layer %d %s\n"), i + 1, field_names[i]);
	G_free(field_names[i]);
	if (!flag_table) {
	    /* no function to do this? */
	    G_free(fi[i]->name);
	    G_free(fi[i]->table);
	    G_free(fi[i]->key);
	    G_free(fi[i]->database);
	    G_free(fi[i]->driver);
	    G_free(fi[i]);
	}
    }
    G_free(field_names);
    G_free(field_cat);

    num_fields = 0;
    field_names = NULL;
    field_cat = NULL;

    if (!flag_table) {
	G_free(fi);
	fi = NULL;
	driver = NULL;
    }

    return;
}

static int get_field_cat(struct Map_info *Map, char *field_name, int *field,
			 int *cat)
{
    int i;

    for (i = 0; i < num_fields; i++) {
	/* field name already exists */
	if (strcmp(field_name, field_names[i]) == 0) {
	    *field = i + 1;
	    *cat = ++field_cat[i];
	    return i;
	}
    }

    num_fields++;

    /* create new field */
    field_names = (char **)G_realloc(field_names, (i + 1) * sizeof(char *));
    field_names[i] = G_store(field_name);
    field_cat = (int *)G_realloc(field_cat, (i + 1) * sizeof(int));

    /* assign field and cat numbers */
    *field = i + 1;
    *cat = field_cat[i] = 1;

    if (flag_table)
	return i;

    /* create a table */
    fi = (struct field_info **)G_realloc(fi,
					 (i + 1) * sizeof(struct field_info *));

    fi[i] = Vect_default_field_info(Map, *field, field_name, GV_MTABLE);
    Vect_map_add_dblink(Map, *field, field_name, fi[i]->table, "cat",
			fi[i]->database, fi[i]->driver);

    if (!driver) {
	driver =
	    db_start_driver_open_database(fi[i]->driver,
					  Vect_subst_var(fi[i]->database, Map));
	if (!driver)
	    G_fatal_error(_("Cannot open database %s by driver %s"),
			  Vect_subst_var(fi[i]->database, Map), fi[i]->driver);

	db_begin_transaction(driver);
    }

    db_init_string(&sql);
    sprintf(buf, "create table %s (cat integer)", fi[i]->table);
    db_set_string(&sql, buf);

    if (db_execute_immediate(driver, &sql) != DB_OK)
	G_fatal_error(_("Cannot create table: %s"), db_get_string(&sql));
    db_free_string(&sql);

    if (db_grant_on_table
	(driver, fi[i]->table, DB_PRIV_SELECT, DB_GROUP | DB_PUBLIC) != DB_OK)
	G_fatal_error(_("Cannot grant privileges on table %s"), fi[i]->table);
    if (db_create_index2(driver, fi[i]->table, fi[i]->key) != DB_OK)
	G_warning(_("Cannot create index"));

    return i;
}

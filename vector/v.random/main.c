
/****************************************************************
 *
 * MODULE:       v.random (based on s.rand)
 *
 * AUTHOR(S):    James Darrell McCauley darrell@mccauley-usa.com
 * 	         http://mccauley-usa.com/
 *
 * PURPOSE:      Randomly generate a 2D/3D GRASS vector points map.
 *
 * COPYRIGHT:    (C) 2003-2007, 2010 by the GRASS Development Team
 *
 *               This program is free software under the
 *               GNU General Public License (>=v2).
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
**************************************************************/
/*
 * Modification History:
 *
 * s.rand v 0.5B <25 Jun 1995> Copyright (c) 1993-1995. James Darrell McCauley
 * <?? ??? 1993> - began coding and released test version (jdm)
 * <10 Jan 1994> - changed RAND_MAX for rand(), since it is different for
 *                 SunOS 4.1.x and Solaris 2.3. stdlib.h in the latter defines
 *                 RAND_MAX, but it doesn't in the former. v0.2B (jdm)
 * <02 Jan 1995> - clean Gmakefile, man page. added html v0.3B (jdm)
 * <25 Feb 1995> - cleaned 'gcc -Wall' warnings (jdm)
 * <25 Jun 1995> - new site API (jdm)
 * <13 Sep 2000> - released under GPL
 */

#include <stdlib.h>
#include <math.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/dbmi.h>
#include <grass/glocale.h>

#ifndef RAND_MAX
#define RAND_MAX (pow(2.0,31.0)-1)
#endif
double myrand(void);

#if defined(__CYGWIN__) || defined(__APPLE__) || defined(__MINGW32__)
double drand48()
{
    return (rand() / 32767.0);
}

#define srand48(sv) (srand((unsigned)(sv)))
#endif

int main(int argc, char *argv[])
{
    char *output, buf[2000];
    double (*rng) ();
    double max, zmin, zmax;
    int i, n, b, type, usefloat;
    struct Map_info Out;
    struct line_pnts *Points;
    struct line_cats *Cats;
    struct Cell_head window;
    struct GModule *module;
    struct
    {
	struct Option *output, *nsites, *zmin, *zmax, *zcol;
    } parm;
    struct
    {
	struct Flag *rand, *drand48, *z, *notopo;
    } flag;
    struct field_info *Fi;
    dbDriver *driver;
    dbTable *table;
    dbString sql;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("vector, statistics");
    module->description = _("Randomly generate a 2D/3D vector points map.");

    parm.output = G_define_standard_option(G_OPT_V_OUTPUT);

    parm.nsites = G_define_option();
    parm.nsites->key = "n";
    parm.nsites->type = TYPE_INTEGER;
    parm.nsites->required = YES;
    parm.nsites->description = _("Number of points to be created");

    parm.zmin = G_define_option();
    parm.zmin->key = "zmin";
    parm.zmin->type = TYPE_DOUBLE;
    parm.zmin->required = NO;
    parm.zmin->description =
	_("Minimum z height (needs -z flag or column name)");
    parm.zmin->answer = "0.0";
    parm.zmin->guisection = _("3D output");

    parm.zmax = G_define_option();
    parm.zmax->key = "zmax";
    parm.zmax->type = TYPE_DOUBLE;
    parm.zmax->required = NO;
    parm.zmax->description =
	_("Maximum z height (needs -z flag or column name)");
    parm.zmax->answer = "0.0";
    parm.zmax->guisection = _("3D output");
    
    parm.zcol = G_define_option();
    parm.zcol->key = "column";
    parm.zcol->type = TYPE_STRING;
    parm.zcol->multiple = NO;
    parm.zcol->required = NO;
    parm.zcol->label =
	_("Column name and type (i.e. INTEGER, DOUBLE PRECISION) for z values");
    parm.zcol->description =
	_("If type is not given then DOUBLE PRECISION is used. "
	  "Writes Z data to column instead of 3D vector.");
    parm.zcol->guisection = _("3D output");

    flag.z = G_define_flag();
    flag.z->key = 'z';
    flag.z->description = _("Create 3D output");
    flag.z->guisection = _("3D output");

    flag.drand48 = G_define_flag();
    flag.drand48->key = 'd';
    flag.drand48->description = _("Use drand48() function instead of rand()");

    flag.notopo = G_define_flag();
    flag.notopo->key = 'b';
    flag.notopo->description = _("Do not build topology");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    if (flag.z->answer && parm.zcol->answer) {
	G_fatal_error(_("v.random can't create 3D vector and attribute table at same time"));
    }

    output = parm.output->answer;
    n = atoi(parm.nsites->answer);
    b = (flag.drand48->answer == '\0') ? 0 : 1;

    if (n <= 0) {
	G_fatal_error(_("Number of points must be > 0 (%d given)"), n);
    }

    if (flag.z->answer)
	Vect_open_new(&Out, output, WITH_Z);
    else
	Vect_open_new(&Out, output, WITHOUT_Z);

    /* Do we need to write random values into attribute table? */
    if (parm.zcol->answer) {
	char **token = G_tokenize(parm.zcol->answer, " ");
	
	Fi = Vect_default_field_info(&Out, 1, NULL, GV_1TABLE);
	driver =
	    db_start_driver_open_database(Fi->driver,
					  Vect_subst_var(Fi->database, &Out));
	if (driver == NULL) {
	    Vect_delete(parm.output->answer);
	    G_fatal_error(_("Unable to open database <%s> by driver <%s>"),
			  Vect_subst_var(Fi->database, &Out), Fi->driver);
	}
	db_begin_transaction(driver);

	db_init_string(&sql);
	if (G_number_of_tokens(token) > 1) {
	    sprintf(buf, "create table %s (cat integer, %s)", Fi->table,
		    parm.zcol->answer);
	}
	else {
	    G_verbose_message(_("Using 'double precision' for column <%s>"), parm.zcol->answer);
	    sprintf(buf, "create table %s (cat integer, %s double precision)", Fi->table,
		    parm.zcol->answer);
	}
	db_set_string(&sql, buf);
	Vect_map_add_dblink(&Out, 1, NULL, Fi->table, "cat", Fi->database,
			    Fi->driver);

	/* Create table */
	G_debug(3, db_get_string(&sql));
	if (db_execute_immediate(driver, &sql) != DB_OK) {
	    db_close_database(driver);
	    db_shutdown_driver(driver);
	    Vect_delete(parm.output->answer);
	    G_fatal_error(_("Unable to create table: %s"),
			  db_get_string(&sql));
	}

	/* Grant */
	if (db_grant_on_table
	    (driver, Fi->table, DB_PRIV_SELECT,
	     DB_GROUP | DB_PUBLIC) != DB_OK) {
	    db_close_database(driver);
	    db_shutdown_driver(driver);
	    Vect_delete(parm.output->answer);
	    G_fatal_error(_("Unable to grant privileges on table <%s>"),
			  Fi->table);
	}

	/* OK. Let's check what type of column user has created */
	db_set_string(&sql, Fi->table);
	if (db_describe_table(driver, &sql, &table) != DB_OK) {
	    db_close_database(driver);
	    db_shutdown_driver(driver);
	    Vect_delete(parm.output->answer);
	    G_fatal_error(_("Unable to describe table <%s>"), Fi->table);
	}

	if (db_get_table_number_of_columns(table) != 2) {
	    db_close_database(driver);
	    db_shutdown_driver(driver);
	    Vect_delete(parm.output->answer);
	    G_fatal_error(_("Table should contain only two columns"));
	}

	type = db_get_column_sqltype(db_get_table_column(table, 1));
	usefloat = -1;
	if (type == DB_SQL_TYPE_SMALLINT || type == DB_SQL_TYPE_INTEGER)
	    usefloat = 0;
	if (type == DB_SQL_TYPE_REAL || type == DB_SQL_TYPE_DOUBLE_PRECISION)
	    usefloat = 1;
	if (usefloat < 0) {
	    db_close_database(driver);
	    db_shutdown_driver(driver);
	    Vect_delete(parm.output->answer);
	    G_fatal_error(_("You have created unsupported column type. This module supports only INTEGER"
			   " and DOUBLE PRECISION column types."));
	}
	G_free_tokens(token);
    }

    Vect_hist_command(&Out);

    if (b) {
	rng = drand48;
	max = 1.0;
	srand48((long)getpid());
    }
    else {			/* default is rand() */

	rng = myrand;
	max = RAND_MAX;
	srand(getpid());
    }

    G_get_window(&window);

    if (flag.z->answer || parm.zcol->answer) {
	zmax = atof(parm.zmax->answer);
	zmin = atof(parm.zmin->answer);
    }

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    G_message(_("Generating points..."));
    for (i = 0; i < n; ++i) {
	double x, y, z;

	G_percent(i, n, 5);

	Vect_reset_line(Points);
	Vect_reset_cats(Cats);

	x = rng() / max * (window.west - window.east) + window.east;
	y = rng() / max * (window.north - window.south) + window.south;

	if (flag.z->answer) {
	    z = rng() / max * (zmax - zmin) + zmin;
	    Vect_append_point(Points, x, y, z);
	}
	else
	    Vect_append_point(Points, x, y, 0.0);

	if (parm.zcol->answer) {
	    z = rng() / max * (zmax - zmin) + zmin;

	    sprintf(buf, "insert into %s values ( %d, ", Fi->table, i + 1);
	    db_set_string(&sql, buf);
	    /* Round random value if column is integer type */
	    if (usefloat)
		sprintf(buf, "%f )", z);
	    else
		sprintf(buf, "%.0f )", z);
	    db_append_string(&sql, buf);

	    G_debug(3, db_get_string(&sql));
	    if (db_execute_immediate(driver, &sql) != DB_OK) {
		db_close_database(driver);
		db_shutdown_driver(driver);
		Vect_delete(parm.output->answer);
		G_fatal_error(_("Cannot insert new row: %s"),
			      db_get_string(&sql));
	    }
	}

	Vect_cat_set(Cats, 1, i + 1);
	Vect_write_line(&Out, GV_POINT, Points, Cats);
    }

    if (parm.zcol->answer) {
	db_commit_transaction(driver);
	db_close_database_shutdown_driver(driver);
    }

    if (!flag.notopo->answer) {
	Vect_build(&Out);
    }
    Vect_close(&Out);

    exit(EXIT_SUCCESS);
}

double myrand()
{
    return (double)rand();
}

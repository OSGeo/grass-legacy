#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "dbmi.h"

#define KEY(x) (G_strcasecmp(key,x)==0)

int inpt (FILE *rulefd, char *buf);
int key_data (char *buf, char **k, char **d);
int reclass ( struct Map_info *In, struct Map_info *Out, int type, int field, dbCatValArray *cvarr, int optiond);

int 
main (int argc, char *argv[])
{
    struct GModule *module;
 /*    struct Flag *d_flag; */
    struct Option *in_opt, *out_opt, *type_opt, *field_opt, *rules_opt, *col_opt;
    char   *mapset, *key, *data, buf[1024];
    int    rclelem, type, field;
    struct Map_info In, Out;

    struct field_info *Fi;
    dbDriver *Driver;
    dbCatValArray cvarr;
	    
    G_gisinit (argv[0]);

    module = G_define_module();
    module->description = "Changes vector category values for an existing vector map "
	    "according to results of SQL queries.";

    /* TODO: Dissolve common boundaries */
    /*
    d_flag = G_define_flag();
    d_flag->key              = 'd';
    d_flag->description      = "Dissolve common boundaries (default is no) ";
    */

    in_opt = G_define_standard_option(G_OPT_V_INPUT);

    out_opt =  G_define_standard_option(G_OPT_V_OUTPUT);

    rules_opt = G_define_option();
    rules_opt->key = "rules";
    rules_opt->required = NO;
    rules_opt->type = TYPE_STRING;
    rules_opt->description =  "Full path to the reclass rule file";

    col_opt = G_define_option();
    col_opt->key            = "col";
    col_opt->type           = TYPE_STRING;
    col_opt->required       = NO;
    col_opt->multiple       = NO;
    col_opt->description    = "The name of the column values of which are used as new categories. "
	                      "The column must be type integer.";
    
    type_opt = G_define_standard_option(G_OPT_V_TYPE);

    field_opt = G_define_standard_option(G_OPT_V_FIELD);

    if (G_parser(argc, argv)) exit(1);

    type = Vect_option_to_types ( type_opt );
    field = atoi (field_opt->answer);
    
    if ( (!(rules_opt->answer) && !(col_opt->answer)) || 
	 (rules_opt->answer && col_opt->answer) ) { 
	G_fatal_error ( "Either 'rules' or 'col' must be specified.");
    }

    Vect_check_input_output_name ( in_opt->answer, out_opt->answer, GV_FATAL_EXIT );
    
    mapset = G_find_vector2 (in_opt->answer, NULL);
    if(mapset == NULL) G_fatal_error ("Could not find input %s\n", in_opt->answer);
    Vect_set_open_level ( 2 );
    Vect_open_old (&In, in_opt->answer, mapset);
    
    Vect_open_new ( &Out, out_opt->answer, Vect_is_3d (&In) );
    Vect_copy_head_data (&In, &Out);
    Vect_hist_copy (&In, &Out);
    Vect_hist_command ( &Out );

    /* Read column values from database */
    db_CatValArray_init ( &cvarr );

    Fi = Vect_get_field( &In, field);
    if ( Fi == NULL )
	G_fatal_error ("Cannot read field info");

    Driver = db_start_driver_open_database ( Fi->driver, Fi->database );
    if (Driver == NULL)
	G_fatal_error("Cannot open database %s by driver %s", Fi->database, Fi->driver);

    if ( col_opt->answer ) {
        int nrec;

	nrec = db_select_CatValArray ( Driver, Fi->table, Fi->key, col_opt->answer, NULL, &cvarr );
	G_debug (3, "nrec = %d", nrec );

	if ( cvarr.ctype != DB_C_TYPE_INT )
	    G_fatal_error ( "Column type must be integer." );

    } else {
	int cat;
        char *label, *where;
        FILE *rulefd;

	G_debug (2, "Reading rules");
	
	if ( (rulefd = fopen(rules_opt->answer, "r")) == NULL )
	    G_fatal_error ("Unable to open rule file %s", rules_opt->answer);

	db_CatValArray_alloc ( &cvarr, Vect_get_num_lines(&In) );
	
	cat = 0;
	where = label = NULL;
	while ( inpt(rulefd, buf) ) {
	    if (!key_data(buf, &key, &data)) continue ;

	    G_strip(data);
	    G_debug (3, "key = %s data = %s", key, data);
	    
	    if (KEY("cat")) {	
		if ( cat > 0 ) G_fatal_error ( "Category %d overwritten by %s", cat, data);
		cat = atoi ( data );
		if( cat <= 0 ) G_fatal_error ( "Category '%s' invalid", data);
	    } else if (KEY("label")) {
		if ( label ) G_fatal_error ( "Label '%s' overwritten by '%s'", label, data);
		label = G_store ( data );
	    } else if (KEY("where")) {	
		if ( where ) G_fatal_error ( "Condition '%s' overwritten by '%s'", where, data);
		where = G_store(data);
	    } else {
		G_fatal_error ( "Unknown rule option: '%s'", key );
	    }

	    if ( cat > 0 && where ) {
		int i, over, *cats, ncats;
		dbCatVal *catval;
		
		G_debug (2, "cat = %d, where = '%s'", cat, where);
		if ( !label ) label = where;

		ncats = db_select_int ( Driver, Fi->table, Fi->key, where, &cats);
		if ( ncats == -1 ) G_fatal_error("Cannot select values from database.");
		G_debug (3, "  ncats = %d", ncats);
		

		/* If the category already exists, overwrite it cvarr, set to 0 in cats
		 * and don't add second time */
		over = 0;
		for ( i = 0; i < ncats; i++ ) {
		    if ( db_CatValArray_get_value ( &cvarr, cats[i], &catval ) == DB_OK ) {
			catval->val.i = cat;
			cats[i] = 0;
			over++;
		    }
		}
		if (over > 0) 
		    G_warning ("%d previously set categories overwritten by new category %d.", over, cat);
			
		for ( i = 0; i < ncats; i++ ) {
		    if ( cats[i] <= 0 ) continue;

		    if ( cvarr.n_values == cvarr.alloc ) {
			db_CatValArray_realloc ( &cvarr, (int)10+cvarr.alloc/3 );
		    }
		    G_debug (3, "Add old cat %d", cats[i]);
		    cvarr.value[cvarr.n_values].cat = cats[i];
		    cvarr.value[cvarr.n_values].val.i = cat;
		    cvarr.n_values++;
		}
		
		db_CatValArray_sort ( &cvarr );

		free ( cats );
		cat = 0;
		where = label = NULL;
	    }
	}

	if ( cat > 0 || where )
	    G_fatal_error ( "Incomplete rule");
    }

    db_close_database_shutdown_driver(Driver);
    
    /* reclass vector map */    
    rclelem = reclass ( &In, &Out, type, field, &cvarr, 0);
    
    Vect_close (&In);

    Vect_build (&Out, stderr);
    Vect_close (&Out);

    fprintf(stderr,"%d elments written\n", rclelem);

    exit(0);
}


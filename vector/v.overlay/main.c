/* ****************************************************************************
 *
 *  MODULE: v.overlay 
 *
 *  AUTHOR(S): Radim Blazek
 *  
 ******************************************************************************/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"

/* 
 * Maps may be big, don't left them open if not necessary :
 *  - open output
 *  - open ainput, copy to output, close ainput
 *  - open binput, copy to output, close binput
 *  - build output, find centroids and store them in array, close output
 *  - open ainput, find areas, close ainput
 *  - open binput, find areas, close binput
 *  - opent output, write centroids, build, close
 */

#define OP_AND 0
#define OP_OR  1
#define OP_NOT 2
#define OP_XOR 3

typedef struct {
    double x, y;
    int cat[2]; /* category in map a and b */
    char valid; 
} CENTR; 

int 
main (int argc, char *argv[])
{
    int    ret, input, line, nlines, area, nareas, operator;
    int    in_area, in_centr, in_cat;
    int    field[2];
    char   *mapset[2];
    char   *pre[0], *sep;
    struct GModule *module;
    struct Option *in_opt[2], *out_opt, *type_opt[2], *field_opt[2], *operator_opt;
    struct Flag *table_flag;
    struct Map_info In[2], Out;
    int    type;
    struct line_pnts *Points;
    struct line_cats *Cats;
    CENTR  *Centr;
    char    *Del;

    struct   field_info *Fi;
    char     buf[1000];
    dbString stmt;
    dbDriver *driver;

    G_gisinit (argv[0]);

    pre[0] ="a";
    pre[1] ="b";
    sep = "---------------------------------------------------------------------\n";

    module = G_define_module();
    module->description = "Overlay 2 vector maps.";

    in_opt[0] = G_define_standard_option(G_OPT_V_INPUT);
    in_opt[0]->key = "ainput";

    in_opt[1] = G_define_standard_option(G_OPT_V_INPUT);
    in_opt[1]->key = "binput";

    type_opt[0] = G_define_standard_option(G_OPT_V_TYPE) ;
    type_opt[0]->key = "atype";
    type_opt[0]->options = "area";
    type_opt[0]->answer = "area";

    type_opt[1] = G_define_standard_option(G_OPT_V_TYPE) ;
    type_opt[1]->key = "btype";
    type_opt[1]->options = "area";
    type_opt[1]->answer = "area";

    field_opt[0] = G_define_standard_option(G_OPT_V_FIELD);
    field_opt[0]->key = "afield";
    
    field_opt[1] = G_define_standard_option(G_OPT_V_FIELD);
    field_opt[1]->key = "bfield";
    
    out_opt = G_define_standard_option(G_OPT_V_OUTPUT);

    operator_opt = G_define_option();
    operator_opt->key = "operator";
    operator_opt->type = TYPE_STRING;
    operator_opt->required = NO;
    operator_opt->multiple = NO;
    operator_opt->options = "and,or,not,xor";
    operator_opt->answer = "or";
    operator_opt->description = "Operator defines features written to output vector. "
	"Feature is written to output if the result of operation 'ainput operator binput' is true. "
	"Input feature is considered to be true, if category of given field is defined.\n"
	"\t and : also known as 'union' in GIS\n"
	"\t or  : also known as 'intersection' in GIS\n"
	"\t not : features from ainput not overlayed by features from binput\n"
	"\t xor : features from either ainput or binput but not those from ainput overlayed by binput";

    table_flag = G_define_flag ();
    table_flag->key             = 't';
    table_flag->description     = "Do not create attribute table.";

    if (G_parser (argc, argv)) exit(-1);

    if ( operator_opt->answer[0] == 'a' ) operator = OP_AND;
    else if ( operator_opt->answer[0] == 'o' ) operator = OP_OR;
    else if ( operator_opt->answer[0] == 'n' ) operator = OP_NOT;
    else if ( operator_opt->answer[0] == 'x' ) operator = OP_XOR;
    
    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    /* Open output */
    Vect_open_new (&Out, out_opt->answer, 0);
    Vect_set_map_name ( &Out, "Output from v.overlay");
    Vect_set_person ( &Out, G_whoami ());
    Vect_hist_command ( &Out );
    
    /* Copy lines to output */
    for ( input = 0; input < 2; input++ ) {
	fprintf (stderr, "Copying %sinput boundaries ... ", pre[input]);

	if ((mapset[input] = G_find_vector2 (in_opt[input]->answer, NULL)) == NULL) {
	    G_fatal_error ("Could not find vector '%s'\n", in_opt[input]->answer);
	}

	Vect_set_open_level (2);
	Vect_open_old (&(In[input]), in_opt[input]->answer, mapset[input] );

	nlines = Vect_get_num_lines ( &(In[input]) );

	for ( line = 1; line <= nlines; line++ ) {
	    G_percent ( line, nlines, 1 ); /* must be before any continue */
	    
	    type = Vect_read_line ( &(In[input]), Points, Cats, line);
	    if ( !(type & GV_BOUNDARY) ) continue;

	    Vect_write_line ( &Out, type, Points, Cats );
	}
	Vect_close ( &(In[input]) ); 
        fprintf (stderr, sep );
    }

    fprintf ( stderr, "Buiding partial topology ...\n" );
    /* do not print output, because befor cleaning it is nonsense */
    Vect_build_partial ( &Out, GV_BUILD_BASE, NULL ); 

    fprintf (stderr, sep );
    fprintf ( stderr, "Breaking boundaries ...\n" );
    Vect_break_lines ( &Out, GV_BOUNDARY, NULL, stderr );
    
    fprintf (stderr, sep );
    fprintf ( stderr, "Removing duplicates ...\n" );
    Vect_remove_duplicates ( &Out, GV_BOUNDARY, NULL, stderr );

    /* ?: May be result of Vect_break_lines() + Vect_remove_duplicates() any dangle or bridge?
     * In that case, calls to Vect_remove_dangles() and Vect_remove_bridges() would be also necessary */
    
    /* Attach islands */
    fprintf (stderr, sep );
    fprintf ( stderr, "Attaching islands ...\n" );
    Vect_build_partial ( &Out, GV_BUILD_ATTACH_ISLES, stderr );

    /* Calculate new centroids for all areas */
    nareas = Vect_get_num_areas ( &Out );

    Centr = (CENTR *) G_malloc ( (nareas+1) * sizeof ( CENTR ) ); /* index from 1 ! */
    for ( area = 1; area <= nareas; area++ ) { 
	ret = Vect_get_point_in_area ( &Out, area, &(Centr[area].x), &(Centr[area].y) );
	if ( ret < 0 ) {
	    G_warning ("Cannot calculate area centroid" );
	    Centr[area].valid = 0;
	} else {
	    Centr[area].valid = 1;
	}
    }
    
    /* Downgrade topo so that it is not written by Vect_close() */
    Vect_build_partial ( &Out, GV_BUILD_NONE, NULL );
    Vect_close (&Out);

    /* Query input maps */
    for ( input = 0; input < 2; input++ ) {
        fprintf (stderr, sep );
	fprintf (stderr, "Querying %sinput ... ", pre[input]);

        field[input] = atoi(field_opt[input]->answer);
	
	Vect_set_open_level (2);
	Vect_open_old (&(In[input]), in_opt[input]->answer, mapset[input] );

	for ( area = 1; area <= nareas; area++ ) {
	    Centr[area].cat[input] = 0;
	    
	    in_area = Vect_find_area ( &(In[input]), Centr[area].x, Centr[area].y );
	    if ( in_area > 0 ) {
		in_centr = Vect_get_area_centroid (  &(In[input]), in_area );
		if ( in_centr > 0 ) {
	            Vect_read_line ( &(In[input]), NULL, Cats, in_centr);
		    Vect_cat_get (Cats, field[input], &in_cat);
		    Centr[area].cat[input] = in_cat;
		}
	    }
	    G_percent ( area, nareas, 1 );
	}

	Vect_close ( &(In[input]) ); 
    }

    /* Open output and write centroids */
    Vect_open_update (&Out, out_opt->answer, G_mapset());

    fprintf (stderr, sep );
    fprintf ( stderr, "Writing centroids ..." );
    
    for ( area = 1; area <= nareas; area++ ) {
	/* check the condition */
        switch (operator) {
	    case OP_AND:
		if ( !( Centr[area].cat[0] && Centr[area].cat[1] ) ) continue;
		break;
	    case OP_OR:
		if ( !( Centr[area].cat[0] || Centr[area].cat[1] ) ) continue;
		break;
	    case OP_NOT:
		if ( !( Centr[area].cat[0] && !(Centr[area].cat[1]) ) ) continue;
		break;
	    case OP_XOR:
		if ( (Centr[area].cat[0] && Centr[area].cat[1]) ||
		     ( !(Centr[area].cat[0]) && !(Centr[area].cat[1]) ) ) continue;
		break;
	}
	
	Vect_reset_line ( Points );
        Vect_reset_cats ( Cats );

	Vect_append_point ( Points, Centr[area].x, Centr[area].y, 0 );
	Vect_cat_set (Cats, 1, area);
	
	if ( Centr[area].cat[0] > 0 )
	    Vect_cat_set (Cats, 2, Centr[area].cat[0]);
	
	if ( Centr[area].cat[1] > 0 )
	    Vect_cat_set (Cats, 3, Centr[area].cat[1]);

	Vect_write_line ( &Out, GV_CENTROID, Points, Cats );
	
	G_percent ( area, nareas, 1 );
    }

    /* Build topology and remove boundaries with area without centroid on both sides */
    fprintf (stderr, sep );
    fprintf ( stderr, "Building topology ...\n" );
    Vect_build (&Out, stderr); 

    /* Create a list of lines to be deleted */
    nlines = Vect_get_num_lines ( &Out );
    Del = (char *) G_calloc ( nlines+1,  sizeof(char) ); /* index from 1 ! */

    for ( line = 1; line <= nlines; line++ ) {
	int i, side[2], centr[2];
	G_percent ( line, nlines, 1 ); /* must be before any continue */
	
	type = Vect_read_line ( &Out, NULL, NULL, line);
	if ( !(type & GV_BOUNDARY) ) continue;
	
	Vect_get_line_areas ( &Out, line, &side[0], &side[1] );

	for ( i = 0; i < 2; i++ ) {
	    if ( side[i] == 0 ) { /* This should not happen ! */
		centr[i] = 0;
		continue;
	    }

	    if ( side[i] > 0 ) {
		area = side[i]; 
	    } else { /* island */
		area = Vect_get_isle_area ( &Out, abs ( side[i] ) ); 
	    }

	    if ( area > 0 )
		centr[i] = Vect_get_area_centroid ( &Out, area );
	    else 
	        centr[i] = 0;
	}
	
	if ( !centr[0] && !centr[1] ) Del[line] = 1;
    }

    /* Delete boundaries */
    for ( line = 1; line <= nlines; line++ ) {
	if ( Del[line] )
	    Vect_delete_line ( &Out, line );
    }
    G_free ( Del );

    fprintf (stderr, sep );
    fprintf ( stderr, "Rebuilding topology ...\n" );
    Vect_build_partial ( &Out, GV_BUILD_NONE, NULL );
    Vect_build (&Out, stderr); /* Build topology to show the final result and prepare for Vect_close() */
    
    /* Create dblinks */
    Fi = Vect_default_field_info ( &Out, 1, NULL, GV_1TABLE );
    
    /* Create table */
    if ( !(table_flag->answer) ) {
	fprintf (stderr, sep );
	fprintf ( stderr, "Writing attributes ...\n" );
	
	db_init_string (&stmt);
	driver = db_start_driver_open_database ( Fi->driver, Fi->database );
	if ( driver == NULL ) {
	    Vect_close (&Out);
	    G_fatal_error ( "Cannot open database %s by driver %s", Fi->database, Fi->driver );
	}
	
	sprintf ( buf, "create table %s (cat integer, cata integer, catb integer)", Fi->table );
	db_set_string ( &stmt, buf);
	G_debug ( 2, db_get_string ( &stmt ) );
	
	if (db_execute_immediate (driver, &stmt) != DB_OK ) { 
	    Vect_close (&Out);
	    db_close_database_shutdown_driver ( driver );
	    G_fatal_error ( "Cannot create table: %s", db_get_string (&stmt) );
	}

	/* Table created, now we can write dblink */
	Vect_map_add_dblink ( &Out, 1, NULL, Fi->table, "cat", Fi->database, Fi->driver);
	
	for ( area = 1; area <= nareas; area++ ) {
	    sprintf ( buf, "insert into %s values ( %d", Fi->table, area ); 
	    db_set_string ( &stmt, buf);

	    /* cata */
	    if ( Centr[area].cat[0] > 0 )
		 sprintf ( buf, ", %d", Centr[area].cat[0] );
	    else
		 sprintf ( buf, ", null");
	    
	    db_append_string ( &stmt, buf);

	    /* catb */
	    if ( Centr[area].cat[1] > 0 )
		 sprintf ( buf, ", %d )", Centr[area].cat[1] );
	    else
		 sprintf ( buf, ", null )");
	    
	    db_append_string ( &stmt, buf);

	    G_debug ( 3, db_get_string ( &stmt ) );

	    if (db_execute_immediate (driver, &stmt) != DB_OK )
		G_warning ( "Cannot insert new row: %s", db_get_string ( &stmt ) );
	
	    G_percent ( area, nareas, 1 );
	}	
	
	db_close_database_shutdown_driver ( driver );
    }
    
    Vect_close (&Out);
    
    exit (0);
}


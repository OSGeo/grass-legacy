#include "global.h"
#include "Vect.h"
#include <string.h>

int parse_units();
int parse_option();
int match(); 

int 
parse_command_line (int argc, char *argv[])
{
    int ncols;

    struct {
	    struct Option *vect;
	    struct Option *option;
	    struct Option *type;
	    struct Option *field;
	    struct Option *qfield;
	    struct Option *col;
	    struct Option *units;		
	    struct Option *qcol;		
    } parms;
    struct {
	    struct Flag *p, *s;
    } flags;

    parms.vect = G_define_standard_option(G_OPT_V_INPUT);
    parms.vect->key    = "map";

    parms.type = G_define_standard_option(G_OPT_V_TYPE) ;
    parms.type->options      = "point,line,boundary,centroid";
    parms.type->answer       = "point,line,boundary,centroid";
    parms.type->description  = "Type of elements (for coor valid point/centroid, "
			       "for length valid line/boundary)";	
    
    parms.field = G_define_standard_option(G_OPT_V_FIELD);

    parms.qfield = G_define_standard_option(G_OPT_V_FIELD);
    parms.qfield->key = "qlayer";
    parms.qfield->description = "Query layer. Used by 'query' option.";

    parms.option = G_define_option();
    parms.option->key          = "option";
    parms.option->type         = TYPE_STRING;
    parms.option->required     = YES;
    parms.option->multiple     = NO;
    parms.option->options      = "cat,area,length,count,coor,sides,query";
    parms.option->description  = "Uploaded value:\n"
				 "\tcat - insert new row for each category if doesn't exist yet\n"
	                         "\tarea - area size\n"
				 "\tlength - line length\n"
				 "\tcount - number of features for each category\n"
				 "\tcoor - point coordinates, X,Y or X,Y,Z\n"
				 "\tsides - categories of areas on the left and right side of the boundary,\n"
    					   "\t\t'qlayer' is used for area category.\n"
				 "\tquery - result of a database query for all records of the geometry\n"
				           "\t\t(or geometries) from table specified by 'qlayer' option";	

    parms.units = G_define_option();
    parms.units->key   = "units";
    parms.units->type   = TYPE_STRING ;
    parms.units->required = NO ;
    parms.units->multiple = NO ;
    parms.units->options      = "mi,miles,f,feet,me,meters,k,kilometers,a,acres,h,hectares";
    parms.units->description = "mi(les),f(eet),me(ters),k(ilometers),a(cres),h(ectares)";

    parms.col = G_define_option();
    parms.col->key    = "col";
    parms.col->type   = TYPE_STRING ;
    parms.col->required = NO ;
    parms.col->multiple = YES ;
    parms.col->gisprompt  = "column(s)" ;
    parms.col->description = "column(s)";

    parms.qcol = G_define_option();
    parms.qcol->key    = "qcol";
    parms.qcol->type   = TYPE_STRING ;
    parms.qcol->required = NO ;
    parms.qcol->multiple = NO ;
    parms.qcol->gisprompt  = "query column";
    parms.qcol->description = "Query column used for 'query' option. E.g. 'cat', 'count(*)', 'sum(val)'";

    flags.p = G_define_flag();
    flags.p->key = 'p';
    flags.p->description = "print only";
    
    flags.s = G_define_flag();
    flags.s->key = 's';
    flags.s->description = "print only sql statements";	

    if (G_parser(argc,argv)) exit(-1);

    options.print = flags.p->answer;
    options.sql = flags.s->answer;

    options.name = parms.vect->answer;
    options.mapset = G_find_vector2 (options.name, NULL);

    if (options.mapset == NULL) 
	G_fatal_error ( "%s: <%s> vector map not found\n", G_program_name(), options.name);

    options.type = Vect_option_to_types ( parms.type ); 
    options.field = atoi( parms.field->answer );
    options.qfield = atoi( parms.qfield->answer );

    options.option = parse_option (parms.option->answer);
    options.units = parse_units (parms.units->answer);

    /* Check number of columns */
    ncols = 0;
    options.col[0] = NULL;
    options.col[1] = NULL;
    options.col[2] = NULL;
    while (  parms.col->answers && parms.col->answers[ncols] ) {
	options.col[ncols] = G_store ( parms.col->answers[ncols] );
	ncols++;
    }
    
    if ( options.option == O_AREA || options.option == O_LENGTH || options.option == O_COUNT 
	 || options.option == O_QUERY ) /* one column required */
    {
	if ( ncols != 1 ) {
	    G_fatal_error ( "This option requires one column" );
	}
    }  else if ( options.option == O_SIDES ) {
	if ( ncols != 2 ) {
	    G_fatal_error ( "This option requires 2 columns" );
	}
    }  else if ( options.option == O_COOR ) {
	if ( ncols < 2 ) {
	    G_fatal_error ( "This option requires at least 2 columns" );
	}
    }

    options.qcol = parms.qcol->answer;

    if ( options.option == O_SIDES && !(options.type | GV_BOUNDARY) )
	G_fatal_error ( "The 'sides' option makes sense only for boundaries.");

    return 0;
}

int parse_units (char *s)
{
    int x=0;

    if (match (s, "miles",2)) x = U_MILES;
    else if (match (s, "feet",1)) x = U_FEET;
    else if (match (s, "meters",2)) x = U_METERS;
    else if (match (s, "kilometers",1)) x = U_KILOMETERS;
    else if (match (s, "acres",1)) x = U_ACRES;
    else if (match (s, "hectares",1)) x = U_HECTARES;

    return x;
}

int parse_option (char *s)
{
    int x=0;

    if (strcmp (s, "cat") == 0) x = O_CAT;
    else if (strcmp (s, "area") == 0) x = O_AREA;
    else if (strcmp (s, "length") == 0) x = O_LENGTH;
    else if (strcmp (s, "count") == 0) x = O_COUNT;
    else if (strcmp (s, "coor") == 0) x = O_COOR;
    else if (strcmp (s, "sides") == 0) x = O_SIDES;
    else if (strcmp (s, "query") == 0) x = O_QUERY;

    return x;
}

int 
match (char *s, char *key, int min)
{
    int len;

    if (!s) return 0;
    len = strlen (s);
    if (len < min) return 0;
    return strncmp (s, key, len) == 0;
}

#include "global.h"
#include "Vect.h"
#include <string.h>

int parse_units();
int parse_option();
int match(); 

int 
parse_command_line (int argc, char *argv[])
{
    struct {
	    struct Option *vect;
	    struct Option *option;
	    struct Option *type;
	    struct Option *field;
	    struct Option *col1;
	    struct Option *col2;
	    struct Option *units;		
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

    parms.option = G_define_option();
    parms.option->key          = "option";
    parms.option->type         = TYPE_STRING;
    parms.option->required     = YES;
    parms.option->multiple     = NO;
    parms.option->options      = "cat,area,length,count,coor";
    parms.option->description  = "what to load";		

    parms.units = G_define_option();
    parms.units->key   = "units";
    parms.units->type   = TYPE_STRING ;
    parms.units->required = NO ;
    parms.units->multiple = NO ;
    parms.units->options      = "mi,miles,f,feet,me,meters,k,kilometers,a,acres,h,hectacres";
    parms.units->description = "mi(les),f(eet),me(ters),k(ilometers),a(cres),h(ectacres)";

    parms.col1 = G_define_option();
    parms.col1->key    = "col1";
    parms.col1->type   = TYPE_STRING ;
    parms.col1->required = YES ;
    parms.col1->multiple = NO ;
    parms.col1->gisprompt  = "column 1" ;
    parms.col1->description = "column 1";

    parms.col2 = G_define_option();
    parms.col2->key    = "col2";
    parms.col2->type   = TYPE_STRING ;
    parms.col2->required = NO ;
    parms.col2->multiple = NO ;
    parms.col2->gisprompt  = "column 2" ;
    parms.col2->description = "column 2";

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

    options.option = parse_option (parms.option->answer);
    options.units = parse_units (parms.units->answer);
    options.col1 = parms.col1->answer;
    options.col2 = parms.col2->answer;

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
    else if (match (s, "hectacres",1)) x = U_HECTARES;

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

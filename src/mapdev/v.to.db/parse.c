#include "global.h"
#include "Vect.h"
#include <string.h>

int parse_units();
int parse_option();
int match(); 

int 
parse_command_line (int argc, char *argv[])
{
	int i;
	char msg[100];
	struct
	    {
		struct Option *vect;
		struct Option *type;
		struct Option *option;
		struct Option *table;   
		struct Option *key;
		struct Option *col1;
		struct Option *col2;
		struct Option *units;		
	} parms;
	struct
	    {
		struct Flag *p, *s;
	} flags;

	parms.vect = G_define_option();
	parms.vect->key    = "map";
	parms.vect->type   = TYPE_STRING ;
	parms.vect->required = YES ;
	parms.vect->multiple = NO ;
	parms.vect->gisprompt  = "vector" ;
	parms.vect->description = "vector map";

        parms.type = G_define_option();
        parms.type->key          = "type";
        parms.type->type         = TYPE_STRING;
        parms.type->required     = YES;
        parms.type->multiple     = YES;
        parms.type->options      = "area,line,site";
        parms.type->description  = "type of elements";	
	
        parms.option = G_define_option();
        parms.option->key          = "option";
        parms.option->type         = TYPE_STRING;
        parms.option->required     = YES;
        parms.option->multiple     = YES;
        parms.option->options      = "cat,label,area,length,count,coor";
        parms.option->description  = "what load";		

	parms.units = G_define_option();
	parms.units->key   = "units";
	parms.units->type   = TYPE_STRING ;
	parms.units->required = NO ;
	parms.units->multiple = NO ;
	parms.units->options      = "mi,miles,f,feet,me,meters,k,kilometers,a,acres,h,hectacres";
	parms.units->description = "mi(les),f(eet),me(ters),k(ilometers),a(cres),h(ectacres)";

	parms.table = G_define_option();
	parms.table->key    = "table";
	parms.table->type   = TYPE_STRING ;
	parms.table->required = YES ;
	parms.table->multiple = NO ;
	parms.table->gisprompt  = "table" ;
	parms.table->description = "DB table";

	parms.key = G_define_option();
	parms.key->key    = "key";
	parms.key->type   = TYPE_STRING ;
	parms.key->required = NO ;
	parms.key->multiple = NO ;
	parms.key->gisprompt  = "key column" ;
	parms.key->description = "key column";

	parms.col1 = G_define_option();
	parms.col1->key    = "col1";
	parms.col1->type   = TYPE_STRING ;
	parms.col1->required = NO ;
	parms.col1->multiple = NO ;
	parms.col1->gisprompt  = "column 1" ;
	parms.col1->description = "column 2";

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

	if (G_parser(argc,argv))
		exit(-1);

	options.print = flags.p->answer;
	options.sql = flags.s->answer;

	options.name = parms.vect->answer;
	options.mapset = G_find_vector (options.name, "");

	if (options.mapset == NULL)
	{
		sprintf (msg, "%s: <%s> vector map not found\n", G_program_name(), options.name);
		G_fatal_error (msg);
		exit(1);
	}

	i=0;
	options.type=0; 
	while (parms.type->answers[i])
        {
	    if ( parms.type->answers[i][0] == 'l')  options.type |= LINE;
	    else if ( parms.type->answers[i][0] == 'a')  options.type |= AREA;
	    else if ( parms.type->answers[i][0] == 's')  options.type |= DOT;
	    i++;
	} 

	options.option = parse_option (parms.option->answer);
	options.units = parse_units (parms.units->answer);
	options.table = parms.table->answer;
	options.key = parms.key->answer;
	options.col1 = parms.col1->answer;
	options.col2 = parms.col2->answer;

        switch (options.option)	{
	    case O_CAT:
	    case O_COUNT:	    
		options.list = LIST_CI; 
		break; 
	    case O_LABEL:
		options.list = LIST_CC; 
		break;
	    case O_AREA:
	    case O_LENGTH:
		options.list = LIST_CD; 
		break;
	    case O_COOR:
		options.list = LIST_CI2D; 
		break;		
	}
	
	return 0;
}

int parse_units (char *s)
{
	int x=0;

	if (match (s, "miles",2))
		x = U_MILES;
	else if (match (s, "feet",1))
		x = U_FEET;
	else if (match (s, "meters",2))
		x = U_METERS;
	else if (match (s, "kilometers",1))
		x = U_KILOMETERS;
	else if (match (s, "acres",1))
		x = U_ACRES;
	else if (match (s, "hectacres",1))
		x = U_HECTARES;

	return x;
}

int parse_option (char *s)
{
	int x=0;

	if (strcmp (s, "cat") == 0)
		x = O_CAT;
	if (strcmp (s, "label") == 0)
		x = O_LABEL;		
	if (strcmp (s, "area") == 0)
		x = O_AREA;
	if (strcmp (s, "length") == 0)
		x = O_LENGTH;
	if (strcmp (s, "count") == 0)
		x = O_COUNT;
	if (strcmp (s, "coor") == 0)
		x = O_COOR;

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

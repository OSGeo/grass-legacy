#include <string.h>
#include "tools.h"
#include "gis.h"

extern int G_gisinit(char*);
extern int G_parser(int , char**);

Filenames init_and_parse(int argc, char *argv[] )
{
	Filenames returnvalue;
	struct Option *opt1, *opt2, *opt3, *opt4;

	/* Specify Options for command-line parsing */
	opt1 = G_define_option() ;  
	opt1->key        = "input" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->multiple   = NO;
	opt1->description= "Input raster map" ;
	opt1->gisprompt  = "old,cell,raster" ;

	opt2 = G_define_option() ; 
	opt2->key        = "output" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = YES ;
	opt2->multiple   = NO;
	opt2->description= "Output raster map" ;
	opt2->gisprompt  = "new,cell,raster" ; 

	opt3 = G_define_option() ; 
	opt3->key        = "neibor" ;
	opt3->type       = TYPE_STRING ;
	opt3->required   = YES ;
	opt3->multiple   = NO;
	opt3->description= "Name of neighbor information file" ;

	opt4 = G_define_option() ;  
	opt4->key        = "aggregation" ;
	opt4->type       = TYPE_INTEGER ;
	opt4->answer	 = "8" ;
	opt4->required   = NO ;
	opt4->multiple   = NO;
	opt4->description= "4 or 8 neighborhood aggregation method" ;
	opt4->options	 = "4,8";

	G_gisinit (argv[0]);
	
	/* In case of invalid parameters print a message and exit */
	if (0 > G_parser(argc,argv)) {
		exit(-1);
		};


	returnvalue.input = strdup(opt1->answer);
	returnvalue.output= strdup( opt2->answer);
	returnvalue.neibordat = strdup ( opt3->answer );
	if (!(returnvalue.neighborhood = atoi(opt4->answer))) {
	  printf("Programmierfehler in init_and_parse");
	  exit(-1);
	};

	return (returnvalue);
}

#include "gis.h"
#include <stdio.h>

main(argc,argv)
char *argv[];
int  argc;
{
	int verbose;
	int compact;
	int range;
	int windowed;
	char name[200];
	char *mapset;
	char msg[100];
	struct Flag *flag1;
	struct Flag *flag2;
	struct Flag *flag3;
	struct Flag *flag4;
	struct Option *opt1;

	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->multiple   = NO;
	opt1->description= "Name of raster map" ;
	opt1->gisprompt  = "old,cell,raster" ;

	/*define the different flags */

	flag1 =G_define_flag() ;
	flag1->key         = '1';
	flag1->description = "Print the output one value per line";

	flag2 =G_define_flag() ;
	flag2->key         = 'r';
	flag2->description = "Only print the range of the data";

	flag3 =G_define_flag() ;
	flag3->key        = 'q';
	flag3->description = "Quiet";

	flag4 =G_define_flag() ;
	flag4->key        = 'd';
	flag4->description = "Use the current region";


	G_gisinit (argv[0]);

	verbose = 1;

	if (0 > G_parser(argc,argv))
		exit(-1);

	verbose = (! flag3->answer);
	compact = (! flag1->answer);
	range =  flag2->answer;
	windowed =  flag4->answer;

	strcpy (name, opt1->answer);
	if (mapset = G_find_cell2 (name, ""))
	{
		describe (name, mapset, compact, verbose, range, windowed);
		exit(0);
	}
	sprintf (msg,"%s: [%s] not found", G_program_name(), name);
	G_fatal_error (msg);
	exit(1);
}

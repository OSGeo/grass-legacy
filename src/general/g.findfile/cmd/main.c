#include "gis.h"
#include <stdio.h>

main(argc,argv)
int   argc;
char *argv[];
{
	char file[1024], name[200], *mapset;
	char *search_mapset;
	struct Option *opt1 ;
	struct Option *opt2 ;
	struct Option *opt3 ;

	/* Define the different options */

	opt1 = G_define_option() ;
	opt1->key        = "element";
	opt1->type       = TYPE_STRING;
	opt1->required   = YES;
	opt1->description= "Name of an element" ;

	opt2 = G_define_option() ;
	opt2->key        = "mapset";
	opt2->type       = TYPE_STRING;
	opt2->required   = NO;
	opt2->description= "Name of a mapset" ;
	opt2->answer     = "";

	opt3 = G_define_option() ;
	opt3->key        = "file";
	opt3->type       = TYPE_STRING;
	opt3->required   = YES;
	opt3->description= "Name of an existing map" ;

	G_gisinit (argv[0]);

	if (G_parser(argc, argv))
		exit(-1);

	search_mapset = opt2->answer ;
	if(strcmp (".", search_mapset) == 0)
	    search_mapset = G_mapset();

	strcpy (name, opt3->answer);

	mapset = G_find_file2 (opt1->answer, name, search_mapset);
	if (mapset)
	{
		printf ("name='%s'\n",name);
		printf ("mapset='%s'\n",mapset);
		printf ("fullname='%s'\n",G_fully_qualified_name(name,mapset));
		G__file_name (file, opt1->answer, name, mapset);
		printf ("file='%s'\n",file);
	}
	else
	{
		printf ("name=\n");
		printf ("mapset=\n");
		printf ("fullname=\n");
		printf ("file=\n");
	}
	exit(mapset==NULL);
}

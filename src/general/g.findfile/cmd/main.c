#include <string.h>
#include "gis.h"
#include <stdio.h>

int main( int   argc, char *argv[])
{
	char file[1024], name[200], *mapset;
	char *search_mapset;
	struct GModule *module;
	struct Option *opt1 ;
	struct Option *opt2 ;
	struct Option *opt3 ;

	module = G_define_module();
	module->description =
		"Searches for GRASS data base files "
		"and sets variables for the shell.";

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
		fprintf (stdout,"name='%s'\n",name);
		fprintf (stdout,"mapset='%s'\n",mapset);
		fprintf (stdout,"fullname='%s'\n",G_fully_qualified_name(name,mapset));
		G__file_name (file, opt1->answer, name, mapset);
		fprintf (stdout,"file='%s'\n",file);
	}
	else
	{
		fprintf (stdout,"name=\n");
		fprintf (stdout,"mapset=\n");
		fprintf (stdout,"fullname=\n");
		fprintf (stdout,"file=\n");
	}
	exit(mapset==NULL);
}

#include <stdio.h>
#include <stdlib.h>
#define GLOBAL
#include "local_proto.h"

int
main(int argc, char **argv)
{
	char *str, tmp[128];
	int i, j;
	int ml;
	int width, mwidth;
	double east, north;
	struct Option *opt1, *opt2;
	struct Flag *flag1;
	struct GModule *module;

	G_gisinit (argv[0]);

	opt1 = G_define_option();
	opt1->key        = "map";
	opt1->type       = TYPE_STRING;
	opt1->multiple   = YES;
	opt1->required   = YES;
	opt1->gisprompt  = "old,site_lists,sites";
	opt1->description= "Name of existing sites file"; 

	opt2 = G_define_option();
	opt2->key        = "east_north";
	opt2->type       = TYPE_DOUBLE;
	opt2->key_desc   = "east,north";
	opt2->multiple   = YES;
	opt2->required   = NO;
	opt2->description= "Coordinates for query";

	flag1 = G_define_flag ();
	flag1->key = 'q';
	flag1->description = "Load quietly";
	
	module = G_define_module();
	module->description = 
	  "Allows the user to query site list descriptions. ";

	if((argc > 1 || !site) && G_parser(argc, argv))
	    exit(-1);

	site = opt1->answers;

	for(i=0; site[i]; i++);
	nsites = i;

	ml = strlen(G_mapset());
	width = mwidth = 0;
	for(i=0; i<nsites; i++)
	{
	    str = strchr(site[i], '@');
	    if(str) j = str - site[i];
	    else	j = strlen(site[i]);
	    if(j > width)
	    	width = j;

	    if(str) j = strlen(str+1);
	    else	j = ml;
	    if(j > mwidth)
	    	mwidth = j;

	    if(G_find_sites2(site[i], (str ? str+1 : "")) == NULL)
	    {
	        char msg[256];
	        sprintf(msg, "Site file [%s] not available", site[i]);
	        G_fatal_error(msg);
	    }
	}
	Snum = (int *) G_malloc(sizeof(int)*nsites);
	CurSites = (Site ***) G_malloc(sizeof(Site **)*nsites);

	for(i=0; i<nsites; i++){
	    load_sites(i, !(flag1->answer));
	}

	if(opt2->answers)
	{
	    for(i=0; opt2->answers[i]; i+=2)
		what(atof(opt2->answers[i]), atof(opt2->answers[i+1]), width, mwidth);
	}
	else
	{
	    fprintf(stderr, "Ctrl+D or \"end\" to quit\n");
	    for(;;)
	    {
      	        fprintf(stderr, "\neast north >  ");
		tmp[0] = 0;
		fgets(tmp, 128, stdin);
		if(strlen(tmp) == 0 || !strcmp(tmp, "end\n"))
		    break;
                i = sscanf(tmp, "%lf %lf", &east, &north);
		if(i == 2)
		    what(east, north, width, mwidth);
	    }
	}

	free_cached_sites();

	exit(0);
}

#define GLOBAL
#include "what.h"
#include "local_proto.h"

int main(int argc, char **argv)
{
	struct Cell_head window ;
	char temp[128] ;
	int i, t, b, l, r ;
	struct Option *opt1;
	struct Flag *shh, *once, *terse;
	struct GModule *module;

	/* Initialize the GIS calls */
	G_gisinit (argv[0]) ;
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	if(D_get_site_list (&site, &nsites) < 0)
		site = NULL;
	else
	{
		site = (char **)G_realloc(site, (nsites+1)*sizeof(char *));
		site[nsites] = NULL;
	}

	R_close_driver();

	opt1 = G_define_option() ;
	opt1->key        = "map" ;
	opt1->type       = TYPE_STRING ;
	opt1->multiple   = YES ;
	if (site)
		opt1->answers = site;
	opt1->required   = NO ;
	opt1->gisprompt  = "old,site_lists,sites" ;
	opt1->description= "Name of existing sites file"; 

	once = G_define_flag();
	once->key = '1';
	once->description ="Identify just one site";
	
	terse = G_define_flag();
	terse->key = 't';
	terse->description = "Terse output. For parsing by programs.";
	
	shh = G_define_flag ();
	shh->key = 'q';
	shh->description = "Load quietly";
	
	module = G_define_module();
	module->description = 
	  "Allows the user to interactively query site list descriptions. ";

	if(!site)
		opt1->required = YES;

	if ((argc > 1 || !site) && G_parser(argc, argv))
	    exit(-1);

	if(opt1->answers && opt1->answers[0])
	    site = opt1->answers;

	if(site)
	{
	    for(i=0; site[i]; i++);
	    nsites = i;

	    for(i=0; i<nsites; i++)
	    {
	        if(G_find_sites(site[i], "") == NULL)
		{
		    char msg[256];
		    sprintf(msg, "Site file [%s] not available", site[i]);
		    G_fatal_error(msg);
		}
	    }
	}

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	if (D_get_cur_wind(temp))
		G_fatal_error("No current graphics window") ;

	if (D_set_cur_wind(temp))
		G_fatal_error("Current graphics window not available") ;

	/* Read in the map window associated with window */
	G_get_window(&window) ;

	if (D_check_map_window(&window))
		G_fatal_error("Setting graphics window") ;

	if (G_set_window(&window) == -1)
		G_fatal_error("Can't set current graphics window") ;

	/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting graphics window coordinates") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;
	
	if(open_sites(opt1->answers[0])){
	    load_sites(&window, !(shh->answer));
	    what (once->answer, terse->answer) ;
	}

	free_cached_sites();

	R_close_driver();
	exit(0);
}

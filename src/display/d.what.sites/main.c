#define GLOBAL
#include "what.h"
#include "local_proto.h"

int main(int argc, char **argv)
{
	struct Cell_head window ;
	char temp[128] ;
	int t, b, l, r ;
	struct Option *opt1;
	struct Flag *shh;


	/* Initialize the GIS calls */
	G_gisinit (argv[0]) ;
	R_open_driver();

	if(D_get_site_list (&site, &nsites) < 0)
		site = NULL;
	else
	{
		site = (char **)G_realloc(site, (nsites+1)*sizeof(char *));
		site[nsites] = NULL;
	}

	R_close_driver();

	opt1 = G_define_option() ;
	opt1->key        = "sites" ;
	opt1->type       = TYPE_STRING ;
	opt1->multiple   = YES ;
	if (site)
		opt1->answers = site;
	opt1->required   = NO ;
	opt1->gisprompt  = "old,site_lists,Sites" ;
	opt1->description= "Name of existing sites file"; 

	shh = G_define_flag ();
	shh->key = 'q';
	shh->description = "Load quietly";
	
	if(!!site)
		opt1->required = YES;

	if (G_parser(argc, argv))
	    exit(-1);

	R_open_driver();

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
	
	if(open_sites(opt1->answer)){
	    load_sites(&window, !(shh->answer));
	    what (0, 0) ;
	}

	free_cached_sites();

	R_close_driver();
	exit(0);
}

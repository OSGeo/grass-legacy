/* Written by Bill Brown, UIUC GIS Laboratory
 */

#include "gis.h"
#include "local_proto.h"

struct Cell_head    Wind;

int 
main (int argc, char *argv[])
{

    struct Option 	*in;
    struct Flag 	*shh, *inreg, *quar;
    char 		errbuf[100];
    char                *sitesmap;
    struct GModule      *module;

    G_gisinit (argv[0]);
   
    module = G_define_module();
    module->description =      
                    "reports attribute, label, and other information about a "
                    "sites file";

    in = G_define_option();
    in->key                    = "sites";
    in->type                   = TYPE_STRING;
    in->required               = YES;
    in->gisprompt    	       = "old,site_lists,sites";
    in->description            = "Name of sites file.";

    shh = G_define_flag ();
    shh->key = 'q';
    shh->description = "Load quietly";

    inreg = G_define_flag ();
    inreg->key = 'w';
    inreg->description = "Only count sites in current window for min/max.";

    quar = G_define_flag ();
    quar->key = 'Q';
    quar->description = "Show quartiles for decimal attributes";

    if (G_parser (argc, argv))
	exit (-1);

    sitesmap = G_find_file2 ("site_lists", in->answer, "");
    if(!sitesmap){
	sprintf(errbuf,"Couldn't find sites file %s", in->answer);
	G_fatal_error(errbuf);
    }

    G_get_set_window (&Wind);
    
    set_quiet(shh->answer);
    set_quartiles(quar->answer);
    do_new_sites(in->answer,sitesmap,inreg->answer);
    sites_describe();
    if(inreg->answer)
	fprintf(stderr,
	   "NOTE: min/max values are only for sites in current region!\n");

    return(0);

}




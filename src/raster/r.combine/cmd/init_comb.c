/* init_comb handles all start up tasks associated with combine */

#include "tree.h"
#include "display.h"
#include "D.h"
#include "raster.h"
#include "local_proto.h"

static int use_graphics = 1 ;

int 
init_comb (int argc, char *argv[])
{
    struct Flag *s;
	struct GModule *module;

	module = G_define_module();
	module->description =
		"Allows category values from several raster "
		"map layers to be combined.";

    s = G_define_flag();
    s->key = 's';
    s->description = "Use symbols (instead of graphics)";
    s->answer = 0;

    if (G_parser(argc,argv)) exit(1);

    use_graphics = !s->answer;

    if(at_console())
    {
	R_open_driver() ;

	D_setup(0);

	D_set_cell_name("combine result") ;
    }

    /* initialize the mapset window */
    if(init_proj_win() == 0)
	yyerror("Can't initialize given mapset") ;

    if(at_console())
    {
	fprintf (stdout,"\nAnalyses will be displayed in color during execution\n");
    }
    else
    {
	fprintf (stdout,"\nAnalyses will be displayed with symbols during execution\n");
    }

    /* Letem know help is available */
    fprintf (stdout,"\nHelp is available for %s: enter (help)\n", G_program_name()) ;

    return 0;
}

int at_console (void)
{
    return(use_graphics) ;
}
